;;; corfu-candidate-overlay.el --- Show first candidate in an overlay while typing -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

;; Author: Adam Kruszewski <adam@kruszewski.name>
;; Maintainer: Adam Kruszewski <adam@kruszewski.name>
;; Created: 2023
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (corfu "0.36"))
;; Homepage: https://code.bsdgeek.org/adam/corfu-candidate-overlay/

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Show first candidate in an inline overlay while typing.
;; When there is only one candidate the overlay text will
;; be underlined.
;;
;; It is ment to be used with `corfu-auto' set to `nil', and
;; executing the corfu completion popup with a keybind.
;;
;; Enable by executing `corfu-candidate-overlay-mode'.

;;; Code:

(require 'corfu)

(defvar-local corfu--candidate-overlay nil
  "Overlay for Corfu candidates display when typing.")

(defvar-local corfu--candidate-last-point nil
  "Last point location when the overlay was calculated for.")

(defvar corfu--candidate-overlay-map nil
  "Keymap to dismiss the Corfu candidate overlay.")

(defcustom corfu-overlay-auto-commands
  '("delete-backward-char\\'" "backward-delete-char-untabify")
  "Additional commands apart from ``corfu-auto-commands'' which initiate
   completion candidate overlay."
  :type '(repeat (choice regexp symbol))
  :group 'corfu)

(defface corfu-candidate-overlay-face
  '((((background light))
     :foreground "MistyRose4")
    (((background dark))
     :foreground "wheat"))
  "Face used for the overlay with the first candidate.")

(defface corfu-candidate-overlay-face-exact-match
  '((t (:inherit 'corfu-candidate-overlay-face :underline t)))
  "Face used for the overlay when there is only one candidate.")

(defun corfu--candiate-overlay-prepare (position)
  "Sets the default properties of the candidates overlay.
       The overlay can be dismissed with a mouse click."
  (when (not corfu--candidate-overlay-map)
    (setq corfu--candidate-overlay-map (make-sparse-keymap))
    (define-key corfu--candidate-overlay-map (kbd "<mouse-1>")
                (lambda ()
                  (interactive)
                  (delete-overlay corfu--candidate-overlay))))

  (if corfu--candidate-overlay
      (move-overlay corfu--candidate-overlay position position)
    (progn
      (setq corfu--candidate-overlay (make-overlay position position nil))
      ;; priority of 1k is the value used by Corfu.
      (overlay-put corfu--candidate-overlay  'priority 1000))))

(defun corfu--get-overlay-property (property)
  "Returns the value of overlays' property"
  (overlay-get corfu--candidate-overlay property))

(defun corfu--set-overlay-property (property value)
  "Returns the value of overlays' property"
  (overlay-put corfu--candidate-overlay property value))

(defun corfu--candidate-overlay-update (position prefix candidate how-many-candidates)
  "Updates the candidate overlay with the first candidate found by Corfu."
  (corfu--candiate-overlay-prepare position)

  (unless (string-empty-p candidate)
    (add-text-properties 0 1 '(cursor 1) candidate))

  (overlay-put corfu--candidate-overlay  'window (selected-window))
  ;; we store the whole candidate and prefix as a property to use when
  ;; deleting characters in quick succession so the backend will not
  ;; keep-up. We will need to use those stored values then to still
  ;; show the overlay with a meaningful suggestion
  ;; (i.e. the last one found)
  (corfu--set-overlay-property  'corfu-candidate candidate)
  (corfu--set-overlay-property  'corfu-prefix    prefix)
  (corfu--set-overlay-property  'corfu-count     how-many-candidates)
  ;; and here is the candidate string as it will be rendered by Emacs.
  (corfu--set-overlay-property  'after-string
               (propertize
                candidate
                'face (if (= how-many-candidates 1)
                          'corfu-candidate-overlay-face-exact-match
                        'corfu-candidate-overlay-face)
                'keymap corfu--candidate-overlay-map)))

(defun corfu-hide-candidate-overlay ()
  "Hide the candidate overlay."
  (when (and
         corfu--candidate-overlay
         (overlayp corfu--candidate-overlay))
    ;; 'invisible property doesn't work really; deleting the overlay
    ;; would need to recreate the object on basically each keystroke
    ;; and I don't like the perspective of it, would also flicker
    ;; for sure - so we keep the one overlay and we clear the contents.
    (corfu--set-overlay-property 'after-string "")))

(defun corfu-show-candidate-overlay ()
  "Computes completion candidates just like Corfu and updats the candidate
       overlay to reflect the first one. Uses different face when there is only
       one candidate available (defaults to underline)."
  (let ((value (while-no-input ;; Interruptible capf query
                 (run-hook-wrapped 'completion-at-point-functions #'corfu--capf-wrapper))))

    (if (not value)
        (corfu-hide-candidate-overlay) ;; when empty, we hide the overlay.
      (pcase value                     ;; when not, we check the completion data.
        (`(,fun ,beg ,end ,table . ,plist)
         (let ((completion-in-region-mode-predicate
                (lambda ()
                  (when-let (newbeg (car-safe (funcall fun)))
                    (= newbeg beg))))
               (completion-extra-properties plist))

           (setq completion-in-region--data
                 (list (if (markerp beg) beg (copy-marker beg))
                       (copy-marker end t)
                       table
                       (plist-get plist :predicate)))

           (when (>= (- end beg) corfu-auto-prefix) ;; adhere to auto prefix length settings.
             (corfu--update)
             ;; unfortunately corfu--candidates CAN and ARE nil sometimes,
             ;; if so we just short-circuit and hide the overlay.
             ;; TODO: Refactor the below to have just one ``if'' condition. Low priority.
            (if corfu--candidates
               (let* ((candidate           (car corfu--candidates))
                      (how-many-candidates (length corfu--candidates))
                      (len                 (- end beg))
                      (prefix              (buffer-substring-no-properties beg end))
                      (suffix              (substring candidate len)))
                 (if (and
                      ;; need candidate, it should be present if we got here, but safety-first
                      candidate
                      ;; the prefix can't be empty (in case of corfu-auto-prefix equal 0)
                      (not (string-empty-p prefix))
                      ;; prefix need to match the candidate as there are „fuzzy”
                      ;; found candidates, esp. when using templates so the user
                      ;; could see strage results when typing the first character.
                      (string-prefix-p prefix candidate))
                     ;; and finally we update the overlay.
                     (corfu--candidate-overlay-update
                      end    ;; we anchor the overlay to the end position as cursor is there.
                      prefix
                      suffix
                      how-many-candidates)
                   ;; hide if the above ``if-condition'' is not met.
                   (corfu-hide-candidate-overlay)))
              ;; hide if there is no corfu--candidates at all (equals nil).
              (corfu-hide-candidate-overlay)))))))))

(defun corfu--candidate-overlay-pre-command ()
  "Pre command hook to hide the overlay if the command is not insert or delete.
   Otherwise the overlay can influence movement commands (i.e. the cursor is
   considered to be located at the end of the overlay, so line movement will
   jump to character far removed from the perceived cursor location)."
  ;; We should not throw an error here, as Emacs will disable
  ;; the hook if it fails with an error.
  (ignore-errors
    ;; This condition look similar to one in the post-command hook but it does
    ;; differ significantly -- this one is in part reversed and less strict.
      (let* ((is-insert-command
              (corfu--match-symbol-p corfu-auto-commands this-command))
             (is-delete-command
              (corfu--match-symbol-p corfu-overlay-auto-commands this-command)))
        (when (and
               ;; we are not in minibuffer.
               (not (minibuffer-window-active-p (selected-window)))
               ;; corfu menu shown
               (and (frame-live-p corfu--frame) (frame-visible-p corfu--frame))
               (not corfu-auto) ;; don't work with corfu-auto
               ;; and the command is not one of insert or delete.
               (not (or
                     is-insert-command
                     is-delete-command)))
          (corfu-hide-candidate-overlay)))))

(defun corfu--candidate-overlay-post-command ()
  "Post command hook updating the candidate overlay when user inserts character
   and the cursor is at the end of word."
  ;; We should not throw an error here, as Emacs will disable
  ;; the hook if it fails with an error (and auto suggestion backends
  ;; can and do throw errors sometimes, corfu even have a readme section
  ;; dedicated to debugging those; but a timer corfu menu is using is much
  ;; more forgiving than how Emacs handle post and pre command hooks).
  (ignore-errors
      (let* ((is-insert-command
              (corfu--match-symbol-p corfu-auto-commands this-command))
             (is-delete-command
              (corfu--match-symbol-p corfu-overlay-auto-commands this-command)))
        ;; short-circuit conditions -- the earlier we return if don't need to do
        ;; anything the better.
        (if (and
             ;; we are not in minibuffer, as it looks awkward.
             (not (minibuffer-window-active-p (selected-window)))
             (not corfu-auto) ;; don't work with corfu-auto
             ;; corfu menu needs to be hidden
             (not (and (frame-live-p corfu--frame) (frame-visible-p corfu--frame)))
             (not (and ;; do not update if the point have not moved.
                   corfu--candidate-last-point
                   (= corfu--candidate-last-point (point))))
             (or  ;; do not update if it is not one of the insert or delete commands.
              is-insert-command
              is-delete-command))
            ;; now we check additional short-circuit conditions, but those operate on
            ;; next character.
            (let ((next-char (char-after)))
              (when (or  ;; do not update if we are not at the end of the word.
                     (not next-char) ;; end of file
                     ;; one of whitespace, quoting character, punctuation,
                     ;; closing bracket, etc is next.
                     ;; When those characters follow next completion won't trigger
                     ;; either-way: ' = * - + / ~ _ (have not investigated further)
                     (memq next-char '(?\s ?\t ?\r ?\n
                                           ?\" ?\` ?\) ?\] ?\>
                                           ?\. ?\, ?\: ?\;)))
                ;; When the completion backend is SLOW, i.e. like every LSP client,
                ;; then the overlay will often not update and will interfere with the typing.
                ;; That's why we operate on stored prefix and candidate giving an illusion
                ;; of updating the overlay -- but using the previous auto suggestion candidate.
                (when corfu--candidate-overlay ;; need overlay active
                  (let* ((candidate
                          (corfu--get-overlay-property  'corfu-candidate))
                         (prefix
                          (corfu--get-overlay-property  'corfu-prefix))
                         (count
                          (corfu--get-overlay-property  'corfu-count))
                         (previous-text
                          (corfu--get-overlay-property 'after-string)))
                    ;; We need to deal with the overlay and stored candidate differently
                    ;; when inserting and deleting (i.e. we need to shift one characte from or to
                    ;; prefix to/from candidate)
                    (cond
                     ;; TODO: Delete character case should probably be moved to pre-command hook?
                     (is-delete-command
                      (if (length> prefix 0)
                          ;; we still have some characters present in the prefix,
                          ;; so we'll borrow one and move to the candidate.
                          (corfu--candidate-overlay-update
                           (point) ;; move to current cursor's position
                           (substring prefix 0 (- (length prefix) 1 ))
                           (concat (substring prefix (- (length prefix) 1)) candidate)
                           count)
                        ;; if the length of prefix is zero then we can only hide
                        ;; the overlay as we are removing past the current word
                        ;; boundary.
                        (corfu-hide-candidate-overlay)))
                     ;; Inserting character - we still update using historical data
                     ;; in case the corfu backend would get interrupted;
                     ;; Here we "borrow" a character from the candidate and append it to the prefix.
                     (is-insert-command
                      (if (and
                           (not (string-empty-p previous-text))
                           (length> candidate 1))
                          (corfu--candidate-overlay-update
                           (point) ;; move to current cursor's position
                           (concat prefix (substring candidate 0 1))
                           (substring candidate 1 (length candidate))
                           count)
                        ;; no previous candidate or candidate is zero length,
                        ;; probably we have reached the end of suggested word,
                        ;; so let's hide the overlay.
                        (corfu-hide-candidate-overlay))))))
                ;; preserve the current position, show and update the overlay.
                ;; the corfu-show-candidate-overlay CAN be interrupted, that's why
                ;; we did the shuffling above.
                (setq corfu--candidate-last-point (point))
                (corfu-show-candidate-overlay)))
          ;; or hide the overlay if the conditions to show the overlay where not met.
          (corfu-hide-candidate-overlay)))))

;;;###autoload
(define-minor-mode corfu-candidate-overlay-mode
  "Show first candidate in an overlay while typing."
  :global t
  :group 'corfu
  (if corfu-candidate-overlay-mode
      (cond
       ((not corfu-auto)
        (add-hook 'post-command-hook #'corfu--candidate-overlay-post-command)
        (add-hook 'pre-command-hook  #'corfu--candidate-overlay-pre-command)
        (message "Enabled `corfu-candidate-overlay-mode'."))
       (t
        (message "`corfu-auto' enabled, `corfu-candidate-overlay-mode' requires `corfu-auto' to be set to `nil'.")))
    (progn
      (remove-hook 'post-command-hook #'corfu--candidate-overlay-post-command)
      (remove-hook 'pre-command-hook  #'corfu--candidate-overlay-pre-command)
      (message "Disabled `corfu-candidate-overlay-mode'."))))

;;; corfu-candidate-overlay.el ends here
(provide 'corfu-candidate-overlay)
