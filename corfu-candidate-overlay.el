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
  '("delete-backward-char\\'")
  "Additional commands apart from corfu-auto-commands which initiate
   completion candidate overlay."
  :type '(repeat (choice regexp symbol))
  :group 'corfu)

(defface corfu--candidate-overlay-face
  '((((background light))
     :foreground "MistyRose4")
    (((background dark))
     :foreground "wheat"))
  "Face used for the overlay with the first candidate.")

(defface corfu--candidate-overlay-face-exact-match
  '((t (:inherit 'corfu--candidate-overlay-face :underline t)))
  "Face used for the overlay when there is only one candidate.")

(defun corfu--candiate-overlay-prepare (beg end)
  "Sets the default properties of the candidates overlay.
       The overlay can be dismissed with a mouse click."
  (when (not corfu--candidate-overlay-map)
    (setq corfu--candidate-overlay-map (make-sparse-keymap))
    (define-key corfu--candidate-overlay-map (kbd "<mouse-1>")
                (lambda ()
                  (interactive)
                  (delete-overlay corfu--candidate-overlay))))

  (if corfu--candidate-overlay
      (move-overlay corfu--candidate-overlay end end)
    (progn
      (setq corfu--candidate-overlay (make-overlay end end nil))
      ;; priority of 1k is the value used by Corfu.
      (overlay-put corfu--candidate-overlay  'priority 1000))))

(defun corfu--candidate-overlay-update (beg end prefix candidate how-many-candidates)
  "Updates the candidate overlay with the first candidate found by Corfu."
  (corfu--candiate-overlay-prepare beg end)

  (unless (string-empty-p candidate)
    (add-text-properties 0 1 '(cursor 1) candidate))

  (overlay-put corfu--candidate-overlay  'window (selected-window))
  (overlay-put corfu--candidate-overlay  'after-string
               (propertize
                candidate
                'face (if (= how-many-candidates 1)
                          'corfu--candidate-overlay-face-exact-match
                        'corfu--candidate-overlay-face)
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
    (overlay-put corfu--candidate-overlay 'after-string "")))

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
             (let* ((candidate (car corfu--candidates))
                    (how-many-candidates (length corfu--candidates))
                    (len (- end beg))
                    (prefix (buffer-substring-no-properties beg end))
                    (suffix (substring candidate len)))

               (if (and
                    ;; need candidate
                    candidate
                    ;; the prefix can't be empty (in case of corfu-auto-prefix equal 0)
                    (not (string-empty-p prefix))
                    ;; prefix need to match the candidate as there are „fuzzy”
                    ;; found candidates, esp. when using templates and the user
                    ;; could see strage results at the first character.
                    (string-prefix-p prefix candidate))

                   (corfu--candidate-overlay-update
                    beg
                    end
                    prefix
                    suffix
                    how-many-candidates)
                 ;; otherwise we hide the overlay.
                 (corfu-hide-candidate-overlay))))))))))

(defun corfu--candidate-overlay-post-command ()
  "Post command hook updating the candidate overlay when user inserts character
       and the cursor is at the end of word."
  (let* ((is-insert-command
          (corfu--match-symbol-p corfu-auto-commands this-command))
         (is-delete-command
          (corfu--match-symbol-p corfu-overlay-auto-commands this-command)))
    (if (and
         ;; we are not in minibuffer, as it looks awkward.
         (not (minibuffer-window-active-p (selected-window)))
         (not (and ;; do not update it the point have not moved.
               corfu--candidate-last-point
               (= corfu--candidate-last-point (point))))
         (or  ;; do not update if it is not one of the insert or delete commands.
          is-insert-command
          is-delete-command))
        (let ((next-char (char-after)))
          (when (or  ;; do not update if we are not at the end of the word.
                 (not next-char) ;; end of file
                 ;; one of whitespace, quoting character, punctuation,
                 ;; closing bracket, etc is next.
                 ;; When those characters follow next completion won't trigger
                 ;; eitherway: ' = * - + / ~ _ (have not investigated further)
                 (memq next-char '(?\s ?\t ?\r ?\n
                                       ?\" ?\` ?\) ?\] ?\>
                                       ?\. ?\, ?\: ?\;)))
            ;; When the completion backend is SLOW, i.e. like every LSP client,
            ;; then the overlay will not update and will interfere with the typing.
            ;; That's why we move preemptively when inserting and deleting the first
            ;; character (look awkward when typing a different word than the completion
            ;; but still looks better than flickering).
            ;; When deleting -- we just move the overlay so it will show
            ;; the „lagging” candidate.
            (when (and is-insert-command corfu--candidate-overlay)
              (let ((previous-text (overlay-get corfu--candidate-overlay 'after-string)))
                (when (not (string-empty-p previous-text))
                  (overlay-put corfu--candidate-overlay 'after-string
                               (substring previous-text 1)))
                (move-overlay corfu--candidate-overlay (point) (point))))

            ;; preserve the current position, show and update the overlay.
            (setq corfu--candidate-last-point (point))
            (corfu-show-candidate-overlay)))
      ;; or hide the overlay if the conditions where not met.
      (corfu-hide-candidate-overlay))))

;;;###autoload
(define-minor-mode corfu-candidate-overlay-mode
  "Show first candidate in an overlay while typing."
  :global t
  :group 'corfu
  (if corfu-candidate-overlay-mode
      (progn
        (add-hook 'post-command-hook #'corfu--candidate-overlay-post-command)
        (message "Enabled `corfu-candidate-overlay-mode'."))
    (progn
      (remove-hook 'post-command-hook #'corfu--candidate-overlay-post-command)
      (message "Disabled `corfu-candidate-overlay-mode'."))))

;;; corfu-candidate-overlay.el ends here
