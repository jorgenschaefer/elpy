;;; elpy-refactor.el --- Refactoring mode for Elpy

;; Copyright (C) 2020  Gaby Launay

;; Author: Gaby Launay <gaby.launay@protonmail.com>
;; URL: https://github.com/jorgenschaefer/elpy

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides an interface, including a major mode, to use
;; refactoring options provided by the Jedi library.

;;; Code:

;; We require elpy, but elpy loads us, so we shouldn't load it back.
;; (require 'elpy)
(require 'diff-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Refactor mode (for applying diffs)

(defvar elpy-refactor--saved-window-configuration nil
  "Saved windows configuration, so that we can restore it after `elpy-refactor' has done its thing.")

(defvar elpy-refactor--saved-pos nil
  "Line and column number of the position we were at before starting refactoring.")

(defvar elpy-refactor--modified-buffers '()
  "Keep track of the buffers modified by the current refactoring sessions.")

(defun elpy-refactor--apply-diff (proj-path diff)
  "Apply DIFF, looking for the files in PROJ-PATH."
  (let ((current-line (line-number-at-pos (point)))
        (current-col (- (point) (line-beginning-position))))
    (with-current-buffer (get-buffer-create " *Elpy Refactor*")
      (elpy-refactor-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert diff))
      (setq default-directory proj-path)
      (goto-char (point-min))
      (elpy-refactor--apply-whole-diff))
    (condition-case nil
        (progn
          (goto-char (point-min))
          (forward-line (- current-line 1))
          (beginning-of-line)
          (forward-char current-col))
      (error))
    ))

(defun elpy-refactor--display-diff (proj-path diff)
  "Display DIFF in a `diff-mode' window.

DIFF files should be relative to PROJ-PATH."
  (setq elpy-refactor--saved-window-configuration (current-window-configuration)
        elpy-refactor--saved-pos (list (line-number-at-pos (point) t)
                                       (- (point) (line-beginning-position)))
        elpy-refactor--modified-buffers '())
  (with-current-buffer (get-buffer-create "*Elpy Refactor*")
    (elpy-refactor-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize
               (substitute-command-keys
                (concat
                 "\\[diff-file-next] and \\[diff-file-prev] -- Move between files\n"
                 "\\[diff-hunk-next] and \\[diff-hunk-prev] -- Move between hunks\n"
                 "\\[diff-split-hunk] -- Split the current hunk at point\n"
                 "\\[elpy-refactor--apply-hunk] -- Apply the current hunk\n"
                 "\\[diff-kill-hunk] -- Kill the current hunk\n"
                 "\\[elpy-refactor--apply-whole-diff] -- Apply the whole diff\n"
                 "\\[elpy-refactor--quit] -- Quit\n"))
               'face 'bold)
              "\n\n")
      (align-regexp (point-min) (point-max) "\\(\\s-*\\) -- ")
      (goto-char (point-min))
      (while (search-forward " -- " nil t)
        (replace-match "  " nil t))
      (goto-char (point-max))
      (insert diff))
    (setq default-directory proj-path)
    (goto-char (point-min))
    (if (diff--some-hunks-p)
        (progn
          (select-window (display-buffer (current-buffer)))
          (diff-hunk-next))
      ;; quit if not diff at all...
      (message "No differences to validate")
      (kill-buffer (current-buffer)))))

(defvar elpy-refactor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'elpy-refactor--apply-hunk)
    (define-key map (kbd "C-c C-a") 'elpy-refactor--apply-whole-diff)
    (define-key map (kbd "C-c C-x") 'diff-kill-hunk)
    (define-key map (kbd "q") 'elpy-refactor--quit)
    (define-key map (kbd "C-c C-k") 'elpy-refactor--quit)
    (define-key map (kbd "h") 'describe-mode)
    (define-key map (kbd "?") 'describe-mode)
    map)
  "The key map for `elpy-refactor-mode'.")

(define-derived-mode elpy-refactor-mode diff-mode "Elpy Refactor"
  "Mode to display refactoring actions and ask confirmation from the user.

\\{elpy-refactor-mode-map}"
  :group 'elpy
  (view-mode 1))

(defun elpy-refactor--apply-hunk ()
  "Apply the current hunk."
  (interactive)
  (save-excursion
    (diff-apply-hunk))
  ;; keep track of modified buffers
  (let ((buf (find-buffer-visiting (diff-find-file-name))))
    (when buf
      (add-to-list 'elpy-refactor--modified-buffers buf)))
  ;;
  (diff-hunk-kill)
  (unless (diff--some-hunks-p)
    (elpy-refactor--quit)))

(defun elpy-refactor--apply-whole-diff ()
  "Apply the whole diff and quit."
  (interactive)
  (goto-char (point-min))
  (diff-hunk-next)
  (while (diff--some-hunks-p)
    (let ((buf (find-buffer-visiting (diff-find-file-name))))
      (when buf
        (add-to-list 'elpy-refactor--modified-buffers buf)))
    (condition-case nil
        (progn
          (save-excursion
            (diff-apply-hunk))
          (diff-hunk-kill))
      (error (diff-hunk-next)))) ;; if a hunk fail, switch to the next one
  ;; quit
  (elpy-refactor--quit))

(defun elpy-refactor--quit ()
  "Quit the refactoring session."
  (interactive)
  ;; save modified buffers
  (dolist (buf elpy-refactor--modified-buffers)
    (with-current-buffer buf
      (basic-save-buffer)))
  (setq elpy-refactor--modified-buffers '())
  ;; kill refactoring buffer
  (kill-buffer (current-buffer))
  ;; Restore window configuration
  (when elpy-refactor--saved-window-configuration
    (set-window-configuration elpy-refactor--saved-window-configuration)
    (setq elpy-refactor--saved-window-configuration nil))
  ;; Restore cursor position
  (when elpy-refactor--saved-pos
    (goto-char (point-min))
    (forward-line (- (car elpy-refactor--saved-pos) 1))
    (forward-char (car (cdr elpy-refactor--saved-pos)))
    (setq elpy-refactor--saved-pos nil)))



;;;;;;;;;;;;;;;;;
;; User functions

(defun elpy-refactor-rename (new-name &optional dontask)
  "Rename the symbol at point to NEW-NAME.

With a prefix argument (or if DONTASK is non-nil),
do not display the diff before applying."
  (interactive (list
                (let ((old-name (thing-at-point 'symbol)))
                  (if (or (not old-name)
                          (not (elpy-refactor--is-valid-symbol-p old-name)))
                      (error "No symbol at point")
                    (read-string
                     (format "New name for '%s': "
                             (thing-at-point 'symbol)))))))
  (unless (and new-name
               (elpy-refactor--is-valid-symbol-p new-name))
    (error "'%s' is not a valid python symbol"))
  (message "Gathering occurences of '%s'..."
           (thing-at-point 'symbol))
  (let* ((elpy-rpc-timeout 10)  ;; refactoring can be long...
         (diff (elpy-rpc-get-rename-diff new-name))
         (proj-path (alist-get 'project_path diff))
         (success (alist-get 'success diff))
         (diff (alist-get 'diff diff)))
    (cond ((not success)
           (error "Refactoring failed for some reason"))
          ((string= success "Not available")
           (error "This functionnality needs jedi > 0.17.0, please update"))
          ((or dontask current-prefix-arg)
           (message "Replacing '%s' with '%s'..."
                    (thing-at-point 'symbol)
                    new-name)
           (elpy-refactor--apply-diff proj-path diff)
           (message "Done"))
          (t
           (elpy-refactor--display-diff proj-path diff)))))

(defun elpy-refactor-extract-variable (new-name)
  "Extract the current region to a new variable NEW-NAME."
  (interactive "sNew name: ")
  (let ((beg (if (region-active-p)
                 (region-beginning)
               (car (or (bounds-of-thing-at-point 'symbol)
                        (error "No symbol at point")))))
        (end (if (region-active-p)
                 (region-end)
               (cdr (bounds-of-thing-at-point 'symbol)))))
    (when (or (elpy-refactor--is-valid-symbol-p new-name)
              (y-or-n-p "'%s' does not appear to be a valid python symbol. Are you sure you want to use it? "))
      (let* ((line-beg (save-excursion
                         (goto-char beg)
                         (line-number-at-pos)))
             (line-end (save-excursion
                         (goto-char end)
                         (line-number-at-pos)))
             (col-beg (save-excursion
                        (goto-char beg)
                        (- (point) (line-beginning-position))))
             (col-end (save-excursion
                        (goto-char end)
                        (- (point) (line-beginning-position))))
             (diff (elpy-rpc-get-extract-variable-diff
                    new-name line-beg line-end col-beg col-end))
             (proj-path (alist-get 'project_path diff))
             (success (alist-get 'success diff))
             (diff (alist-get 'diff diff)))
        (cond ((not success)
               (error "We could not extract the selection as a variable"))
              ((string= success "Not available")
               (error "This functionnality needs jedi > 0.17.0, please update"))
              (t
               (deactivate-mark)
               (elpy-refactor--apply-diff proj-path diff)))))))

(defun elpy-refactor-extract-function (new-name)
  "Extract the current region to a new function NEW-NAME."
  (interactive "sNew function name: ")
  (unless (region-active-p)
    (error "No selection"))
  (when (or (elpy-refactor--is-valid-symbol-p new-name)
            (y-or-n-p "'%s' does not appear to be a valid python symbol. Are you sure you want to use it? "))
    (let* ((line-beg (save-excursion
                       (goto-char (region-beginning))
                       (line-number-at-pos)))
           (line-end (save-excursion
                       (goto-char (region-end))
                       (line-number-at-pos)))
           (col-beg (save-excursion
                      (goto-char (region-beginning))
                      (- (point) (line-beginning-position))))
           (col-end (save-excursion
                      (goto-char (region-end))
                      (- (point) (line-beginning-position))))
           (diff (elpy-rpc-get-extract-function-diff
                  new-name line-beg line-end col-beg col-end))
           (proj-path (alist-get 'project_path diff))
           (success (alist-get 'success diff))
           (diff (alist-get 'diff diff)))
      (cond ((not success)
             (error "We could not extract the selection as a function"))
            ((string= success "Not available")
             (error "This functionnality needs jedi > 0.17.0, please update"))
            (t
             (deactivate-mark)
             (elpy-refactor--apply-diff proj-path diff))))))

(defun elpy-refactor-inline ()
  "Inline the variable at point."
  (interactive)
  (let* ((diff (elpy-rpc-get-inline-diff))
         (proj-path (alist-get 'project_path diff))
         (success (alist-get 'success diff))
         (diff (alist-get 'diff diff)))
    (cond ((not success)
           (error "We could not inline the variable '%s'"
                  (thing-at-point 'symbol)))
          ((string= success "Not available")
           (error "This functionnality needs jedi > 0.17.0, please update"))
          (t
           (elpy-refactor--apply-diff proj-path diff)))))


;;;;;;;;;;;;
;; Utilities

(defun elpy-refactor--is-valid-symbol-p (symbol)
  "Return t if SYMBOL is a valid python symbol."
  (eq 0 (string-match "^[a-zA-Z_][a-zA-Z0-9_]*$" symbol)))

;;;;;;;;;;;;
;; Compatibility
(unless (fboundp 'diff--some-hunks-p)
  (defun diff--some-hunks-p ()
    (save-excursion
      (goto-char (point-min))
      (re-search-forward diff-hunk-header-re nil t))))

(provide 'elpy-refactor)
;;; elpy-refactor.el ends here
