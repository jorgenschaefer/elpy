;;; elpy-backport-24.3.el --- provides subr-x for pre 24.4 emacsen

;;; Commentary:
;;  backport of subr-x -- see https://github.com/vijaykiran/spacemacs/commit/0776a66fab8cb5ad51570c3f0f0d5397b27ffb4a

;;; Code:

(require 'company)
(unless (featurep 'subr-x)
  ;; `subr-x' function for Emacs 24.3 and below
  (defsubst string-join (strings &optional separator)
    "Join all STRINGS using SEPARATOR."
    (mapconcat 'identity strings separator))
  (defsubst string-trim-left (string)
    "Remove leading whitespace from STRING."
    (if (string-match "\\`[ \t\n\r]+" string)
        (replace-match "" t t string)
      string))
  (defsubst string-trim-right (string)
    "Remove trailing whitespace from STRING."
    (if (string-match "[ \t\n\r]+\\'" string)
        (replace-match "" t t string)
      string))
  (defsubst string-trim (string)
    "Remove leading and trailing whitespace from STRING."
    (string-trim-left (string-trim-right string)))
  (defsubst string-empty-p (string)
    "Check whether STRING is empty."
    (string= string ""))
  (defsubst string-remove-suffix (suffix string)
    "Remove SUFFIX from STRING if present."
    (if (string-suffix-p suffix string)
        (substring string 0 (- (length string) (length suffix)))
      string)))

(unless (fboundp 'python-shell-get-process-or-error)
  ;; python-shell-get-process-or-error def for 24.3  
  (defun python-shell-get-process-or-error (&optional interactivep)
    "Return inferior Python process for current buffer or signal error.
When argument INTERACTIVEP is non-nil, use `user-error' instead
of `error' with a user-friendly message."
    (or (python-shell-get-process)
        (if interactivep
            (user-error
             "Start a Python process first with `M-x run-python' or `%s'."
             ;; Get the binding.
             (key-description
              (where-is-internal
               #'run-python overriding-local-map t)))
          (error
           "No inferior Python process running.")))))

(unless (fboundp 'python-shell-get-buffer)
  ;; python-shell-get-buffer def for 24.3 
  (defun python-shell-get-buffer ()
    "Return inferior Python buffer for current buffer.
If current buffer is in `inferior-python-mode', return it."
    (if (derived-mode-p 'inferior-python-mode)
        (current-buffer)
      (format "*%s*" (python-shell-get-process-name nil))))
)

(provide 'elpy-backport-24.3)
;;; elpy-backport-24.3.el ends here
