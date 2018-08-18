;;; elpy-backport-24.3.el --- provides subr-x for pre 24.4 emacsen

;;; Commentary:
;;  backport of subr-x -- see https://github.com/vijaykiran/spacemacs/commit/0776a66fab8cb5ad51570c3f0f0d5397b27ffb4a

;;; Code:

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

(provide 'elpy-backport-24.3)
;;; elpy-backport-24.3.el ends here
