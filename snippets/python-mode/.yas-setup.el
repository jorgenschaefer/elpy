(defvar elpy-snippet-split-arg-arg-regex
"\\([[:alnum:]*]+\\)\\(:[[:blank:]]*[[:alpha:]]*\\)?\\([[:blank:]]*=[[:blank:]]*[[:alnum:]]*\\)?"
"Regular expression matching an argument of a python function.
First group should give the argument name.")

(defvar elpy-snippet-split-arg-separator
"[[:blank:]]*,[[:blank:]]*"
"Regular expression matching the separator in a list of argument.")

(defun elpy-snippet-split-args (arg-string)
  "Split the python argument string ARG-STRING into a tuple of argument names."
  (mapcar (lambda (x)
            (when (string-match elpy-snippet-split-arg-arg-regex x)
              (match-string-no-properties 1 x)))
          (split-string arg-string elpy-snippet-split-arg-separator t)))

(defun elpy-snippet-current-method-and-args ()
  "Return information on the current definition."
  (let ((current-defun (python-info-current-defun))
        (current-arglist
         (save-excursion
           (python-nav-beginning-of-defun)
           (when (re-search-forward "(" nil t)
             (let* ((start (point))
                    (end (progn
                           (forward-char -1)
                           (forward-sexp)
                           (- (point) 1))))
               (elpy-snippet-split-args
                (buffer-substring-no-properties start end))))))
        class method args)
    (unless current-arglist
      (setq current-arglist '("self")))
    (if (and current-defun
             (string-match "^\\(.*\\)\\.\\(.*\\)$" current-defun))
        (setq class (match-string 1 current-defun)
              method (match-string 2 current-defun))
      (setq class "Class"
            method "method"))
    (list class method current-arglist)))

(defun elpy-snippet-init-assignments (arg-string)
  "Return the typical __init__ assignments for arguments in ARG-STRING."
  (let ((indentation (make-string (save-excursion
                                    (goto-char start-point)
                                    (current-indentation))
                                  ?\s)))
    (mapconcat (lambda (arg)
                 (if (string-match "^\\*" arg)
                     ""
                   (format "self.%s = %s\n%s" arg arg indentation)))
               (elpy-snippet-split-args arg-string)
               "")))

(defun elpy-snippet-super-form ()
  "Return (Class, first-arg).method if Py2.
Else return ().method for Py3."
  (let* ((defun-info (elpy-snippet-current-method-and-args))
         (class (nth 0 defun-info))
         (method (nth 1 defun-info))
         (args (nth 2 defun-info))
         (first-arg (nth 0 args))
         (py-version-command " -c 'import sys ; print(sys.version_info.major)'")
         ;; Get the python version. Either 2 or 3
         (py-version-num (substring (shell-command-to-string (concat elpy-rpc-python-command py-version-command))0 1)))
    (if (string-match py-version-num "2")
        (format "(%s, %s).%s" class first-arg method)
      (format "().%s" method))))

(defun elpy-snippet-super-arguments ()
  "Return the argument list for the current method."
  (mapconcat (lambda (x) x)
             (cdr (nth 2 (elpy-snippet-current-method-and-args)))
             ", "))
