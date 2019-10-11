;; -*- lexical-binding: t -*-

(require 'f)
(let ((elpy-dir (f-parent (f-dirname (f-this-file)))))
  (add-to-list 'load-path elpy-dir)
  (add-to-list 'process-environment (format "PYTHONPATH=%s:%s"
					    elpy-dir
					    (getenv "PYTHONPATH")))
  (add-to-list 'process-environment "ELPY_TEST=1"))
(require 'elpy)
;; Travis regularly has some lag for some reason.
(setq elpy-rpc-timeout 10)
;; If the environment variable `ELPY_TEST_DONT_USE_VIRTUALENV' is
;; defined, don't check for features involving virtualenvs.
;; This permits testing on platforms that do not allow virtualenvs
;; (for the Debian package for example)
(setq elpy-test-dont-use-virtualenv (getenv "ELPY_TEST_DONT_USE_VIRTUALENV"))
(when elpy-test-dont-use-virtualenv
  (message "ELPY_TEST_DONT_USE_VIRTUALENV is set, skipping tests using virtualenvs.")
  (setq elpy-rpc-virtualenv-path 'system))

(defmacro mletf* (bindings &rest body)
  "Liket `cl-letf*', just with a slightly more concise function syntax.

\(mletf* ((var 5)
         (fun (arg) (* arg 2)))
  (fun var))
=> 10"
  (declare (indent 1))
  `(cl-letf* ,(mapcar (lambda (binding)
                        (if (cddr binding)
                            `((symbol-function ',(car binding))
                              (lambda ,(cadr binding)
                                ,@(cddr binding)))
                          binding))
                      bindings)
     ,@body))

(defmacro with-temp-dir (name &rest body)
  "Create a temporary directory and bind the symbol NAME to the path.

Run BODY with that binding."
  (declare (indent 1))
  `(let ((,name (make-temp-file "elpy-test-" t)))
     (unwind-protect
         (progn ,@body)
       (ignore-errors
         (delete-directory ,name t)))))

(defmacro save-buffer-excursion (&rest body)
  (declare (indent 0))
  (let ((old-process-list (make-symbol "old-process-list"))
        (old-buffer-list (make-symbol "old-buffer-list")))
    `(let ((,old-process-list (process-list))
           (,old-buffer-list (buffer-list)))
       (unwind-protect
           (progn ,@body)
         (dolist (proc (process-list))
           (unless (member proc ,old-process-list)
             (kill-process proc)))
         (let ((kill-buffer-query-functions nil))
           (dolist (buf (buffer-list))
             (unless (member buf ,old-buffer-list)
               (kill-buffer buf))))))))

(defun elpy-testcase-transform-spec (speclist body)
  (if (null speclist)
      `(progn ,@body)
    (let ((spec (car speclist)))
      (pcase (car spec)
        (`:project
         (let ((symbol (cadr spec))
               (filespec (cddr spec)))
           `(with-temp-dir ,symbol
              (elpy-testcase-create-files ,symbol
                                          ',filespec)
              ,(elpy-testcase-transform-spec (cdr speclist)
                                             body))))
        (`:emacs-required
         (unless (version< emacs-version (cadr spec))
           (elpy-testcase-transform-spec (cdr speclist)
					 body)))
        (`:teardown
         `(unwind-protect
              ,(elpy-testcase-transform-spec (cdr speclist)
                                             body)
            ,@ (cdr spec)))
        (_
         (error "Bad environment specifier %s" (car spec)))))))

(defmacro elpy-testcase (spec &rest body)
  "Initialize Emacs using SPEC, then run BODY in the environment.

This will try as best as possible to create a clean start
environment for the test.

SPEC is a list of environment specifiers. Each specifier is
itself a list where the car indicates the type of environment.

\(:project symbol files ...)

  Create a temporary directory and bind the name to SYMBOL.
  Create FILES under that directory. FILES is a list of file
  names, possibly including directory names."
  (declare (indent 1))
  `(save-buffer-excursion
     (with-temp-buffer
       (setq elpy-rpc-timeout 100)
       ,(elpy-testcase-transform-spec spec body))
     (when (and (boundp 'elpy-enable)
                elpy-enable)
       (elpy-disable))))

(defun elpy-testcase-create-files (basedir filespec)
  "In BASEDIR, create files according to FILESPEC.

FILESPEC is a list of two-element lists, where the first element
is a file name relative to BASEDIR and the second the contents
for that file."
  (dolist (spec filespec)
    (let* ((filename (if (stringp spec)
                         spec
                       (car spec)))
           (contents (if (stringp spec)
                         ""
                       (cadr spec)))
           (fullname (format "%s/%s" basedir filename))
           (dirname (file-name-directory fullname)))
      (unless (file-directory-p dirname)
        (make-directory dirname t))
      (write-region contents nil fullname))))

(defun elpy/mark-region (beg end)
  (transient-mark-mode 1)
  (set-mark beg)
  (goto-char end))

(defun elpy/wait-for-output (output &optional max-wait)
  (let ((end (time-add (current-time)
                       (seconds-to-time (or max-wait 10)))))
    (while (and (time-less-p (current-time)
                             end)
                (save-excursion
                  (goto-char (point-min))
                  (not (re-search-forward output nil t))))
      (accept-process-output (get-buffer-process (current-buffer))
                             1))))

(defun insert-source (&rest lines)
  (dolist (line lines)
    (insert line "\n")))

(defun buffer-string-with-point ()
  "Return the contents of the current buffer, with point marked with _|_."
  (insert "_|_")
  (prog1 (buffer-string)
    (delete-char -3)))

(defun set-buffer-string-with-point (&rest strings)
  "Set STRINGS to be the buffer contents, moving point wherever _|_ is."
  (erase-buffer)
  (apply #'insert-source strings)
  (goto-char (point-min))
  (search-forward "_|_")
  (delete-char -3))

(defun source-string (&rest lines)
  (mapconcat (lambda (line) (concat line "\n"))
             lines
             ""))

(defun buffer-be (&rest lines)
  (equal (apply #'source-string lines)
         (buffer-string-with-point)))

(defun buffer-be-explainer (&rest lines)
  `(buffer-contents-differ ,(apply #'source-string lines)
                           ,(buffer-string-with-point)))
(put 'buffer-be 'ert-explainer 'buffer-be-explainer)

(setq yas-verbosity 0)
(setq yas-snippet-dirs ())


;; Avoid asking stuff during tests
(defun yes-or-no-p (&rest args) t)
(defun y-or-n-p (&rest args) t)
