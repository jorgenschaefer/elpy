(require 'f)
(let ((elpy-dir (f-parent (f-dirname (f-this-file)))))
  (add-to-list 'load-path elpy-dir)
  (add-to-list 'process-environment (format "PYTHONPATH=%s" elpy-dir)))
(require 'elpy)

(require 'mocker)

(defmacro with-temp-dir (name &rest body)
  "Create a temporary directory and bind the symbol NAME to the path.

Run BODY with that binding."
  (declare (indent 1))
  `(let ((,name (make-temp-file "elpy-test-" t)))
     (unwind-protect
         (progn ,@body)
       (ignore-errors
         (delete-directory ,name t)))))
(put 'with-temp-dir 'lisp-indent-function 1)

(defmacro with-elpy-file (tmpdir name &rest body)
  (let ((buffer (make-symbol "file")))
    `(with-temp-dir ,tmpdir
       (let ((,buffer (find-file (format "%s/%s" ,tmpdir ,name))))
         (unwind-protect
             (with-current-buffer ,buffer
               (python-mode)
               (elpy-mode 1)
               ,@body)
           (let ((kill-buffer-query-functions nil))
             (kill-buffer ,buffer)))))))
(put 'with-elpy-file 'lisp-indent-function 2)
