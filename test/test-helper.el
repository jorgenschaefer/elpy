(require 'f)
(add-to-list 'load-path (f-parent (f-dirname (f-this-file))))
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
