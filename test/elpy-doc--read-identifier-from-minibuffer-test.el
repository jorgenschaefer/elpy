;;; Another of these highly interactive functions. Check that the
;;; function we're using actually exists.

(ert-deftest elpy-doc--read-identifier-from-minibuffer ()
  (elpy-testcase ()
    (completion-table-dynamic (lambda (prefix) nil))))
