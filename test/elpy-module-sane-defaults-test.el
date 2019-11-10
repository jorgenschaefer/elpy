(ert-deftest elpy-module-sane-defaults-buffer-init ()
  (elpy-testcase ()
    (elpy-enable)
    (python-mode)

    (should (eq forward-sexp-function nil))
    (should (= comment-inline-offset 2))))
