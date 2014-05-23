(ert-deftest elpy-module-sane-defaults-buffer-init ()
  (elpy-testcase ()
    (python-mode)
    (elpy-mode)

    (elpy-module-sane-defaults 'buffer-init)

    (should (eq forward-sexp-function nil))
    (should (= comment-inline-offset 2))))
