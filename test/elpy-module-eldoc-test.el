(ert-deftest elpy-module-eldoc-global-init ()
  (elpy-testcase ()
    (elpy-module-eldoc 'global-init)

    (should (eq eldoc-minor-mode-string nil))))

(ert-deftest elpy-module-eldoc-buffer-init ()
  (elpy-testcase ()
    (elpy-module-eldoc 'buffer-init)

    (should eldoc-mode)

    (should
     (if (boundp 'eldoc-documentation-functions)
         (member 'elpy-eldoc-documentation eldoc-documentation-functions)
       (eq eldoc-documentation-function 'elpy-eldoc-documentation)))))

(ert-deftest elpy-module-eldoc-buffer-stop ()
  (elpy-testcase ()
    (eldoc-mode 1)

    (elpy-module-eldoc 'buffer-stop)

    (should-not eldoc-mode)))
