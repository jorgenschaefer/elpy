(ert-deftest elpy-rpc-get-pydoc-documentation ()
  (elpy-testcase ()
    (mletf* ((called-args nil)
             (elpy-rpc (&rest args) (setq called-args args)))

      (elpy-rpc-get-pydoc-documentation "test")

      (should (equal called-args
                     '("get_pydoc_documentation"
                       ("test")
                       nil nil))))))
