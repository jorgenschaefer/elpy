(ert-deftest elpy-rpc-get-pydoc-completions ()
  (elpy-testcase ()
    (mletf* ((called-args nil)
             (elpy-rpc (&rest args) (setq called-args args)))

      (elpy-rpc-get-pydoc-completions "test")

            (should (equal called-args
                     '("get_pydoc_completions"
                       ("test")
                       nil nil))))))
