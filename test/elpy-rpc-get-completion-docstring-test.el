(ert-deftest elpy-rpc-get-completion-docstring ()
  (elpy-testcase ()
    (mletf* ((called-args nil)
             (elpy-rpc (&rest args) (setq called-args args)))

      (elpy-rpc-get-completion-docstring "foo")

      (should (equal called-args
                     '("get_completion_docstring"
                       ("foo")
                       nil nil))))))
