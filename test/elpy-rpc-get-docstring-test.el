(ert-deftest elpy-rpc-get-docstring ()
  (elpy-testcase ()
    (mletf* ((elpy-project-root () "/")
             (called-args nil)
             (elpy-rpc (&rest args) (setq called-args args)))

      (elpy-rpc-get-docstring)

      (should (equal called-args
                     '("get_docstring"
                       ("/" nil "" 0)
                       nil nil))))))
