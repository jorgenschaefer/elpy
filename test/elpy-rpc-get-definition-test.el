(ert-deftest elpy-rpc-get-definition ()
  (elpy-testcase ()
    (mletf* ((elpy-project-root () "/")
             (called-args nil)
             (elpy-rpc (&rest args) (setq called-args args)))

      (elpy-rpc-get-definition)

      (should (equal called-args
                     '("get_definition"
                       ("/" nil "" 0)
                       nil nil))))))
