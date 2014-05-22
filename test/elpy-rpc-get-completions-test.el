(ert-deftest elpy-rpc-get-completions ()
  (elpy-testcase ()
    (mletf* ((elpy-project-root () "/")
             (called-args nil)
             (elpy-rpc (&rest args) (setq called-args args)))

      (elpy-rpc-get-completions)

      (should (equal called-args
                     '("get_completions"
                       ("/" nil "" 0)
                       nil nil))))))
