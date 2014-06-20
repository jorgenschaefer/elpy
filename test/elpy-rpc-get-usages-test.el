(ert-deftest elpy-rpc-get-completions ()
  (elpy-testcase ()
    (mletf* ((called-args nil)
             (elpy-rpc (&rest args) (setq called-args args)))

      (elpy-rpc-get-usages)

      (should (equal called-args
                     '("get_usages"
                       (nil "" 0)
                       nil nil))))))
