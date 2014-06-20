(ert-deftest elpy-rpc-get-calltip ()
  (elpy-testcase ()
    (mletf* ((called-args nil)
             (elpy-rpc (&rest args) (setq called-args args)))

      (elpy-rpc-get-calltip)

      (should (equal called-args
                     '("get_calltip"
                       (nil "" 0)
                       nil nil))))))
