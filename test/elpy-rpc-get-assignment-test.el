(ert-deftest elpy-rpc-get-assignment ()
  (elpy-testcase ()
    (mletf* ((called-args nil)
             (elpy-rpc (&rest args) (setq called-args args)))

      (elpy-rpc-get-assignment)

      (should (equal called-args
                     '("get_assignment"
                       (nil "" 0)
                       nil nil))))))
