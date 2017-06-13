(ert-deftest elpy-rpc-get-names ()
  (elpy-testcase ()
    (mletf* ((called-args nil)
             (elpy-rpc (&rest args) (setq called-args args)))

      (elpy-rpc-get-names)

      (should (equal called-args
                     '("get_names"
                       (nil "" 0)
                       nil nil))))))
