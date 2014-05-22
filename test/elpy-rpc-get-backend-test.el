(ert-deftest elpy-rpc-get-backend ()
  (elpy-testcase ()
    (mletf* ((called-args nil)
             (elpy-rpc (&rest args) (setq called-args args)))

      (elpy-rpc-get-backend)

      (should (equal called-args
                     '("get_backend" nil nil nil))))))
