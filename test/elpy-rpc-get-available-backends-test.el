(ert-deftest elpy-rpc-get-available-backends ()
  (elpy-testcase ()
    (mletf* ((called-args nil)
             (elpy-rpc (&rest args) (setq called-args args)))

      (elpy-rpc-get-available-backends)

      (should (equal called-args
                     '("get_available_backends" nil nil nil))))))
