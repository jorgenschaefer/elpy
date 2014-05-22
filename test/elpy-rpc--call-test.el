(ert-deftest elpy-rpc--call ()
  (elpy-testcase ()
    (mletf* ((sent-string nil)
             (process-send-string (proc str) (setq sent-string str)))

      (elpy-rpc--call "test-method" '(1 2) 'success 'error)

      (should (stringp sent-string)))))
