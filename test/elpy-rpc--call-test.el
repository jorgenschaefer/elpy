(ert-deftest elpy-rpc--call-should-send-something ()
  (elpy-testcase ()
    (mletf* ((sent-string nil)
             (process-send-string (proc str) (setq sent-string str)))

      (elpy-rpc--call "test-method" '(1 2) 'success 'error)

      (should (stringp sent-string)))))

(ert-deftest elpy-rpc--call-should-return-promise ()
  (elpy-testcase ()
    (mletf* ((sent-string nil)
             (process-send-string (proc str) (setq sent-string str))
             (promise nil)
             (called-with nil))

      (setq promise
            (elpy-rpc--call "test-method" '(1 2)
                            (lambda (arg)
                              (setq called-with arg))
                            nil))
      (elpy-promise-resolve promise "test")

      (should (elpy-promise-p promise))
      (should (equal called-with "test")))))
