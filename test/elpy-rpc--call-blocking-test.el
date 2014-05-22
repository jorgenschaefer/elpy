(ert-deftest elpy-rpc--call-blocking-should-return-success-value ()
  (elpy-testcase ()
    (mletf* ((elpy-rpc--timeout 0.1)
             (elpy-rpc--call
              (method params success error)
              (funcall success "test-success")))
      (should (equal (elpy-rpc--call-blocking "test-method" nil)
                     "test-success")))))

(ert-deftest elpy-rpc--call-blocking-should-call-default-error ()
  (elpy-testcase ()
    (mletf* ((error-called nil)
             (elpy-rpc--default-error-callback
              (error-object)
              (when (equal error-object "test-failure")
                (setq error-called t)))
             (elpy-rpc--timeout 0.1)
             (elpy-rpc--call
              (method params success error)
              (funcall error "test-failure")))

      (elpy-rpc--call-blocking "test-method" nil)

      (should error-called))))

(ert-deftest elpy-rpc--call-blocking-should-fail-after-timeout ()
  (elpy-testcase ()
    (mletf* ((elpy-rpc--timeout 0.1)
             (elpy-rpc--call (method params success error) nil))
      (should-error (elpy-rpc--call-blocking "test-method" nil)))))
