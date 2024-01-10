;;; -*-coding: utf-8-*-

(ert-deftest elpy-rpc-should-call-async-with-success-callback ()
  (elpy-testcase ()
    (mletf* ((elpy-rpc--call
              (method params success error)
              (should (equal method "test-method"))
              (should (equal params nil))
              (should (equal success 'success))
              (should (equal error 'error))))
      (elpy-rpc "test-method" nil 'success 'error))))

(ert-deftest elpy-rpc-should-use-default-without-error-callback ()
  (elpy-testcase ()
    (mletf* ((elpy-rpc--call
              (method params success error)
              (should (equal error 'elpy-rpc--default-error-callback))))
      (elpy-rpc "test-method" nil 'success))))

(ert-deftest elpy-rpc-should-use-default-without-error-callback-2 ()
  (elpy-testcase ()
    (mletf* ((elpy-rpc--call-blocking
              (method params)
              (should (equal method "test-method"))
              (should (equal params nil))))
      (elpy-rpc "test-method" nil))))

(ert-deftest elpy-rpc-integration ()
  (elpy-testcase ()
    (should (equal (elpy-rpc "echo" '(23 "möp"))
                   '(23 "möp")))))
