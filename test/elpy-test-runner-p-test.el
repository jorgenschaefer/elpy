(ert-deftest elpy-test-runner-p-should-identify-test-runner ()
  (elpy-testcase ()
    (defun test-runner-test ()
      nil)

    (put 'test-runner-test 'elpy-test-runner-p t)

    (should (elpy-test-runner-p 'test-runner-test))))

(ert-deftest elpy-test-runner-p-should-not-identify-non-test-runner ()
  (elpy-testcase ()
    (defun test-runner-test ()
      nil)

    (put 'test-runner-test 'elpy-test-runner-p nil)

    (should-not (elpy-test-runner-p 'test-runner-test))))
