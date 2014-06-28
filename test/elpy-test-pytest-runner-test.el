(ert-deftest elpy-test-pytest-runner-should-run-single-test ()
  (elpy-testcase ()
    (mletf* ((command nil)
             (top nil)
             (elpy-test-run (start-dir &rest args) (setq command args
                                                         top start-dir)))

      (elpy-test-pytest-runner "/project/root/"
                               "/project/root/package/module.py"
                               "package.module"
                               "TestClass.test_method")

      (should (equal command
                     '("py.test" "/project/root/package/module.py::TestClass::test_method")))
      (should (equal top "/project/root/")))))

(ert-deftest elpy-test-pytest-runner-should-run-single-test ()
  (elpy-testcase ()
    (mletf* ((command nil)
             (top nil)
             (elpy-test-run (start-dir &rest args) (setq command args
                                                         top start-dir)))

      (elpy-test-pytest-runner "/project/root/" nil nil nil)

      (should (equal command '("py.test")))
      (should (equal top "/project/root/")))))

(ert-deftest elpy-test-pytest-runner-should-be-test-runner ()
  (elpy-testcase ()
    (elpy-test-runner-p 'elpy-test-pytest-runner)))
