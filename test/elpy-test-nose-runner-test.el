(ert-deftest elpy-test-nose-runner-should-run-single-test ()
  (elpy-testcase ()
    (mletf* ((command nil)
             (top nil)
             (elpy-test-run (start-dir &rest args) (setq command args
                                                         top start-dir)))

      (elpy-test-nose-runner "/project/root/"
                             "/project/root/package/module.py"
                             "package.module"
                             "TestClass.test_method")

      (should (equal command
                     '("nosetests" "package.module:TestClass.test_method")))
      (should (equal top "/project/root/")))))

(ert-deftest elpy-test-nose-runner-should-run-single-test ()
  (elpy-testcase ()
    (mletf* ((command nil)
             (top nil)
             (elpy-test-run (start-dir &rest args) (setq command args
                                                         top start-dir)))

      (elpy-test-nose-runner "/project/root/" nil nil nil)

      (should (equal command '("nosetests")))
      (should (equal top "/project/root/")))))
