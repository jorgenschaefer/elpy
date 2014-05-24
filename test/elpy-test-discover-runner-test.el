(ert-deftest elpy-test-discover-runner-should-run-single-test ()
  (elpy-testcase ()
    (mletf* ((command nil)
             (top nil)
             (elpy-test-run (start-dir &rest args) (setq command args
                                                         top start-dir)))

      (elpy-test-discover-runner "/project/root/"
                                 "/project/root/package/module.py"
                                 "package.module"
                                 "TestClass.test_method")

      (should (equal command
                     '("python" "-m" "unittest"
                       "package.module.TestClass.test_method")))
      (should (equal top "/project/root/")))))

(ert-deftest elpy-test-discover-runner-should-run-single-test ()
  (elpy-testcase ()
    (mletf* ((command nil)
             (top nil)
             (elpy-test-run (start-dir &rest args) (setq command args
                                                         top start-dir)))

      (elpy-test-discover-runner "/project/root/" nil nil nil)

      (should (equal command
                     '("python" "-m" "unittest" "discover")))
      (should (equal top "/project/root/")))))
