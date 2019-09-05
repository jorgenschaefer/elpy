(ert-deftest elpy-test-discover-runner-should-be-test-runner ()
  (elpy-testcase ()
    (elpy-test-runner-p 'elpy-test-discover-runner)))

(ert-deftest elpy-test-discover-runner-should-run-all-tests ()
  (elpy-testcase ()
    (mletf* ((command nil)
             (top nil)
             (elpy-test-run (start-dir &rest args) (setq command args
                                                         top start-dir)))

      (elpy-test-discover-runner "/project/root/" nil nil nil)

      (should (equal command
                     (list python-shell-interpreter "-m" "unittest" "discover")))
      (should (equal top "/project/root/")))))

(ert-deftest elpy-test-discover-runner-should-run-test-module ()
  (elpy-testcase ()
    (mletf* ((command nil)
             (top nil)
             (elpy-test-run (start-dir &rest args) (setq command args
                                                         top start-dir)))

      (elpy-test-discover-runner "/project/root/"
                                 "/project/root/package/module.py"
                                 "package.module"
                                 nil)

      (should (equal command
                     (list python-shell-interpreter "-m" "unittest"
                       "package.module")))
      (should (equal top "/project/root/")))))

(ert-deftest elpy-test-discover-runner-should-run-test-class ()
  (elpy-testcase ()
    (mletf* ((command nil)
             (top nil)
             (elpy-test-run (start-dir &rest args) (setq command args
                                                         top start-dir)))

      (elpy-test-discover-runner "/project/root/"
                                 "/project/root/package/module.py"
                                 "package.module"
                                 "TestClass")

      (should (equal command
                     (list python-shell-interpreter "-m" "unittest"
                       "package.module.TestClass")))
      (should (equal top "/project/root/")))))

(ert-deftest elpy-test-discover-runner-should-run-test-method ()
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
                     (list python-shell-interpreter "-m" "unittest"
                       "package.module.TestClass.test_method")))
      (should (equal top "/project/root/")))))
