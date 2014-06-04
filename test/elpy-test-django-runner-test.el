(ert-deftest elpy-test-django-runner-should-run-single-test ()
  (elpy-testcase ()
    (mletf* ((command nil)
             (top nil)
             (elpy-test-run (start-dir &rest args) (setq command args
                                                         top start-dir)))

      (elpy-test-django-runner "/project/root/"
                               "/project/root/package/module.py"
                               "package.module"
                               "TestClass.test_method")

      (should (equal command
                     '("django-admin.py" "test"
                       "package.module.TestClass.test_method")))
      (should (equal top "/project/root/")))))

(ert-deftest elpy-test-django-runner-should-run-single-test ()
  (elpy-testcase ()
    (mletf* ((command nil)
             (top nil)
             (elpy-test-run (start-dir &rest args) (setq command args
                                                         top start-dir)))

      (elpy-test-django-runner "/project/root/" nil nil nil)

      (should (equal command
                     '("django-admin.py" "test" "--noinput")))
      (should (equal top "/project/root/")))))
