(ert-deftest elpy-test-django-runner-should-be-test-runner ()
  (elpy-testcase ()
    (elpy-test-runner-p 'elpy-test-django-runner)))

(ert-deftest elpy-test-django-runner-should-run-all-tests ()
  (elpy-testcase ()
    (mletf* ((command nil)
             (root nil)
             (elpy-test-run (start-dir &rest args) (setq command args
                                                         root start-dir)))

      (elpy-test-django-runner "/project/root/" nil nil nil)

      (should (equal command
                     '("django-admin.py" "test" "--noinput")))
      (should (equal root "/project/root/")))))

(ert-deftest elpy-test-django-runner-should-run-test-module ()
  (elpy-testcase ()
    (mletf* ((command nil)
             (root nil)
             (elpy-test-run (start-dir &rest args) (setq command args
                                                         root start-dir)))

      (elpy-test-django-runner "/project/root/"
                               "/project/root/package/module.py"
                               "package.module"
                               nil)

      (should (equal command
                     '("django-admin.py" "test" "--noinput"
                       "package.module")))
      (should (equal root "/project/root/")))))

(ert-deftest elpy-test-django-runner-should-run-test-class ()
  (elpy-testcase ()
    (mletf* ((command nil)
             (root nil)
             (elpy-django--get-test-format () ".")
             (elpy-test-run (start-dir &rest args) (setq command args
                                                         root start-dir)))

      (elpy-test-django-runner "/project/root/"
                               "/project/root/package/module.py"
                               "package.module"
                               "TestClass")

      (should (equal command
                     '("django-admin.py" "test" "--noinput"
                       "package.module.TestClass")))
      (should (equal root "/project/root/")))))

(ert-deftest elpy-test-django-runner-should-run-test-method ()
  (elpy-testcase ()
    (mletf* ((command nil)
             (root nil)
             (elpy-django--get-test-format () ".")
             (elpy-test-run (start-dir &rest args) (setq command args
                                                         root start-dir)))

      (elpy-test-django-runner "/project/root/"
                               "/project/root/package/module.py"
                               "package.module"
                               "TestClass.test_method")

      (should (equal command
                     '("django-admin.py" "test" "--noinput"
                       "package.module.TestClass.test_method")))
      (should (equal root "/project/root/")))))
