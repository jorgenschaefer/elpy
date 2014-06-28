(ert-deftest elpy-test-should-run-current-test-if-in-test ()
  (elpy-testcase ()
    (python-mode)
    (elpy-mode)
    (mletf* ((current-test (list "/dir/project"
                                 "/dir/project/test.py"
                                 "test"
                                 "TestClass.test_method"))
             (elpy-test-at-point () current-test)
             (called-args nil)
             (elpy-test-runner (lambda (&rest args)
                                 (setq called-args args))))

      (elpy-test)

      (should (equal called-args current-test)))))

(ert-deftest elpy-test-should-run-current-module-if-in-test ()
  (elpy-testcase ()
    (python-mode)
    (elpy-mode)
    (mletf* ((current-test (list "/dir/project"
                                 "/dir/project/test.py"
                                 "test"
                                 nil))
             (elpy-test-at-point () current-test)
             (called-args nil)
             (elpy-test-runner (lambda (&rest args)
                                 (setq called-args args))))

      (elpy-test)

      (should (equal called-args current-test)))))

(ert-deftest elpy-test-should-run-all-tests-otherwise ()
  (elpy-testcase ()
    (python-mode)
    (elpy-mode)
    (mletf* ((current-test (list "/dir/project"
                                 nil
                                 nil
                                 nil))
             (elpy-test-at-point () current-test)
             (called-args nil)
             (elpy-test-runner (lambda (&rest args)
                                 (setq called-args args))))

      (elpy-test)

      (should (equal called-args (list "/dir/project" nil nil nil))))))

(ert-deftest elpy-test-should-run-all-tests-with-prefix-argument ()
  (elpy-testcase ()
    (python-mode)
    (elpy-mode)
    (mletf* ((current-test (list "/dir/project"
                                 "/dir/project/test.py"
                                 "test"
                                 "TestClass.test_method"))
             (elpy-test-at-point () current-test)
             (called-args nil)
             (elpy-test-runner (lambda (&rest args)
                                 (setq called-args args))))

      (elpy-test 4)

      (should (equal called-args (list "/dir/project" nil nil nil))))))
