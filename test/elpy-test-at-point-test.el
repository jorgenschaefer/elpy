(ert-deftest elpy-test-at-point-should-return-library-root-without-file ()
  (elpy-testcase ()
    (mletf* ((saved nil)
             (save-some-buffers () (setq saved 'all))
             (save-buffer () (setq saved 'one))
             (elpy-library-root () "/project/root"))

      (should (equal (elpy-test-at-point)
                     (list "/project/root"
                           nil nil nil)))
      (should (eq saved 'all)))))

(ert-deftest elpy-test-at-point-should-return-library-root-without-test ()
  (elpy-testcase ()
    (mletf* ((saved nil)
             (save-some-buffers () (setq saved 'all))
             (save-buffer () (setq saved 'one))
             (elpy-library-root () "/project/root")
             (buffer-file-name "/project/root/package/implementation.py")
             (elpy-test-do-at-point-p 'elpy-all-have-test-p))

      (should (equal (elpy-test-at-point)
                     (list "/project/root"
                           nil nil nil)))
      (should (eq saved 'all)))))

(ert-deftest elpy-test-at-point-should-return-test-module ()
  (elpy-testcase ()
    (mletf* ((saved nil)
             (save-some-buffers () (setq saved 'all))
             (save-buffer () (setq saved 'one))
             (elpy-library-root () "/project/root")
             (buffer-file-name "/project/root/tests/test.py"))

      (should (equal (elpy-test-at-point)
                     (list "/project/root"
                           "/project/root/tests/test.py" "tests.test" nil)))
      (should (eq saved 'one)))))

(ert-deftest elpy-test-at-point-should-return-current-test-class ()
  (elpy-testcase ()
    (mletf* ((saved nil)
             (save-some-buffers () (setq saved 'all))
             (save-buffer () (setq saved 'one))
             (elpy-library-root () "/project/root")
             (buffer-file-name "/project/root/tests/test.py"))
      (insert-source "class TestClass(TestCase):"
                     "    def test_method(self):"
                     "        self.assertTrue(False)")
      (goto-char (point-min))
      (should (equal (elpy-test-at-point)
                     (list "/project/root"
                           "/project/root/tests/test.py"
                           "tests.test"
                           (if (version< emacs-version "24.3")
                               nil
                             "TestClass"))))
      (should (eq saved 'one)))))

(ert-deftest elpy-test-at-point-should-work-when-method-has-test ()
  (elpy-testcase ()
    (mletf* ((saved nil)
             (save-some-buffers () (setq saved 'all))
             (save-buffer () (setq saved 'one))
             (elpy-library-root () "/project/root")
             (buffer-file-name "/project/root/other/other.py"))
      (insert-source "class SomeClass(TestCase):"
                     "    def test_method(self):"
                     "        self.assertTrue(False)")
      (backward-char)
      (should (equal (python-info-current-defun) "SomeClass.test_method"))
      (should (equal (elpy-test--current-test-name) "SomeClass.test_method"))
      (should (equal (elpy-test-at-point)
                     (list "/project/root"
                           "/project/root/other/other.py"
                           "other.other"
                           (if (version< emacs-version "24.3")
                               nil
                             "SomeClass.test_method"))))
      (should (eq saved 'one)))))


(ert-deftest elpy-test-at-point-should-work-when-function-has-test ()
  (elpy-testcase ()
    (mletf* ((saved nil)
             (save-some-buffers () (setq saved 'all))
             (save-buffer () (setq saved 'one))
             (elpy-library-root () "/project/root")
             (buffer-file-name "/project/root/other/other.py"))
      (insert-source "def test_method(self):"
                     "    self.assertTrue(False)")
      (goto-char (point-min))

      (should (equal (elpy-test-at-point)
                     (list "/project/root"
                           "/project/root/other/other.py"
                           "other.other"
                           (if (version< emacs-version "24.3")
                               nil
                             "test_method"))))
      (should (eq saved 'one)))))

(ert-deftest elpy-test-at-point-should-work-when-class-has-test ()
  (elpy-testcase ()
    (mletf* ((saved nil)
             (save-some-buffers () (setq saved 'all))
             (save-buffer () (setq saved 'one))
             (elpy-library-root () "/project/root")
             (buffer-file-name "/project/root/other/other.py"))
      (insert-source "class TestClass(TestCase):"
                     "    def other_method(self):"
                     "        self.assertTrue(False)")
      (backward-char)

      (should (equal (elpy-test-at-point)
                     (list "/project/root"
                           "/project/root/other/other.py"
                           "other.other"
                           (if (version< emacs-version "24.3")
                               nil
                             "TestClass.other_method"))))
      (should (eq saved 'one)))))

(ert-deftest elpy-test-at-point-should-return-current-test-method ()
  (elpy-testcase ()
    (mletf* ((saved nil)
             (save-some-buffers () (setq saved 'all))
             (save-buffer () (setq saved 'one))
             (elpy-library-root () "/project/root")
             (buffer-file-name "/project/root/tests/test.py"))
      (insert-source "class TestClass(TestCase):"
                     "    def test_method(self):"
                     "        self.assertTrue(False)")
      (goto-char (point-min))
      (search-forward "False)")

      (should (equal (elpy-test-at-point)
                     (list "/project/root"
                           "/project/root/tests/test.py"
                           "tests.test"
                           (if (version< emacs-version "24.3")
                               "TestClass"
                             "TestClass.test_method"))))
      (should (eq saved 'one)))))
