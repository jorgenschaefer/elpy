(ert-deftest elpy--test-file-should-return-nil-if-not-found ()
  (elpy-testcase ((:project project-root "foo.py"))
    (find-file (f-join project-root "foo.py"))

    (should (equal (elpy--test-file)
                   nil))))

(ert-deftest elpy--test-file-should-find-test-file-same-directory ()
  (elpy-testcase ((:project project-root "foo.py" "test_foo.py"))
    (find-file (f-join project-root "foo.py"))

    (should (f-equal? (elpy--test-file)
                      (f-join project-root "test_foo.py")))))

(ert-deftest elpy--test-file-should-find-test-file-in-test-directory ()
  (elpy-testcase ((:project project-root "foo.py" "test/test_foo.py"))
    (find-file (f-join project-root "foo.py"))

    (should (f-equal? (elpy--test-file)
                      (f-join project-root "test/test_foo.py")))))

(ert-deftest elpy--test-file-should-find-test-file-in-tests-directory ()
  (elpy-testcase ((:project project-root "foo.py" "tests/test_foo.py"))
    (find-file (f-join project-root "foo.py"))

    (should (f-equal? (elpy--test-file)
                      (f-join project-root "tests/test_foo.py")))))

(ert-deftest elpy--test-file-should-find-test-file-in-test-superdirectory ()
  (elpy-testcase ((:project project-root "pkg/foo.py" "test/test_foo.py"))
    (find-file (f-join project-root "pkg/foo.py"))

    (should (f-equal? (elpy--test-file)
                      (f-join project-root "test/test_foo.py")))))

(ert-deftest elpy--test-file-should-find-test-file-in-tests-superdirectory ()
  (elpy-testcase ((:project project-root "pkg/foo.py" "tests/test_foo.py"))
    (find-file (f-join project-root "pkg/foo.py"))

    (should (f-equal? (elpy--test-file)
                      (f-join project-root "tests/test_foo.py")))))

(ert-deftest elpy--test-file-should-find-implementation-file-same-directory ()
  (elpy-testcase ((:project project-root "foo.py" "test_foo.py"))
    (find-file (f-join project-root "test_foo.py"))

    (should (f-equal? (elpy--test-file)
                      (f-join project-root "foo.py")))))

(ert-deftest elpy--test-file-should-find-implementation-file-superdirectory ()
  (elpy-testcase ((:project project-root "foo.py" "tests/test_foo.py"))
    (find-file (f-join project-root "tests/test_foo.py"))

    (should (f-equal? (elpy--test-file)
                      (f-join project-root "foo.py")))))
