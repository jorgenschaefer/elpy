(ert-deftest elpy-find--resolve-module-should-find-module ()
  (elpy-testcase ((:project project-root "foo.py" "bar.py"))
    (mletf* ((elpy-library-root () project-root)
             (buffer-file-name () (f-join project-root "foo.py")))

      (should (f-equal? (elpy-find--resolve-module "bar")
                        (f-join project-root "bar.py"))))))

(ert-deftest elpy-find--resolve-module-should-find-package ()
  (elpy-testcase ((:project project-root "foo/foo.py" "bar/__init__.py"))
    (mletf* ((elpy-library-root () project-root)
             (buffer-file-name () (f-join project-root "foo/foo.py")))

      (should (f-equal? (elpy-find--resolve-module "bar")
                        (f-join project-root "bar/__init__.py"))))))

(ert-deftest elpy-find--resolve-module-should-find-method ()
  (elpy-testcase ((:project project-root "foo/foo.py" "bar/__init__.py"))
    (mletf* ((elpy-library-root () project-root)
             (buffer-file-name () (f-join project-root "foo/foo.py")))

      (should (f-equal? (elpy-find--resolve-module "bar.Class.method")
                        (f-join project-root "bar/__init__.py"))))))

(ert-deftest elpy-find--resolve-module-should-find-complex-path ()
  (elpy-testcase ((:project project-root "foo/foo.py" "bar/baz/__init__.py"))
    (mletf* ((elpy-library-root () project-root)
             (buffer-file-name () (f-join project-root "foo/foo.py")))

      (should (f-equal? (elpy-find--resolve-module "..bar.baz")
                        (f-join project-root "bar/baz/__init__.py"))))))
