(ert-deftest elpy-project-find-python-root-should-find-setup-py ()
  (elpy-testcase ((:project project-root
                            "setup.py"
                            "foo/bar.py"))
    (find-file (f-join project-root "foo/bar.py"))
    (should (f-equal? (elpy-project-find-python-root)
                      project-root))))

(ert-deftest elpy-project-find-python-root-should-find-setup-cfg ()
  (elpy-testcase ((:project project-root
                            "setup.py"
                            "foo/bar.cfg"))
    (find-file (f-join project-root "foo/bar.cfg"))
    (should (f-equal? (elpy-project-find-python-root)
                      project-root))))

(ert-deftest elpy-project-find-python-root-should-find-pyproject-toml ()
  (elpy-testcase ((:project project-root
                            "pyproject.toml"
                            "foo/bar.py"))
    (find-file (f-join project-root "foo/bar.py"))
    (should (f-equal? (elpy-project-find-python-root)
                      project-root))))
