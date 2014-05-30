(ert-deftest elpy-project-find-hg-root ()
  (elpy-testcase ((:project project-root
                            ".hg/test"
                            "foo/bar.py"))
    (find-file (f-join project-root "foo/bar.py"))
    (should (f-equal? (elpy-project-find-hg-root)
                      project-root))))
