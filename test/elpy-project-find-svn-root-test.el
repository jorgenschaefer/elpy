(ert-deftest elpy-project-find-svn-root ()
  (elpy-testcase ((:project project-root
                            ".svn/f"
                            "foo/.svn/f"
                            "foo/bar.py"))
    (find-file (f-join project-root "foo/bar.py"))
    (should (f-equal? (elpy-project-find-svn-root)
                      project-root))))
