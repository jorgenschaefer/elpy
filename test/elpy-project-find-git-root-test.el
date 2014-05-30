(ert-deftest elpy-project-find-git-root ()
  (elpy-testcase ((:project project-root
                            ".git/test"
                            "foo/bar.py"))
    (find-file (f-join project-root "foo/bar.py"))
    (should (f-equal? (elpy-project-find-git-root)
                      project-root))))
