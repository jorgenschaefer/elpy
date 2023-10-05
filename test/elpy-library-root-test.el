(ert-deftest elpy-library-root-should-find-current-directory ()
  (elpy-testcase ((:project project-root "test.py"))
    (find-file (f-join project-root "test.py"))
    (should (f-equal? (elpy-library-root)
                      project-root))))

(ert-deftest elpy-library-root-should-find-current-directory-2 ()
  (elpy-testcase ((:project project-root
                            "p1/p2/test.py"
                            "p1/p2/__init__.py"
                            "p1/__init__.py"))
    (find-file (f-join project-root "p1/p2/test.py"))
    (should (f-equal? (elpy-library-root)
                      project-root))))
