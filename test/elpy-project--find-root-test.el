(ert-deftest elpy-project--find-root-should-use-projectile-root ()
  (elpy-testcase ()
    (mocker-let ((projectile-project-root
                  ()
                  ((:output "/project/root"))))
      (should (f-equal? (elpy-project--find-root)
                        "/project/root")))))

(ert-deftest elpy-project--find-root-should-find-root-of-packages ()
  (elpy-testcase ((:project project-root
                            "package/__init__.py"
                            "package/foo.py"))
    (find-file (f-join project-root "package/foo.py"))
    (should (f-equal? (elpy-project--find-root)
                      project-root))))

(ert-deftest elpy-project--find-root-should-find-same-directory ()
  (elpy-testcase ((:project project-root "foo.py"))
    (find-file (f-join project-root "foo.py"))
    (should (f-equal? (elpy-project--find-root)
                      project-root))))
