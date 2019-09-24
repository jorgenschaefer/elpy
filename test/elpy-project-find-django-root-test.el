(ert-deftest elpy-project-find-django-root-should-find-setup-py ()
  (elpy-testcase ((:project project-root
                            "django-admin.py"
                            "foo/bar.py"))
    (find-file (f-join project-root "foo/bar.py"))
    (should (f-equal? (elpy-project-find-django-root)
                      project-root))))

(ert-deftest elpy-project-find-django-root-should-find-setup-cfg ()
  (elpy-testcase ((:project project-root
                            "manage.py"
                            "foo/bar.cfg"))
    (find-file (f-join project-root "foo/bar.cfg"))
    (should (f-equal? (elpy-project-find-django-root)
                      project-root))))
