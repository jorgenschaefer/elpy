(fset 'projectile-project-root (lambda (&rest ignored) nil))

(ert-deftest elpy-project-find-projectile-root-should-return-root ()
  (elpy-testcase ()
    (mletf* ((projectile-project-root () "/test/root"))
      (should (f-equal? (elpy-project-find-projectile-root)
                        "/test/root")))))

(ert-deftest elpy-project-find-projectile-root-should-ignore-errors ()
  (elpy-testcase ()
    (mletf* ((projectile-project-root () (error "No root")))
      (should-not (elpy-project-find-projectile-root)))))
