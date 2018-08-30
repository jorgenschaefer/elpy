(if (version< "25.1" emacs-version)
    ((fset 'projectile-project-root (lambda (&rest ignored) nil))

     (ert-deftest elpy-project-find-projectile-root-should-return-root ()
       (elpy-testcase ()
                      (mletf* ((projectile-project-root () "/tmp"))
                              (should (f-equal? (elpy-project-find-projectile-root)
                                                "/tmp")))))
     
     (ert-deftest elpy-project-find-projectile-root-should-ignore-errors ()
       (elpy-testcase ()
                      (mletf* ((projectile-project-root () (error "No root")))
                              (should-not (elpy-project-find-projectile-root)))))))
