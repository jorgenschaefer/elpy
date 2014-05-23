(ert-deftest elpy-company-backend-should-be-interactive ()
  (elpy-testcase ()
    (require 'company)

    (mletf* ((called-backend nil)
             (company-begin-backend (backend) (setq called-backend backend)))

      (call-interactively 'elpy-company-backend)

      (should (eq called-backend 'elpy-company-backend)))))

(ert-deftest elpy-company-backend-should-find-no-prefix-without-elpy ()
  (elpy-testcase ()
    (elpy-module-company 'global-init)
    (should-not (elpy-company-backend 'prefix))))

(ert-deftest elpy-company-backend-should-find-no-prefix-in-string ()
  (elpy-testcase ()
    (elpy-modules-run 'global-init)
    (python-mode)
    (elpy-mode)
    (insert "# hello")
    (should-not (elpy-company-backend 'prefix))))

(ert-deftest elpy-company-backend-should-find-simple-prefix-string ()
  (elpy-testcase ()
    (elpy-modules-run 'global-init)
    (python-mode)
    (elpy-mode)
    (insert "hello")
    (should (equal (elpy-company-backend 'prefix)
                   "hello"))))

(ert-deftest elpy-company-backend-should-find-full-prefix-string ()
  (elpy-testcase ()
    (elpy-modules-run 'global-init)
    (python-mode)
    (elpy-mode)
    (insert "hello.world")
    (should (equal (elpy-company-backend 'prefix)
                   '("world" . t)))))

;; FIXME! candidates is a convoluted *mess*.

;; FIXME! Do the rest when we're re-doing the completion.
