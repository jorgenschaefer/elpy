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

(ert-deftest elpy-company-backend-should-never-require-match ()
  (elpy-testcase ()
    (should (equal (elpy-company-backend 'require-match)
                   'never))))

;; FIXME! candidates is a convoluted *mess*.

(ert-deftest elpy-company-backend-should-get-cached-meta ()
  (elpy-testcase ()
    (mletf* ((called-with nil)
             (elpy-company--cache-meta (arg)
                                       (setq called-with arg)
                                       "bar"))

      (should (equal "bar"
                     (elpy-company-backend 'meta "foo")))
      (should (equal "foo" called-with)))))

(ert-deftest elpy-company-backend-should-get-cached-annotation ()
  (elpy-testcase ()
    (mletf* ((called-with nil)
             (elpy-company--cache-annotation (arg)
                                             (setq called-with arg)
                                             "bar"))

      (should (equal "bar"
                     (elpy-company-backend 'annotation "foo")))
      (should (equal "foo" called-with)))))

(ert-deftest elpy-company-backend-should-get-cached-docs ()
  (elpy-testcase ()
    (mletf* ((called-with-origname nil)
             (elpy-company--cache-name (arg)
                                       (setq called-with-origname arg)
                                       "backend-name")
             (called-with-backendname nil)
             (elpy-rpc-get-completion-docstring (arg)
                                                (setq called-with-backendname
                                                      arg)
                                                "docstring")
             (called-with-doc nil)
             (company-doc-buffer (doc)
                                 (setq called-with-doc doc)))

      (elpy-company-backend 'doc-buffer "orig-name")

      (should (equal called-with-origname "orig-name"))
      (should (equal called-with-backendname "backend-name"))
      (should (equal called-with-doc "docstring")))))

(ert-deftest elpy-company-backend-should-get-cached-location ()
  (elpy-testcase ()
    (mletf* ((called-with-origname nil)
             (elpy-company--cache-name (arg)
                                       (setq called-with-origname arg)
                                       "backend-name")
             (called-with-backendname nil)
             (elpy-rpc-get-completion-location (arg)
                                               (setq called-with-backendname
                                                     arg)
                                               '("file" 23)))

      (should (equal '("file" . 23)
                     (elpy-company-backend 'location "orig-name")))
      (should (equal called-with-origname "orig-name"))
      (should (equal called-with-backendname "backend-name")))))
