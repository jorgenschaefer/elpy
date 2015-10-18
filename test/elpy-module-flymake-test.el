(ert-deftest elpy-module-flymake-global-init-should-use-flake8 ()
  (elpy-testcase ()
    (mletf* ((executable-find (name) (equal name "flake8")))
      (elpy-module-flymake 'global-init)

      (should (equal python-check-command "flake8")))))

(ert-deftest elpy-module-flymake-global-init-should-not-override-existing-checker ()
  (elpy-testcase ()
    (mletf* ((python-check-command "my-test-command")
             (executable-find (name) t))
      (elpy-module-flymake 'global-init)

      (should (equal python-check-command "my-test-command")))))

(ert-deftest elpy-module-flymake-global-init ()
  (elpy-testcase ()
    (elpy-module-flymake 'global-init)

    (should (member '("\\.py\\'" elpy-flymake-python-init)
                    flymake-allowed-file-name-masks))))

(ert-deftest elpy-module-flymake-buffer-init ()
  (elpy-testcase ((:project project-root
                            "test.py"))
    (find-file (f-join project-root "test.py"))
    (elpy-module-flymake 'buffer-init)

    ;; Too many things can prevent flymake from starting, like missing
    ;; flake8 and such. So don't even try.
    ;; (should flymake-mode)

    (should (equal flymake-no-changes-timeout 60))
    (should (equal flymake-start-syntax-check-on-newline nil))

    (should (equal "^W[0-9]"
                   (if (boundp 'flymake-warning-predicate)
                       flymake-warning-predicate
                     flymake-warning-re)))))

(ert-deftest elpy-module-flymake-buffer-stop ()
  (elpy-testcase ()
    (flymake-mode 1)

    (elpy-module-flymake 'buffer-stop)

    (should-not flymake-mode)))
