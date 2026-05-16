(ert-deftest elpy-module-flymake-global-init ()
  (elpy-testcase ()
    (elpy-module-flymake 'global-init)))

(ert-deftest elpy-module-flymake-buffer-init ()
  (elpy-testcase ((:project project-root
                            "test.py"))
    (find-file (f-join project-root "test.py"))
    (elpy-module-flymake 'buffer-init)

    ;; Too many things can prevent flymake from starting, like missing
    ;; flake8 and such. So don't even try.
    ;; (should flymake-mode)

    (should (equal flymake-no-changes-timeout 60))
    (should (equal flymake-start-syntax-check-on-newline nil))))

(ert-deftest elpy-module-flymake-buffer-stop ()
  (elpy-testcase ()
    (flymake-mode 1)

    (elpy-module-flymake 'buffer-stop)

    (should-not flymake-mode)))
