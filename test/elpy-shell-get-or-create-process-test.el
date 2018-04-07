(ert-deftest elpy-shell-get-or-create-process-should-return-process ()
  (elpy-testcase ()
    ;; This was left set by some other test, apparently, which breaks
    ;; in 24.1.
    (setq python-buffer nil)

    (let ((proc (elpy-shell-get-or-create-process)))
      (with-current-buffer (process-buffer proc)
        (should (eq major-mode 'inferior-python-mode))))))

(ert-deftest elpy-shell-get-or-create-process-should-fail-for-missing-binaries ()
  (elpy-testcase ()
    ;; test that it does not fail outside a virtual env
    (elpy-shell-get-or-create-process)
    (elpy-shell-kill)
    ;; Test that it fail for a virtualenv without the wanted interpreter
    (pyvenv-activate (expand-file-name "./dummy-folder"))
    (should-error (elpy-shell-get-or-create-process))
    (pyvenv-deactivate)))
