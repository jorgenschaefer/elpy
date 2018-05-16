(ert-deftest elpy-shell-get-or-create-process-should-return-process ()
  (elpy-testcase ()
    ;; This was left set by some other test, apparently, which breaks
    ;; in 24.1.
    (setq python-buffer nil)

    (let ((proc (elpy-shell-get-or-create-process)))
      (with-current-buffer (process-buffer proc)
        (should (eq major-mode 'inferior-python-mode))))))
