(ert-deftest elpy-mode-should-fail-outside-of-python-mode ()
  (elpy-testcase ()
    (let ((major-mode 'not-python-mode))
      (should-error (elpy-mode)))))

(ert-deftest elpy-mode-should-run-buffer-init-on-start ()
  (elpy-testcase ()
    (python-mode)
    (mocker-let ((elpy-modules-run
                  (command &rest args)
                  ((:input '(buffer-init)))))
      (elpy-mode 1))))

(ert-deftest elpy-mode-should-run-buffer-stop-on-stop ()
  (elpy-testcase ()
    (python-mode)
    (mocker-let ((elpy-modules-run
                  (command &rest args)
                  ((:input '(buffer-stop)))))
      (elpy-mode -1))))
