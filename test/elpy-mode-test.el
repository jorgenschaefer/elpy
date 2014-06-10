(ert-deftest elpy-mode-should-fail-outside-of-python-mode ()
  (elpy-testcase ()
    (let ((major-mode 'not-python-mode))
      (should-error (elpy-mode)))))

(ert-deftest elpy-mode-should-run-buffer-init-on-start ()
  (elpy-testcase ()
    (python-mode)
    (mletf* ((buffer-init-called nil)
             (elpy-modules-buffer-init
              ()
              (setq buffer-init-called t)))

      (elpy-mode 1)

      (should buffer-init-called))))

(ert-deftest elpy-mode-should-run-buffer-stop-on-stop ()
  (elpy-testcase ()
    (python-mode)
    (mletf* ((buffer-stop-called nil)
             (elpy-modules-buffer-stop
              ()
              (setq buffer-stop-called t)))

      (elpy-mode -1)

      (should buffer-stop-called))))
