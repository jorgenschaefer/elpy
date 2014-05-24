(ert-deftest elpy-mode-should-fail-outside-of-python-mode ()
  (elpy-testcase ()
    (let ((major-mode 'not-python-mode))
      (should-error (elpy-mode)))))

(ert-deftest elpy-mode-should-run-buffer-init-on-start ()
  (elpy-testcase ()
    (python-mode)
    (mletf* ((buffer-init-called nil)
             (elpy-modules-run
              (command &rest args)
              (pcase command
                (`buffer-init (setq buffer-init-called t)))))

      (elpy-mode 1)

      (should buffer-init-called))))

(ert-deftest elpy-mode-should-run-buffer-stop-on-stop ()
  (elpy-testcase ()
    (python-mode)
    (mletf* ((buffer-stop-called nil)
             (elpy-modules-run
              (command &rest args)
              (pcase command
                (`buffer-stop (setq buffer-stop-called t)))))

      (elpy-mode -1)

      (should buffer-stop-called))))

(ert-deftest elpy-mode-should-run-global-init-if-not-done ()
  (elpy-testcase ()
    (mletf* ((global-init-run nil)
             (elpy-modules-run (command)
                               (when (eq command 'global-init)
                                 (setq global-init-run t))))

      (setq elpy-modules-initialized-p nil)
      (python-mode)
      (elpy-mode)

      (should global-init-run))))

(ert-deftest elpy-mode-should-run-global-init-only-once ()
  (elpy-testcase ()
    (mletf* ((global-init-run nil)
             (elpy-modules-run (command)
                               (when (eq command 'global-init)
                                 (setq global-init-run t))))

      (setq elpy-modules-initialized-p nil)
      (python-mode)

      (elpy-mode)
      (should global-init-run)
      (setq global-init-run nil)
      (elpy-mode)

      (should-not global-init-run))))
