(ert-deftest elpy-modules-buffer-init-should-run-global-init-if-not-done ()
  (elpy-testcase ()
    (mletf* ((global-init-run nil)
             (elpy-modules-run (command)
                               (when (eq command 'global-init)
                                 (setq global-init-run t))))

      (setq elpy-modules-initialized-p nil)
      (python-mode)
      (elpy-modules-buffer-init)

      (should global-init-run))))

(ert-deftest elpy-modules-buffer-init-should-run-global-init-only-once ()
  (elpy-testcase ()
    (mletf* ((global-init-run nil)
             (elpy-modules-run (command)
                               (when (eq command 'global-init)
                                 (setq global-init-run t))))

      (setq elpy-modules-initialized-p nil)
      (python-mode)

      (elpy-modules-buffer-init)
      (should global-init-run)
      (setq global-init-run nil)
      (elpy-modules-buffer-init)

      (should-not global-init-run))))
