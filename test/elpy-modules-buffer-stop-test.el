(ert-deftest elpy-modules-buffer-stop-should-run-buffer-stop ()
  (elpy-testcase ()
    (mletf* ((buffer-stop-run nil)
             (elpy-modules-run (command)
                               (when (eq command 'buffer-stop)
                                 (setq buffer-stop-run t))))

      (setq elpy-modules-initialized-p nil)
      (python-mode)
      (elpy-modules-buffer-stop)

      (should buffer-stop-run))))
