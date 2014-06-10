(ert-deftest elpy-modules-global-init-should-run-global-init ()
  (elpy-testcase ()
    (mletf* ((global-init-called nil)
             (elpy-modules-run
              (command &rest args)
              (pcase command
                (`global-init (setq global-init-called t)))))

      (setq elpy-modules-initialized-p nil)

      (elpy-modules-global-init)

      (should global-init-called))))
