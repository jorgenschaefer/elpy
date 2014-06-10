(ert-deftest elpy-modules-global-stop-should-call-global-stop ()
  (elpy-testcase ()
    (mletf* ((global-stop-called nil)
             (elpy-modules-run
              (command &rest args)
              (pcase command
                (`global-stop (setq global-stop-called t)))))
      (elpy-enable)

      (elpy-modules-global-stop)

      (should global-stop-called))))
