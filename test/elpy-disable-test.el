(ert-deftest elpy-disable-should-remove-elpy-from-python-mode ()
  (elpy-testcase ()
    (elpy-enable)
    (should (memq 'elpy-mode python-mode-hook))

    (elpy-disable)

    (should (not (memq 'elpy-mode python-mode-hook)))))

(ert-deftest elpy-disable-should-global-stop ()
  (elpy-testcase ()
    (mletf* ((global-stop-called nil)
             (elpy-modules-global-stop
              ()
              (setq global-stop-called t)))
      (elpy-enable)

      (elpy-disable)

      (should global-stop-called))))
