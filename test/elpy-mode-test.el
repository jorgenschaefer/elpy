(ert-deftest elpy-mode-should-fail-outside-of-python-mode ()
  (elpy-testcase ()
    (let ((major-mode 'not-python-mode))
      (should-error (elpy-mode)))))

(ert-deftest elpy-mode-should-fail-when-elpy-not-activated ()
  (elpy-testcase ()
    (python-mode)
    (should-error (elpy-mode))))

(ert-deftest elpy-mode-should-run-buffer-init-on-start ()
  (elpy-testcase ()
    (python-mode)
    (mletf* ((buffer-init-called nil)
             (elpy-enabled-p t)
             (elpy-modules-buffer-init
              ()
              (setq buffer-init-called t)))

      (elpy-mode 1)

      (should buffer-init-called))))

(ert-deftest elpy-mode-should-run-buffer-stop-on-stop ()
  (elpy-testcase ()
    (elpy-enable)
    (python-mode)
    (mletf* ((buffer-stop-called nil)
             (elpy-modules-buffer-stop
              ()
              (setq buffer-stop-called t)))

      (elpy-mode -1)

      (should buffer-stop-called))))


(ert-deftest elpy-mode-should-set-python-check-command ()
  (elpy-testcase ()
    (mletf* ((executable-find (name) (equal name "flake8")))
      (elpy-enable)
      (python-mode)
      (should (equal python-check-command "flake8"))
      (if (version<= "26.1" emacs-version)
          (should (equal python-flymake-command '("flake8" "-")))))))
