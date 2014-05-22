(ert-deftest elpy-enable-should-fail-before-emacs-24 ()
  (elpy-testcase ()
    (let ((emacs-major-version 23))
      (should-error (elpy-enable)))))

(ert-deftest elpy-enable-should-fail-with-wrong-python-mode ()
  (elpy-testcase ()
    (mletf* ((find-lisp-object-file-name
              (object type)
              (pcase (list object type)
                (`(python-mode symbol-function)
                 "/some/path/python-mode.el"))))
      (should-error (elpy-enable)))))

(ert-deftest elpy-enable-should-add-elpy-to-python-mode ()
  (elpy-testcase ()
    (elpy-enable)

    (should (memq 'elpy-mode python-mode-hook))))

(ert-deftest elpy-enable-should-set-the-elpy-enable-variable ()
  (elpy-testcase ()
    (elpy-enable)

    (should (eq elpy-enable t))))

(ert-deftest elpy-enable-should-run-global-init ()
  (elpy-testcase ()
    (mletf* ((global-init-called nil)
             (elpy-modules-run
              (command &rest args)
              (pcase command
                (`global-init (setq global-init-called t)))))

      (elpy-enable)

      (should global-init-called))))

(ert-deftest elpy-enable-should-skip-global-init-with-argument ()
  (elpy-testcase ()
    (mletf* ((global-init-called nil)
             (elpy-modules-run
              (command &rest args)
              (pcase command
                (`global-init (setq global-init-called t)))))

      (elpy-enable t)

      (should-not global-init-called))))
