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

(ert-deftest elpy-enable-should-run-global-init ()
  (elpy-testcase ()
    (mletf* ((global-init-called nil)
             (elpy-modules-run
              (command &rest args)
              (pcase command
                (`global-init (setq global-init-called t)))))

      (setq elpy-modules-initialized-p nil)

      (elpy-enable)

      (should global-init-called))))
