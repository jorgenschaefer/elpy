(ert-deftest elpy-enable-should-fail-before-emacs-24 ()
  (elpy-testcase ()
    (let ((emacs-major-version 23))
      (should-error (elpy-enable)))))

(ert-deftest elpy-enable-should-fail-with-wrong-python-mode ()
  (elpy-testcase ()
    (mocker-let ((find-lisp-object-file-name
                  (object type)
                  ((:input '(python-mode symbol-function)
                           :output "/some/path/python-mode.el"))))
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
    (mocker-let ((elpy-modules-run
                  (command &rest args)
                  ((:input '(global-init)))))
      (elpy-enable))))

(ert-deftest elpy-enable-should-skip-global-init-with-argument ()
  (elpy-testcase ()
    (mocker-let ((elpy-modules-run
                  (command &rest args)
                  ((:input '(global-init)
                           :min-occur 0
                           :max-occur 0))))
      (elpy-enable t))))
