;; This is deliberately short. The function is primarily concerned
;; with output. We will test its constituent functions later, and just
;; make sure it doesn't throw an error here.

(ert-deftest elpy-config-should-not-fail ()
  (elpy-testcase ()
    (elpy-config)))

(ert-deftest elpy-config-should-not-fail-without-virtualenv ()
  (elpy-testcase ()
    (let ((process-environment (cons "VIRTUAL_ENV" process-environment)))
      (elpy-config))))

(ert-deftest elpy-config-should-show-flake8-pip-button-when-no-syntax-checker-available ()
  (elpy-testcase ()
                 (mletf* ((elpy-syntax-check-command "/foo/bar/flake8"))
                         (elpy-enable)
                         (python-mode)
                         (elpy-config)
                         (let ((output (with-current-buffer  "*Elpy Config*"
                                         (elpy/wait-for-output "Options")
                                         (buffer-string))))
                           (should (equal elpy-syntax-check-command "/foo/bar/flake8"))
                           (should (string-match "Install flake8" output))))))
