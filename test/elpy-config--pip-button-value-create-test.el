(ert-deftest elpy-config--pip-button-value-create-should-do-user-install ()
  (elpy-testcase ()
    (let* ((process-environment (cons '"VIRTUAL_ENV"
                                      process-environment))
           (pyvenv-virtual-env nil)
           (widget (widget-create 'elpy-insert--pip-button "test-module")))
      (should (string-match "--user" (widget-get widget :command)))
      (should (string-match "--user" (buffer-string))))))

(ert-deftest elpy-config--pip-button-value-create-should-use-pip ()
  (elpy-testcase ()
    (mocker-let ((executable-find
                  (command)
                  ((:input '("pip") :output t)
                   (:input '("easy_install") :output t :min-occur 0))))
      (let ((widget (widget-create 'elpy-insert--pip-button "test-module")))
        (should (string-match "pip" (widget-get widget :command)))
        (should (string-match "pip" (buffer-string)))))))

(ert-deftest elpy-config--pip-button-value-create-should-use-easy_install ()
  (elpy-testcase ()
    (mocker-let ((executable-find
                  (command)
                  ((:input '("pip") :output nil)
                   (:input '("easy_install") :output t))))
      (let ((widget (widget-create 'elpy-insert--pip-button "test-module")))
        (should (string-match "easy_install" (widget-get widget :command)))
        (should (string-match "easy_install" (buffer-string)))))))

(ert-deftest elpy-config--pip-button-value-create-should-error-without-pip-or-easy_install ()
  (elpy-testcase ()
    (mocker-let ((executable-find
                  (command)
                  ((:input '("pip") :output nil)
                   (:input '("easy_install") :output nil))))
      (should-error (widget-create 'elpy-insert--pip-button "test-module")))))
