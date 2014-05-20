(ert-deftest elpy-config--pip-button-action-should-run-command ()
  (elpy-testcase ()
    (mocker-let ((widget-get
                  (widget option)
                  ((:input '(test-widget :command) :output "test-command")))
                 (async-shell-command
                  (command)
                  ((:input '("test-command")))))
      (elpy-config--pip-button-action 'test-widget))))
