(ert-deftest elpy-insert--pip-button-action-should-run-command ()
  (elpy-testcase ()
    (mletf* ((widget-get (widget option)
                         (when (and (eq widget 'test-widget)
                                    (eq option :command))
                           "test-command"))
             (async-shell-command-called-with nil)
             (async-shell-command (command)
                                  (setq async-shell-command-called-with
                                        command)))
      (elpy-insert--pip-button-action 'test-widget)
      (should (equal async-shell-command-called-with
                     "test-command")))))
