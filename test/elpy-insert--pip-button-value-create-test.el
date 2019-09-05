(ert-deftest elpy-insert--pip-button-value-create-should-use-pip ()
  (elpy-testcase ()
    (mletf* ((executable-find
              (command)
              (cond
               ((equal command "easy_install") t)))
             (call-process
              (program &optional infile destination display &rest args)
              0)
             (widget (widget-create 'elpy-insert--pip-button
                                    :package "test-module")))
      (should (string-match "python.*\ -m pip" (widget-get widget :command)))
      (should (string-match "Install test-module" (buffer-string))))))

(ert-deftest elpy-insert--pip-button-value-create-should-use-easy_install ()
  (elpy-testcase ()
    (mletf* ((executable-find
              (command)
              (cond
               ((equal command "pip") nil)
               (t t)))
             (call-process
              (program &optional infile destination display &rest args)
              1)
             (widget (widget-create 'elpy-insert--pip-button
                                    :package "test-module")))

      (should (string-match "easy_install" (widget-get widget :command)))
      (should (string-match "Install test-module" (buffer-string))))))

(ert-deftest elpy-insert--pip-button-value-create-should-error-without-pip-or-easy_install ()
  (elpy-testcase ()
    (mletf* ((executable-find
              (command)
              nil)
             (call-process
              (program &optional infile destination display &rest args)
              1))
      (should-error (widget-create 'elpy-insert--pip-button "test-module")))))
