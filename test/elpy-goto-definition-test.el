(ert-deftest elpy-goto-definition-should-pass-location ()
  (elpy-testcase ()
    (mocker-let ((elpy-rpc-get-definition
                  ()
                  ((:output '(test location))))
                 (elpy-goto-location
                  (line column)
                  ((:input '(test location)))))
      (elpy-goto-definition))))

(ert-deftest elpy-goto-definition-should-fail-for-missing-location ()
  (elpy-testcase ()
    (mocker-let ((elpy-rpc-get-definition
                  ()
                  ((:output nil))))
      (should-error (elpy-goto-definition)))))
