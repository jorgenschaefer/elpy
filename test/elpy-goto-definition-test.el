(ert-deftest elpy-goto-definition-should-pass-location ()
  (elpy-testcase ()
    (mletf* ((elpy-rpc-get-definition
              ()
              '(test location))
             (goto-location-line nil)
             (goto-location-col nil)
             (elpy-goto-location
              (line column)
              (setq goto-location-line line
                    goto-location-col column)))

      (elpy-goto-definition)

      (should (equal goto-location-line 'test))
      (should (equal goto-location-col 'location)))))

(ert-deftest elpy-goto-definition-should-fail-for-missing-location ()
  (elpy-testcase ()
    (mletf* ((elpy-rpc-get-definition
              ()
              nil))

      (should-error (elpy-goto-definition)))))
