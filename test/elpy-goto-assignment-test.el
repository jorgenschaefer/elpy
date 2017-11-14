(ert-deftest elpy-goto-assignment-should-pass-location ()
  (elpy-testcase ()
    (mletf* ((elpy-rpc-get-assignment
              ()
              '(test location))
             (goto-location-line nil)
             (goto-location-col nil)
             (elpy-goto-location
              (line column)
              (setq goto-location-line line
                    goto-location-col column)))

      (elpy-goto-assignment)

      (should (equal goto-location-line 'test))
      (should (equal goto-location-col 'location)))))

(ert-deftest elpy-goto-assignment-should-fail-for-missing-location ()
  (elpy-testcase ()
    (mletf* ((elpy-rpc-get-assignment
              ()
              nil))

      (should-error (elpy-goto-assignment)))))
