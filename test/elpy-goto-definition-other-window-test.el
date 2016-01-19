(ert-deftest elpy-goto-definition-other-window-should-open-other-window ()
  (elpy-testcase ()
                 (mletf* ((elpy-rpc-get-definition
                           ()
                           '(test location t))
                          (elpy-goto-location
                           (line column window)
                           (if window
                               (split-window-below))))

                         (elpy-goto-definition-other-window)

                         (should (= (count-windows) 2)))))


(ert-deftest elpy-goto-definition-other-window-should-fail-for-missing-location ()
  (elpy-testcase ()
                 (mletf* ((elpy-rpc-get-definition
                           ()
                           nil))

                         (should-error (elpy-goto-definition-other-window)))))
