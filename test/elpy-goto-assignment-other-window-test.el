(ert-deftest elpy-goto-assignment-other-window-should-open-other-window ()
  (elpy-testcase ()
                 (mletf* ((elpy-rpc-get-assignment
                           ()
                           '(test location t))
                          (elpy-goto-location
                           (line column window)
                           (if window
                               (split-window-below))))

                         (elpy-goto-assignment-other-window)

                         (should (= (count-windows) 2)))))


(ert-deftest elpy-goto-assignment-other-window-should-fail-for-missing-location ()
  (elpy-testcase ()
                 (mletf* ((elpy-rpc-get-assignment
                           ()
                           nil))

                         (should-error (elpy-goto-assignment-other-window)))))
