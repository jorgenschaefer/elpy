(ert-deftest elpy-promise-set-resolved ()
  (elpy-testcase ()
    (let ((promise (elpy-promise "success" "error")))
      (should-not (elpy-promise-resolved-p promise))

      (elpy-promise-set-resolved promise)

      (should (elpy-promise-resolved-p promise)))))
