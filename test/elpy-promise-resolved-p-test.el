(ert-deftest elpy-promise-resolved-p-should-return-false-for-new-promise ()
  (elpy-testcase ()
    (should-not (elpy-promise-resolved-p (elpy-promise nil)))))

(ert-deftest elpy-promise-resolved-p-should-return-true-for-resolved-promise ()
  (elpy-testcase ()
    (let ((promise (elpy-promise nil)))
      (elpy-promise-resolve promise nil)

      (should (elpy-promise-resolved-p promise)))))

(ert-deftest elpy-promise-resolved-p-should-return-true-for-rejected-promise ()
  (elpy-testcase ()
    (let ((promise (elpy-promise nil)))
      (elpy-promise-reject promise nil)

      (should (elpy-promise-resolved-p promise)))))
