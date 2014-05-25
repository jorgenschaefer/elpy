(ert-deftest elpy-promise-should-return-promise ()
  (elpy-testcase ()
    (should (elpy-promise-p (elpy-promise 'ding)))))

(ert-deftest elpy-promise-should-accept-error-callback ()
  (elpy-testcase ()
    (should (elpy-promise-p (elpy-promise 'ding 'dong)))))
