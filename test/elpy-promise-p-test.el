(ert-deftest elpy-promise-p-should-find-promises ()
  (elpy-testcase ()
    (should (elpy-promise-p (elpy-promise 'ding)))
    (should-not (elpy-promise-p (vector '*elpy-promise* nil nil nil)))))
