(ert-deftest elpy-promise-wait-should-return-early-for-resolved-promise ()
  (elpy-testcase ()
    (let ((promise (elpy-promise nil)))
      (elpy-promise-resolve promise t)

      (elpy-promise-wait promise))))

(ert-deftest elpy-promise-wait-should-return-after-timeout ()
  (elpy-testcase ()
    (elpy-promise-wait (elpy-promise nil) 0.1)))

(ert-deftest elpy-promise-wait-should-return-eventually-for-resolved-promise ()
  (elpy-testcase ()
    (let ((start-time (current-time))
          (promise (elpy-promise nil)))

      (run-at-time 0.1 nil 'elpy-promise-resolve promise t)
      (elpy-promise-wait promise 5)

      (should (elpy-promise-resolved-p promise)))))
