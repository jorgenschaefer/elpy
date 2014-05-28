;;; The interesting thing to test here is the interactive form. But
;;; then, interactive forms are tricky to test. So we don't. Which
;;; makes this test kind of ridiculous.

(ert-deftest elpy-set-test-runner ()
  (elpy-testcase ()
    (setq elpy-test-runner 'foo)
    (elpy-set-test-runner 'bar)
    (should (eq elpy-test-runner 'bar))))
