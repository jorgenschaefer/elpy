(ert-deftest elpy-test-run-should-set-default-directory ()
  (elpy-testcase ()
    (mletf* ((top nil)
             (compile (arg) (setq top default-directory)))

      (elpy-test-run "/test/dir/" "ls")

      (should (equal top "/test/dir/")))))

(ert-deftest elpy-test-run-should-run-command ()
  (elpy-testcase ()
    (mletf* ((command nil)
             (compile (arg) (setq command arg)))

      (elpy-test-run "/" "ls" "foo")

      (should (equal command "ls foo")))))

(ert-deftest elpy-test-run-should-escape-arguments ()
  (elpy-testcase ()
    (mletf* ((command nil)
             (compile (arg) (setq command arg)))

      (elpy-test-run "/" "ls" "foo bar")

      (should (equal command "ls foo\\ bar")))))
