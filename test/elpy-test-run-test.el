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

(ert-deftest elpy-test-run-should-escape-arguments-on-unix ()
  (elpy-testcase ()
    (mletf* ((command nil)
             (compile (arg) (setq command arg))
             (system-type 'gnu))

      (elpy-test-run "/" "ls" "foo bar")

      (should (equal command "ls foo\\ bar")))))

(ert-deftest elpy-test-run-should-not-escape-arguments-on-windows ()
  (elpy-testcase ()
    (mletf* ((command nil)
             (compile (arg) (setq command arg))
             (system-type 'windows-nt))

      (elpy-test-run "/" "ls" "foo bar")

      (should (equal command "ls foo bar")))))
