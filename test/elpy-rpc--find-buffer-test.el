(ert-deftest elpy-rpc--find-buffer-should-find-correct-buffer ()
  (elpy-testcase ()
    (mletf* ((buffer (current-buffer))
             (elpy-rpc--process-buffer-p (buf) (equal buf buffer)))
      (setq elpy-rpc--backend-library-root "test-project"
            elpy-rpc--backend-python-command (executable-find "python"))
      (should (equal (elpy-rpc--find-buffer "test-project"
                                            "python")
                     (current-buffer))))))

(ert-deftest elpy-rpc--find-buffer-should-not-find-without-process-buffer ()
  (elpy-testcase ()
    (mletf* ((buffer (current-buffer))
             (elpy-rpc--process-buffer-p (buf) nil))
      (setq elpy-rpc--backend-library-root "test-project"
            elpy-rpc--backend-python-command (executable-find "python"))

      (should (equal (elpy-rpc--find-buffer "test-project"
                                            "python")
                     nil)))))

(ert-deftest elpy-rpc--find-buffer-should-not-find-with-bad-project-root ()
  (elpy-testcase ()
    (mletf* ((buffer (current-buffer))
             (elpy-rpc--process-buffer-p (buf) t))
      (setq elpy-rpc--backend-library-root "bad-test-project"
            elpy-rpc--backend-python-command (executable-find "python"))

      (should (equal (elpy-rpc--find-buffer "test-project"
                                            "python")
                     nil)))))

(ert-deftest elpy-rpc--find-buffer-should-not-find-without-bad-python-command ()
  (elpy-testcase ()
    (mletf* ((buffer (current-buffer))
             (elpy-rpc--process-buffer-p (buf) t))
      (setq elpy-rpc--backend-library-root "test-project"
            elpy-rpc--backend-python-command "bad-test-python-command")

      (should (equal (elpy-rpc--find-buffer "test-project"
                                            "test-python-command")
                     nil)))))
