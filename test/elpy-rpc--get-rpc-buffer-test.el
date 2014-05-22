(ert-deftest elpy-rpc--get-rpc-buffer-should-return-cached-value ()
  (elpy-testcase ()
    (mletf* ((elpy-rpc--process-buffer-p (buffer) t))
      (setq elpy-rpc--buffer 'test-buffer)

      (should (equal (elpy-rpc--get-rpc-buffer)
                     'test-buffer)))))

(ert-deftest elpy-rpc--get-rpc-buffer-should-find-buffer-if-not-cached ()
  (elpy-testcase ()
    (mletf* ((elpy-rpc--process-buffer-p (buffer) nil)
             (elpy-project-root () 'project-root)
             (elpy-rpc--find-buffer (project-root python-command)
                                    'test-buffer))
      (setq elpy-rpc--buffer nil)

      (should (equal (elpy-rpc--get-rpc-buffer)
                     'test-buffer)))))

(ert-deftest elpy-rpc--get-rpc-buffer-should-open-buffer-if-not-found ()
  (elpy-testcase ()
    (mletf* ((elpy-rpc--process-buffer-p (buffer) nil)
             (elpy-project-root () 'project-root)
             (elpy-rpc--find-buffer (project-root python-command) nil)
             (elpy-rpc--open (project-root python-command) 'test-buffer))
      (setq elpy-rpc--buffer nil)

      (should (equal (elpy-rpc--get-rpc-buffer)
                     'test-buffer)))))
