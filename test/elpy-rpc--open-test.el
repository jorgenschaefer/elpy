(ert-deftest elpy-rpc--open-should-set-local-variables ()
  (elpy-testcase ()
    (mletf* ((start-process (name buffer command &rest args) 'test-process)
             (requested-backend nil)
             (requested-library-root nil)
             (elpy-rpc-init (library-root env success)
                            (setq requested-library-root library-root))
             (exit-flag-disabled-for nil)
             (sentinel nil)
             (filter nil)
             (set-process-query-on-exit-flag
              (proc flag)
              (when (not flag)
                (setq exit-flag-disabled-for proc)))
             (set-process-sentinel
              (proc fun)
              (when (eq proc 'test-process)
                (setq sentinel fun)))
             (set-process-filter
              (proc fun)
              (when (eq proc 'test-process)
                (setq filter fun))))
      (with-current-buffer (elpy-rpc--open "/tmp" elpy-rpc-python-command)
        (should elpy-rpc--buffer-p)
        (should (equal requested-library-root "/tmp"))
        (should (equal elpy-rpc--buffer (current-buffer)))
        (should (equal elpy-rpc--backend-library-root "/tmp"))
        (should (equal elpy-rpc--backend-python-command
                       (with-elpy-rpc-venv-activated
                        (executable-find elpy-rpc-python-command))))
        (should (equal default-directory "/"))
        (should (equal exit-flag-disabled-for 'test-process))
        (should (equal sentinel 'elpy-rpc--sentinel))
        (should (equal filter 'elpy-rpc--filter))))))

(ert-deftest elpy-rpc--open-should-change-pythonpath ()
  (elpy-testcase ()
    (mletf* ((elpy-rpc--environment () "test-environment")
             (environment nil)
             (start-process (name buffer command &rest args)
                            (setq environment process-environment))
             (elpy-rpc-init (&rest ignored) nil)
             (set-process-query-on-exit-flag (proc flag) nil)
             (set-process-sentinel (proc fun) nil)
             (set-process-filter (proc fun) nil))

      (elpy-rpc--open "/tmp" elpy-rpc-python-command)

      (should (equal environment "test-environment")))))

(ert-deftest elpy-rpc--open-should-include-venv-name ()
  (elpy-testcase ()
    (let ((buf (elpy-rpc--open "/tmp" elpy-rpc-python-command)))
      (should (string-match (directory-file-name
                             (file-name-directory
                              (directory-file-name
                               (file-name-directory
                                (executable-find elpy-rpc-python-command)))))
                            (buffer-name buf)))
      (should
       (equal (buffer-local-value 'elpy-rpc--backend-python-command
                                  buf)
              (with-elpy-rpc-venv-activated
               (executable-find elpy-rpc-python-command)))))))

(ert-deftest elpy-rpc--open-should-open-in-a-dedicated-venv ()
  (elpy-testcase ()
    (elpy-rpc--get-rpc-buffer)
    (with-elpy-rpc-venv-activated
     (should (string= "elpy-rpc-venv" pyvenv-virtual-env-name)))))
