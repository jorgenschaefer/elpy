(ert-deftest elpy-config--get-config-should-return-keys ()
  (elpy-testcase ()
    (let ((config (elpy-config--get-config)))
      (dolist (key '("emacs_version"
                     "python_rpc"
                     "python_rpc_version"
                     "python_rpc_executable"
                     "python_interactive"
                     "python_interactive_version"
                     "python_interactive_executable"
                     "elpy_version"
                     "jedi_version"
                     "jedi_latest"
                     "rope_version"
                     "rope_latest"
                     "virtual_env"
                     "virtual_env_short"))
        (should (not (eq :not-set (gethash key config :not-set))))))))

(ert-deftest elpy-config--get-config-should-set-pythonpath ()
  (elpy-testcase ()
    (mletf* ((elpy-rpc--environment () "test-environment")
             (environment nil)
             (call-process (&rest ignored)
                           (setq environment process-environment)))

      (elpy-config--get-config)

      (should (equal environment "test-environment")))))
