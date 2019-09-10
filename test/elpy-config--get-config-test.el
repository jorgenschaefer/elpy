(ert-deftest elpy-config--get-config-should-return-keys ()
  (elpy-testcase ()
    (let ((config (elpy-config--get-config)))
      (dolist (key '("emacs_version"
                     "elpy_version"
                     "virtual_env"
                     "virtual_env_short"
                     "python_interactive"
                     "python_interactive_version"
                     "python_interactive_executable"
                     "rpc_virtualenv"
                     "rpc_virtualenv_short"
                     "rpc_python"
                     "rpc_python_version"
                     "rpc_python_executable"
                     "jedi_version"
                     "jedi_latest"
                     "rope_version"
                     "rope_latest"
                     ))
        (should (not (eq :not-set (gethash key config :not-set))))))))

(ert-deftest elpy-config--get-config-should-set-pythonpath ()
  (elpy-testcase ()
    (mletf* ((elpy-rpc--environment () "test-environment")
             (environment nil)
             (call-process (&rest ignored)
                           (when (stringp process-environment)
                             (setq environment process-environment))))

      (elpy-config--get-config)

      (should (equal environment "test-environment")))))

(ert-deftest elpy-config--get-config-should-be-evaluated-in-the-rpc-virtualenv ()
  (elpy-testcase ()
    (should (string-match "elpy-rpc-venv/bin/python"
                          (gethash "rpc_python_executable"
                                   (elpy-config--get-config))))))
