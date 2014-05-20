(ert-deftest elpy-config--get-config ()
  (elpy-testcase ()
    (let ((config (elpy-config--get-config)))
      (dolist (key '("emacs_version"
                     "python_rpc"
                     "python_rpc_executable"
                     "python_interactive"
                     "python_interactive_executable"
                     "python_version"
                     "elpy_version"
                     "jedi_version"
                     "rope_version"
                     "virtual_env"
                     "virtual_env_short"))
        (should (not (eq :not-set (gethash key config :not-set))))))))
