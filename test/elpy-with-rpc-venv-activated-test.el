;;; -*-coding: utf-8-*-


(ert-deftest elpy-with-rpc-virtualenv-should-not-activate-venv-when-using-the-same-venv ()
  ;; when the current environment is the same as the RPC environment,
  ;; `with-elpy-rpc-virtualenv-activated` should not switch virtualenvs.
  (elpy-testcase ()
    (let ((elpy-rpc-virtualenv-path 'current)
          (venv-has-been-activated nil))
      (mletf* ((pyvenv-activate (&rest rest)
                                (setq venv-has-been-activated t)))
        (with-elpy-rpc-virtualenv-activated
         "pass")
        (should-not venv-has-been-activated)))))

(ert-deftest elpy-with-rpc-virtualenv-should-activate-venv-when-using-different-venv ()
  (elpy-testcase ()
    (let ((elpy-rpc-virtualenv-path 'default)
          (venv-has-been-activated nil))
      (mletf* ((pyvenv-activate (&rest rest)
                                (setq venv-has-been-activated t)))
        (with-elpy-rpc-virtualenv-activated
         "pass")
        (should venv-has-been-activated)))))

(when (not elpy-test-dont-use-virtualenv)
  (ert-deftest elpy-with-rpc-virtualenv-activated-should-temporarily-activate-the-rpc-virtualenv ()
    (elpy-testcase ()
      (let ((elpy-rpc-virtualenv-path 'default)
            (current-venv pyvenv-virtual-env-name))
      (with-elpy-rpc-virtualenv-activated
       (should (string= pyvenv-virtual-env-name "rpc-venv")))
      (should (string= pyvenv-virtual-env-name current-venv)))))

  (ert-deftest elpy-with-rpc-virtualenv-activated-should-handle-errors ()
    (elpy-testcase ()
      (let ((current-venv pyvenv-virtual-env-name))
        (condition-case nil
            (with-elpy-rpc-virtualenv-activated
             (error "nope"))
            (error t))
      (should (string= pyvenv-virtual-env-name current-venv))))))
