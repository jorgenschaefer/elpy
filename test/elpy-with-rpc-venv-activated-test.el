;;; -*-coding: utf-8-*-

(ert-deftest elpy-with-rpc-virtualenv-activated-should-temporarily-activate-the-rpc-virtualenv ()
  (elpy-testcase ()
    (let ((current-venv pyvenv-virtual-env-name))
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
    (should (string= pyvenv-virtual-env-name current-venv)))))
