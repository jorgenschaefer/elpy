;;; -*-coding: utf-8-*-

(ert-deftest elpy-with-rpc-venv-activated-should-temporarily-activate-the-rpc-venv ()
  (elpy-testcase ()
    (let ((current-venv pyvenv-virtual-env-name))
    (with-elpy-rpc-venv-activated
     (should (string= pyvenv-virtual-env-name "elpy-rpc-venv")))
    (should (string= pyvenv-virtual-env-name current-venv)))))

(ert-deftest elpy-with-rpc-venv-activated-should-handle-errors ()
  (elpy-testcase ()
    (let ((current-venv pyvenv-virtual-env-name))
      (condition-case nil
          (with-elpy-rpc-venv-activated
           (error "nope"))
          (error t))
    (should (string= pyvenv-virtual-env-name current-venv)))))
