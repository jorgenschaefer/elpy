;;; -*-coding: utf-8-*-

(ert-deftest elpy-rpc-get-venv-should-return-venv ()
  (elpy-testcase ()
    (should (string-match "elpy-rpc-venv" (elpy-rpc-get-or-create-venv)))))

(ert-deftest elpy-rpc-get-venv-should-create-the-venv-if-necessary ()
  (elpy-testcase ()
    (should (string-match "elpy-rpc-venv" (elpy-rpc-get-or-create-venv)))
    (delete-directory (elpy-rpc-get-or-create-venv) t nil)
    (should (string-match "elpy-rpc-venv" (elpy-rpc-get-or-create-venv)))
    (should (file-exists-p (elpy-rpc-get-or-create-venv)))))

(ert-deftest elpy-rpc-get-venv-should-not-perturbate-the-current-venv ()
  (elpy-testcase ()
    (let ((old-venv pyvenv-virtual-env))
      (pyvenv-workon "elpy-test-venv")
      (elpy-rpc-get-or-create-venv)
      (should (string= pyvenv-virtual-env-name "elpy-test-venv"))
      (pyvenv-workon old-venv))))
