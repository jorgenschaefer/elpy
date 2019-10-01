;;; -*-coding: utf-8-*-


(ert-deftest elpy-rpc-get-virtualenv-path-should-return-default-path ()
  (elpy-testcase ()
    (let ((elpy-rpc-virtualenv-path 'default))
      (should (string-match "elpy/rpc-venv" (elpy-rpc-get-virtualenv-path)))
      (pyvenv-workon "elpy-test-venv")
      (should (string-match "elpy/rpc-venv" (elpy-rpc-get-virtualenv-path)))
      (pyvenv-deactivate))))

(ert-deftest elpy-rpc-get-virtualenv-path-should-return-current-venv-path ()
  (elpy-testcase ()
    (let ((elpy-rpc-virtualenv-path 'current))
      (should (string-match "\\(travis/virtualenv/python\\|.virtualenvs/elpy\\)"
                            (elpy-rpc-get-virtualenv-path)))
      (pyvenv-workon "elpy-test-venv")
      (should (string-match "elpy-test-venv" (elpy-rpc-get-virtualenv-path)))
      (pyvenv-deactivate))))

(ert-deftest elpy-rpc-get-virtualenv-path-should-return-global-venv-path ()
  (elpy-testcase ()
    (let ((elpy-rpc-virtualenv-path 'global))
      (should-not (string-match "\\(travis/virtualenv\\|.virtualenvs\\)"
                            (elpy-rpc-get-virtualenv-path)))
      (pyvenv-workon "elpy-test-venv")
      (should-not (string-match "\\(travis/virtualenv\\|.virtualenvs\\)"
                            (elpy-rpc-get-virtualenv-path)))
      (pyvenv-deactivate))))

(ert-deftest elpy-rpc-get-virtualenv-path-should-return-custom-venv ()
  (elpy-testcase ()
    (let ((elpy-rpc-virtualenv-path "elpy-test-venv"))
      (should (string-match "elpy-test-venv" (elpy-rpc-get-virtualenv-path))))))

(ert-deftest elpy-rpc-get-virtualenv-path-should-return-custom-venv-with-fun ()
  (elpy-testcase ()
    (let ((elpy-rpc-virtualenv-path (lambda () "elpy-test-venv")))
      (should (string-match "elpy-test-venv" (elpy-rpc-get-virtualenv-path))))))
