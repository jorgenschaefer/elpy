;;; -*-coding: utf-8-*-


(unless elpy-test-dont-use-virtualenv
  (ert-deftest elpy-rpc-get-virtualenv-path-should-return-default-path ()
    (elpy-testcase ()
      (let ((elpy-rpc-virtualenv-path 'default)
            (old-venv pyvenv-virtual-env))
        (should (string-match "elpy/rpc-venv" (elpy-rpc-get-virtualenv-path)))
        (pyvenv-workon "elpy-test-venv")
        (should (string-match "elpy/rpc-venv" (elpy-rpc-get-virtualenv-path)))
        (if old-venv
            (pyvenv-activate old-venv)
          (pyvenv-deactivate))))))

(unless elpy-test-dont-use-virtualenv
  (ert-deftest elpy-rpc-get-virtualenv-path-should-return-current-venv-path ()
    (elpy-testcase ()
      (let ((elpy-rpc-virtualenv-path 'current)
            (old-venv pyvenv-virtual-env))
        (should-not (string-match "elpy-test-venv"
                                  (elpy-rpc-get-virtualenv-path)))
        (pyvenv-workon "elpy-test-venv")
        (should (string-match "elpy-test-venv" (elpy-rpc-get-virtualenv-path)))
        (if old-venv
            (pyvenv-activate old-venv)
          (pyvenv-deactivate))))))

(ert-deftest elpy-rpc-get-virtualenv-path-should-return-global-venv-path ()
  (elpy-testcase ()
    (let ((elpy-rpc-virtualenv-path 'system)
          (old-venv pyvenv-virtual-env))
      (pyvenv-workon "elpy-test-venv")
      (should-not (string-match "elpy-test-venv"
                            (elpy-rpc-get-virtualenv-path)))
      (if old-venv
          (pyvenv-activate old-venv)
        (pyvenv-deactivate)))))

(unless elpy-test-dont-use-virtualenv
 (ert-deftest elpy-rpc-get-virtualenv-path-should-return-custom-venv ()
   (elpy-testcase ()
     (let ((elpy-rpc-virtualenv-path "elpy-test-venv"))
       (should (string-match "elpy-test-venv" (elpy-rpc-get-virtualenv-path)))))))

(unless elpy-test-dont-use-virtualenv
 (ert-deftest elpy-rpc-get-virtualenv-path-should-return-custom-venv-with-fun ()
   (elpy-testcase ()
     (let ((elpy-rpc-virtualenv-path (lambda () "elpy-test-venv")))
       (should (string-match "elpy-test-venv" (elpy-rpc-get-virtualenv-path)))))))
