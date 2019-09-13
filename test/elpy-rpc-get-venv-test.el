;;; -*-coding: utf-8-*-

(ert-deftest elpy-rpc-get-virtualenv-should-return-virtualenv ()
  (elpy-testcase ()
    (should (string-match "elpy/rpc-venv" (elpy-rpc-get-or-create-virtualenv)))))

(ert-deftest elpy-rpc-get-virtualenv-should-create-the-virtualenv-if-necessary ()
  (elpy-testcase ()
    (should (string-match "elpy/rpc-venv" (elpy-rpc-get-or-create-virtualenv)))
    (delete-directory (elpy-rpc-get-or-create-virtualenv) t nil)
    (should (string-match "elpy/rpc-venv" (elpy-rpc-get-or-create-virtualenv)))
    (should (file-exists-p (elpy-rpc-get-or-create-virtualenv)))))

(ert-deftest elpy-rpc-get-virtualenv-should-not-reinstall-the-virtualenv-every-time ()
  (elpy-testcase ()
   (elpy-rpc-get-or-create-virtualenv)
   (mletf* ((message (mess) (setq messages (concat messages mess)))
            (messages ""))
     (elpy-rpc-get-or-create-virtualenv)
     (should-not (string-match "lpy is creating the RPC virtualenv"
                               messages)))))

(ert-deftest elpy-rpc-get-virtualenv-should-not-perturbate-the-current-virtualenv ()
  (elpy-testcase ()
    (let ((old-venv pyvenv-virtual-env))
      (pyvenv-workon "elpy-test-venv")
      (elpy-rpc-get-or-create-virtualenv)
      (should (string= pyvenv-virtual-env-name "elpy-test-venv"))
      (pyvenv-workon old-venv))))

(ert-deftest elpy-rpc-get-virtualenv-should-update-the-virtualenv-when-rpc-command-change ()
  (elpy-testcase ()
   (let ((rpc-venv-path (elpy-rpc-get-or-create-virtualenv))
         (venv-python-path-command-file
          (concat (file-name-as-directory (elpy-rpc-get-virtualenv-path))
                  "elpy-rpc-python-path-command")))
     ;; Simulate a modification of `elpy-rpc-python-command' by modifying
     ;; the cookie file
     (with-temp-file venv-python-path-command-file
       (insert "Another python command")))
   (mletf* ((message (mess &rest rest)
                     (setq messages (concat messages (apply 'format mess rest))))
            (messages ""))
     (elpy-rpc-get-or-create-virtualenv)
     (should (string-match "lpy is \\(creating\\|updating\\) the RPC virtualenv" messages)))))

(ert-deftest elpy-rpc-get-virtualenv-should-NOT-update-the-virtualenv-when-it-is-not-the-default-venv ()
  (elpy-testcase ()
   (let ((elpy-rpc-virtualenv-path (concat
                                    (file-name-as-directory (pyvenv-workon-home))
                                    "elpy-test-venv")))
   (mletf* ((message (mess) (setq messages (concat messages mess)))
            (messages ""))
     (elpy-rpc-get-or-create-virtualenv)
     (should-not (string-match "lpy is installing the RPC virtualenv" messages))))))

(ert-deftest elpy-rpc-get-virtualenv-should-ask-before-creating-venvs ()
  (elpy-testcase ()
   (mletf* ((elpy-rpc-virtualenv-path "other-venv")
            (was-asked nil)
            (y-or-n-p (prompt) (setq was-asked t) nil))
     (elpy-rpc-get-or-create-virtualenv)
     (should was-asked))))
