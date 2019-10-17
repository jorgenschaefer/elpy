;;; -*-coding: utf-8-*-

(unless elpy-test-dont-use-virtualenv
  (ert-deftest elpy-rpc-reinstall-virtualenv-should-reinstall ()
    (elpy-testcase ()
      (let* ((elpy-rpc-virtualenv-path 'default)
            (rpc-venv-path (elpy-rpc-get-or-create-virtualenv)))
        (mletf* ((messages "")
                 (y-or-n-p (question)
                           (setq messages (concat messages question))
                           t))
          (with-temp-file (concat (file-name-as-directory rpc-venv-path)
                                  "additional-file")
            (insert "nothing here"))
          (elpy-rpc-reinstall-virtualenv)
          (should-not (file-exists-p (concat
                                      (file-name-as-directory rpc-venv-path)
                                      "additional-file")))
          (should (string= messages "Automatically install the RPC dependencies from PyPI (needed for completion, autoformatting and documentation) ? ")))))))

(unless elpy-test-dont-use-virtualenv
  (ert-deftest elpy-rpc-reinstall-virtualenv-should-reinstall-with-global-venv ()
    (elpy-testcase ()
      (let* ((elpy-rpc-virtualenv-path 'system)
            (rpc-venv-path (elpy-rpc-get-or-create-virtualenv)))
        (should-error (elpy-rpc-reinstall-virtualenv))))))

(unless elpy-test-dont-use-virtualenv
  (ert-deftest elpy-rpc-reinstall-virtualenv-should-reinstall-with-custom-venv ()
    (elpy-testcase ()
      (let* ((elpy-rpc-virtualenv-path (concat
                                      (file-name-as-directory
                                       (pyvenv-workon-home))
                                      "elpy-test-venv"))
            (rpc-venv-path (elpy-rpc-get-or-create-virtualenv)))
        (mletf* ((messages "")
                 (y-or-n-p (question)
                           (setq messages (concat messages question))
                           t))
          (with-temp-file (concat (file-name-as-directory rpc-venv-path)
                                  "additional-file")
            (insert "nothing here"))
          (elpy-rpc-reinstall-virtualenv)
          (should-not (file-exists-p (concat
                                      (file-name-as-directory rpc-venv-path)
                                      "additional-file")))
          (should (= 0 (string-match "Are you sure you want to reinstall the virtualenv in '[^']*' (every manual modifications will be lost) \\? `elpy-rpc-virtualenv-path' was set to '[^']*', but this virtualenv does not exist, create it \\? Automatically install the RPC dependencies from PyPI (needed for completion, autoformatting and documentation) \\? " messages))))))))
