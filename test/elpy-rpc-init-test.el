(ert-deftest elpy-rpc-init ()
  (elpy-testcase ()
    (mletf* ((called-args nil)
             (elpy-rpc (&rest args) (setq called-args args)))

      (elpy-rpc-init "library-root" "env_path")

      (let ((library-root (expand-file-name "library-root"))
            (env_path (expand-file-name "env_path")))
        (should (equal called-args
                       (list "init"
                             (vector `((project_root . ,library-root)
                                       (environment . ,env_path)))
                             nil nil)))))))
