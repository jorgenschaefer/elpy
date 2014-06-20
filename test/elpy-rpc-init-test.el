(ert-deftest elpy-rpc-init ()
  (elpy-testcase ()
    (mletf* ((called-args nil)
             (elpy-rpc (&rest args) (setq called-args args)))

      (elpy-rpc-init "backend" "library-root")

      (let ((library-root (expand-file-name "library-root")))
        (should (equal called-args
                       (list "init"
                             (vector `((backend . "backend")
                                       (project_root . ,library-root)))
                             nil nil)))))))
