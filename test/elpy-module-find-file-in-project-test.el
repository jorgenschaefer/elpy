(ert-deftest elpy-module-find-file-in-project-global-init ()
  (elpy-testcase ()
    (mletf* ((required-feature nil)
             (require (feature) (setq required-feature feature)))

      (elpy-module-find-file-in-project 'global-init)

      (should (eq required-feature 'find-file-in-project)))))

(ert-deftest elpy-module-find-file-in-project-buffer-init ()
  (elpy-testcase ()
    (mletf* ((elpy-project-root () "/"))
      (setq buffer-file-name "test")

      (elpy-module-find-file-in-project 'buffer-init)

      (should (equal ffip-project-root "/")))))
