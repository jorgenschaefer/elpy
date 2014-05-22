(ert-deftest elpy-rpc--handle-unexpected-line-should-find-elpy-not-found ()
  (elpy-testcase ()
    (mletf* ((config-error nil)
             (elpy-config-error (err) (setq config-error err)))
      (insert "No module named elpy")

      (elpy-rpc--handle-unexpected-line)

      (should (equal config-error "Elpy module not found")))))

(ert-deftest elpy-rpc--handle-unexpected-line-should-display-general-error ()
  (elpy-testcase ()
    (mletf* ((config-error nil)
             (elpy-config-error (err) (setq config-error err)))
      (insert "The world is dark and cruel")

      (elpy-rpc--handle-unexpected-line)

      (with-current-buffer "*Elpy Error*"
        (re-search-forward "The world is dark and cruel")))))
