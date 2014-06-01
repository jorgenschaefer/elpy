(ert-deftest elpy-rpc--default-error-callback-warning-is-no-error ()
  (elpy-testcase ()
    (mletf* ((output nil)
             (message (fmt &rest args)
                      (setq output (apply #'format fmt args))))

      (elpy-rpc--default-error-callback '((name . "e-name")
                                          (message . "e-message")
                                          (warning . t)))

      (should (equal output "Elpy warning: e-message")))))

(ert-deftest elpy-rpc--default-error-callback-no-traceback-is-normal-error ()
  (elpy-testcase ()
    (should-error
     (elpy-rpc--default-error-callback '((name . "e-name")
                                         (message . "e-message"))))))

(ert-deftest elpy-rpc--default-error-callback-should-include-traceback ()
  (elpy-testcase ()
    (elpy-rpc--default-error-callback '((name . "e-name")
                                        (message . "e-message")
                                        (traceback . "e-traceback")))

    (with-current-buffer "*Elpy Error*"
      (goto-char (point-min))
      (re-search-forward "backend encountered an unexpected error")
      (re-search-forward "e-traceback"))))

(ert-deftest elpy-rpc--default-error-callback-should-handle-real-error ()
  (elpy-testcase ()
    (should-error (elpy-rpc--default-error-callback '(error "frob")))))
