(ert-deftest elpy-rpc--default-error-callback-should-handle-real-error ()
  (elpy-testcase ()
    (should-error (elpy-rpc--default-error-callback '(error "frob")))))

(ert-deftest elpy-rpc--default-error-callback-code-200-should-not-error ()
  (elpy-testcase ()
    (mletf* ((output nil)
             (message (fmt &rest args)
                      (setq output (apply #'format fmt args))))

      (elpy-rpc--default-error-callback '((message . "e-message")
                                          (code . 200)))

      (should (equal output "Elpy warning: e-message")))))

(ert-deftest elpy-rpc--default-error-callback-code-400-should-error ()
  (elpy-testcase ()
    (should-error
     (elpy-rpc--default-error-callback '((message . "e-message")
                                         (code . 400))))))

(ert-deftest elpy-rpc--default-error-callback-code-500-should-pop-up-window ()
  (elpy-testcase ()
    (let ((elpy-disable-backend-error-display nil))
      (elpy-rpc--default-error-callback
       '((message . "e-message")
         (code . 500)
         (data . ((traceback . "e-traceback\n"))))))

    (with-current-buffer "*Elpy Error*"
      (goto-char (point-min))
      (re-search-forward "backend encountered an unexpected error")
      (re-search-forward "e-traceback"))))
