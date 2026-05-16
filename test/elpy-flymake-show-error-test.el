(ert-deftest elpy-flymake-show-error ()
  (elpy-testcase ()
       (mletf* ((flymake-diagnostics
                 (point)
                 (list (flymake-make-diagnostic (current-buffer)
                        23 56 :error "test-error-1")
                       (flymake-make-diagnostic (current-buffer)
                        62 28 :error "test-error-2")))
                 (output nil)
                 (message (fmt &rest args)
                          (setq output (apply 'format fmt args))))

        (elpy-flymake-show-error)

        (should (equal output "test-error-1, test-error-2")))))
