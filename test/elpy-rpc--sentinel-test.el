(ert-deftest elpy-rpc--sentinel-should-not-fail-on-dead-buffer ()
  (elpy-testcase ()
    (mletf* ((buf (get-buffer-create "*temp*"))
             (process-buffer (proc) buf))
      (kill-buffer buf)
      (elpy-rpc--sentinel 'process "killed"))))

(ert-deftest elpy-rpc--sentinel-should-call-error-handlers ()
  (elpy-testcase ()
    (mletf* ((buf (get-buffer-create "*temp*"))
             (process-buffer (proc) buf)
             (error-called nil))
      (with-current-buffer buf
        (setq elpy-rpc--buffer-p t)
        (elpy-rpc--register-callback
         0
         (elpy-promise (lambda (arg) nil)
                       (lambda (error-obj) (setq error-called error-obj)))))

      (elpy-rpc--sentinel 'process "killed\n")

      (should (equal error-called '(process-sentinel "killed"))))))

(ert-deftest elpy-rpc--sentinel-should-call-error-handler-in-correct-buffer ()
  (elpy-testcase ()
    (mletf* ((orig-buf (current-buffer))
             (buf (get-buffer-create "*temp*"))
             (process-buffer (proc) buf)
             (error-called-in nil)
             (promise (elpy-promise (lambda (arg) nil)
                                    (lambda (error-obj)
                                      (setq error-called-in
                                            (current-buffer))))))
      (with-current-buffer buf
        (setq elpy-rpc--buffer-p t)
        (elpy-rpc--register-callback 0 promise)

        (elpy-rpc--sentinel 'process "killed\n")

        (should (equal error-called-in orig-buf))))))
