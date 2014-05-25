(ert-deftest elpy-rpc--handle-json-should-fail-for-unknown-call-id ()
  (elpy-testcase ()
    (should-error (elpy-rpc--handle-json '((id . 0)
                                           (result . 5))))))

(ert-deftest elpy-rpc--handle-json-should-call-success-in-right-buffer ()
  (elpy-testcase ()
    (let ((buf (generate-new-buffer "*Test*"))
          (called-in nil)
          (called-with nil))
      (setq elpy-rpc--buffer-p t)
      (elpy-rpc--register-callback
       5
       (with-current-buffer buf
         (elpy-promise (lambda (arg)
                         (setq called-in (current-buffer)
                               called-with arg)))))

      (elpy-rpc--handle-json '((id . 5)
                               (result . 23)))

      (should (equal called-in buf))
      (should (equal called-with 23)))))

(ert-deftest elpy-rpc--handle-json-should-call-error-in-right-buffer ()
  (elpy-testcase ()
    (let ((buf (generate-new-buffer "*Test*"))
          (called-in nil)
          (called-with nil))
      (setq elpy-rpc--buffer-p t)
      (elpy-rpc--register-callback
       5
       (with-current-buffer buf
         (elpy-promise nil
                       (lambda (arg)
                         (setq called-in (current-buffer)
                               called-with arg)))))

      (elpy-rpc--handle-json '((id . 5)
                               (error . 23)))

      (should (equal called-in buf))
      (should (equal called-with 23)))))
