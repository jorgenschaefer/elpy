(ert-deftest elpy-promise-reject-should-call-error-function ()
  (elpy-testcase ()
    (let* ((success nil)
           (err nil)
           (promise (elpy-promise (lambda (arg)
                                    (setq success arg))
                                  (lambda (arg)
                                    (setq err arg)))))
      (should-not success)
      (should-not err)

      (elpy-promise-reject promise 'fail)

      (should-not success)
      (should (equal err 'fail)))))

(ert-deftest elpy-promise-reject-should-not-fail-for-no-error-function ()
  (elpy-testcase ()
    (elpy-promise-reject (elpy-promise nil nil) "reason")))

(ert-deftest elpy-promise-reject-should-restore-buffer ()
  (elpy-testcase ()
    (let* ((buf (get-buffer-create "*test-buf*"))
           (called-buf nil)
           (promise (with-current-buffer buf
                      (elpy-promise nil
                                    (lambda (arg)
                                      (setq called-buf (current-buffer)))))))

      (elpy-promise-reject promise "arg")

      (should (equal called-buf buf)))))

(ert-deftest elpy-promise-reject-should-not-fail-on-dead-buffer ()
  (elpy-testcase ()
    (let* ((buf (get-buffer-create "*test-buf*"))
           (reason nil)
           (promise (with-current-buffer buf
                      (elpy-promise nil
                                    (lambda (arg)
                                      (setq reason arg))))))
      (kill-buffer buf)

      (elpy-promise-reject promise "arg")

      (should (equal reason "arg")))))

(ert-deftest elpy-promise-reject-should-only-run-once ()
  (elpy-testcase ()
    (let* ((runs 0)
           (promise (elpy-promise nil
                                  (lambda (arg)
                                    (setq runs (1+ runs))))))

      (elpy-promise-reject promise t)
      (elpy-promise-reject promise t)

      (should (equal runs 1)))))

(ert-deftest elpy-promise-reject-should-only-run-once-if-fails ()
  (elpy-testcase ()
    (let* ((runs 0)
           (promise (elpy-promise nil
                                  (lambda (arg)
                                    (setq runs (1+ runs))
                                    (/ 0 0)))))

      (should-error (elpy-promise-reject promise t))
      (elpy-promise-reject promise t)

      (should (equal runs 1)))))
