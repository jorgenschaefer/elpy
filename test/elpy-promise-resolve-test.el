(ert-deftest elpy-promise-resolve-should-call-success-function ()
  (elpy-testcase ()
    (let* ((success nil)
           (err nil)
           (promise (elpy-promise (lambda (arg)
                                    (setq success arg))
                                  (lambda (arg)
                                    (setq err arg)))))
      (should-not success)
      (should-not err)

      (elpy-promise-resolve promise 'yes)

      (should (equal success 'yes))
      (should-not err))))

(ert-deftest elpy-promise-resolve-should-not-fail-for-no-success-function ()
  (elpy-testcase ()
    (elpy-promise-resolve (elpy-promise nil) "arg")))

(ert-deftest elpy-promise-resolve-should-restore-buffer ()
  (elpy-testcase ()
    (let* ((buf (get-buffer-create "*test-buf*"))
           (called-buf nil)
           (promise (with-current-buffer buf
                      (elpy-promise (lambda (arg)
                                      (setq called-buf (current-buffer)))))))

      (elpy-promise-resolve promise "arg")

      (should (equal called-buf buf)))))

(ert-deftest elpy-promise-resolve-should-call-error-on-dead-buffer ()
  (elpy-testcase ()
    (let* ((buf (get-buffer-create "*test-buf*"))
           (called-buf nil)
           (reason nil)
           (promise (with-current-buffer buf
                      (elpy-promise (lambda (arg)
                                      (setq called-buf (current-buffer)))
                                    (lambda (arg)
                                      (setq reason arg))))))
      (kill-buffer buf)

      (elpy-promise-resolve promise "arg")

      (should-not called-buf)
      (should (equal reason '(error "Selecting deleted buffer"))))))

(ert-deftest elpy-promise-resolve-should-only-run-once ()
  (elpy-testcase ()
    (let* ((runs 0)
           (promise (elpy-promise (lambda (arg)
                                    (setq runs (1+ runs))))))

      (elpy-promise-resolve promise t)
      (elpy-promise-resolve promise t)

      (should (equal runs 1)))))
