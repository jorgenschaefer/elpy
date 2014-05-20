(ert-deftest elpy-check-should-fail-in-buffer-without-file ()
  (elpy-testcase ()
    (should-error (elpy-check))))

(ert-deftest elpy-check-should-call-save-some-buffers ()
  (elpy-testcase ((:project project-root "test.py"))
    (find-file (f-join project-root "test.py"))
    (mocker-let ((save-some-buffers
                  (arg pred)
                  ((:input '(nil nil)))))
      (elpy-check))))

(ert-deftest elpy-check-should-call-save-some-buffers ()
  (elpy-testcase ((:project project-root "test.py"))
    (find-file (f-join project-root "test.py"))
    (mocker-let ((save-some-buffers
                  (arg pred)
                  ((:input '(nil nil)))))
      (elpy-check))))

(ert-deftest elpy-check-should-call-compilation-start-for-single-file ()
  (elpy-testcase ((:project project-root "test.py"))
    (find-file (f-join project-root "test.py"))
    (cl-letf* ((command nil)
               ((symbol-function 'compilation-start)
                (lambda (command-arg mode name-function)
                  (setq command command-arg))))
      (elpy-check)
      (should (equal command (format "%s %s"
                                     python-check-command
                                     (f-join project-root "test.py")))))))

(ert-deftest elpy-check-should-pass-extra-args-for-project-check ()
  (elpy-testcase ((:project project-root "test.py"))
    (find-file (f-join project-root "test.py"))
    (cl-letf* ((command nil)
               (elpy-project-ignored-directories '("foo" "bar"))
               ((symbol-function 'compilation-start)
                (lambda (command-arg mode name-function)
                  (setq command command-arg))))
      (elpy-check t)
      (should (equal command (format "%s %s --exclude=foo,bar"
                                     python-check-command
                                     (format "%s/" project-root)))))))
