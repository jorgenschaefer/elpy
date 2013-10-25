(ert-deftest elpy-project-root-should-call-find-root-if-not-set ()
  (mocker-let ((elpy-project--find-root () ((:output "/opt/foo"))))
    (with-temp-buffer
      (should (f-equal? (elpy-project-root)
                        "/opt/foo")))))

(ert-deftest elpy-project-root-should-return-user-set-version ()
  (let ((elpy-project-root "/opt/bar"))
    (with-temp-buffer
      (should (f-equal? (elpy-project-root)
                        "/opt/bar")))))

(ert-deftest elpy-project-find-root-should-find-same-directory ()
  (with-elpy-file tmpdir "foo.py"
    (should (f-equal? (elpy-project--find-root)
                      tmpdir))))

(ert-deftest elpy-project-find-root-should-find-root-of-packages ()
  (with-elpy-file tmpdir "bar.py"
    (make-directory (format "%s/subdir" tmpdir))
    (write-region "" nil (format "%s/subdir/__init__.py" tmpdir))
    (let ((buf (find-file (format "%s/subdir/bar.py" tmpdir))))
      (unwind-protect
          (progn
            (python-mode)
            (elpy-mode 1)
            (should (f-equal? (elpy-project--find-root)
                              tmpdir)))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buf))))))
