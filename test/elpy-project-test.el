(ert-deftest elpy-project-root ()
  (with-elpy-file tmpdir "foo.py"
    (should (f-equal? (elpy-project-root)
                      tmpdir)))
  (with-elpy-file tmpdir "bar.py"
    (make-directory (format "%s/subdir" tmpdir))
    (write-region "" nil (format "%s/subdir/__init__.py" tmpdir))
    (let ((buf (find-file (format "%s/subdir/bar.py" tmpdir))))
      (unwind-protect
          (progn
            (python-mode)
            (elpy-mode 1)
            (should (f-equal? (elpy-project-root)
                              tmpdir)))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buf))))))
