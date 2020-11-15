(unless (or (version< (elpy-rpc--get-python-version) "3.6" )
            (string= (alist-get 'success (elpy-rpc-get-rename-diff "foo"))
                     "Not available"))

(ert-deftest elpy-refactor-should-warn-about-invalid-symbols ()
  (elpy-testcase ((:project project-root "test.py"))
    (find-file (f-join project-root "test.py"))
    (insert (concat
             "foo = 3.1\n"
             "bar = foo + 1\n"
             "x = int(bar)\n"))
    (save-buffer)
    (goto-char 13)
    (should-error (elpy-refactor-rename "invalid-name"))
    (should-error (elpy-refactor-rename "invalid name"))
    (should-error (elpy-refactor-rename "0_invalid_name"))
    (should-error (elpy-refactor-rename "invalid%_name"))))

)
