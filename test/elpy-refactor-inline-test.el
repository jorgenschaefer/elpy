(unless (or (version< (elpy-rpc--get-python-version) "3.6" )
            (version< emacs-version "25.0")  ;; diffs are not parsed properly before emacs 25
            (string= (alist-get 'success (elpy-rpc-get-rename-diff "foo"))
                     "Not available"))

(ert-deftest elpy-refactor-rpc-inline-should-return-diff ()
  (elpy-testcase ((:project project-root "test.py"))
    (find-file (f-join project-root "test.py"))
    (insert (concat
             "foo = 3.1\n"
             "bar = foo + 1\n"
             "x = int(bar)\n"))
    (save-buffer)
    (goto-char 13)
    (let* ((diff (elpy-rpc-get-inline-diff))
           (proj-path (alist-get 'project_path diff))
           (success (alist-get 'success diff))
           (diff (alist-get 'diff diff)))
      (should success)
      (should (file-exists-p proj-path))
      (should (string= diff
                       "--- test.py
+++ test.py
@@ -1,4 +1,3 @@
 foo = 3.1
-bar = foo + 1
-x = int(bar)
+x = int(foo + 1)
")))))

(ert-deftest elpy-refactor-inline-should-inline-from-var-definition ()
  (elpy-testcase ((:project project-root "test.py"))
    (find-file (f-join project-root "test.py"))
    (insert (concat
             "foo = 3.1\n"
             "bar = foo + 1\n"
             "x = int(bar)\n"))
    (save-buffer)
    (goto-char 13)
    (elpy-refactor-inline)
    (should (string= (buffer-string)
                     "foo = 3.1
x = int(foo + 1)
"))))

(ert-deftest elpy-refactor-inline-should-inline-from-var-usage ()
  (elpy-testcase ((:project project-root "test.py"))
    (find-file (f-join project-root "test.py"))
    (insert (concat
             "foo = 3.1\n"
             "bar = foo + 1\n"
             "x = int(bar)\n"
             "y = 5\n"))
    (save-buffer)
    (goto-char 35)
    (elpy-refactor-inline)
    (should (string= (buffer-string)
                     "foo = 3.1
x = int(foo + 1)
y = 5
"))
    ))

(ert-deftest elpy-refactor-inline-should-abort-when-invalid-position ()
  (elpy-testcase ((:project project-root "test.py"))
    (find-file (f-join project-root "test.py"))
    (insert (concat
             "foo = 3.1\n"
             "bar = foo + 1\n"
             "x = int(bar)\n"))
    (save-buffer)
    (goto-char 28)
    (should-error (elpy-refactor-inline))))


)
