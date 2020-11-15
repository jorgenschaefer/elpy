(unless (or (version< (elpy-rpc--get-python-version) "3.6" )
            (version< emacs-version "25.0")  ;; diffs are not parsed properly before emacs 25
            (string= (alist-get 'success (elpy-rpc-get-rename-diff "foo"))
                     "Not available"))

(ert-deftest elpy-refactor-rpc-extract-variable-should-return-diff ()
  (elpy-testcase ((:project project-root "test.py"))
    (find-file (f-join project-root "test.py"))
    (insert (concat
             "foo = 3.1\n"
             "x = int(foo + 1)"))
    (save-buffer)
    (let* ((diff (elpy-rpc-get-extract-variable-diff
                  "bar" 2 2 8 15))
           (proj-path (alist-get 'project_path diff))
           (success (alist-get 'success diff))
           (diff (alist-get 'diff diff)))
      (should success)
      (should (file-exists-p proj-path))
      (should (string= diff "--- test.py
+++ test.py
@@ -1,3 +1,4 @@
 foo = 3.1
-x = int(foo + 1)
+bar = foo + 1
+x = int(bar)
")))))

(ert-deftest elpy-refactor-extract-variable-should-extract-variable ()
  (elpy-testcase ((:project project-root "test.py"))
    (find-file (f-join project-root "test.py"))
    (insert (concat
             "foo = 3.1\n"
             "x = int(foo + 1)"))
    (save-buffer)
    (goto-char 20)
    (elpy-refactor-extract-variable "bar")
    (should (string= (buffer-string)
                     (concat
                      "foo = 3.1\n"
                      "bar = foo\n"
                      "x = int(bar + 1)\n")))))

(ert-deftest elpy-refactor-extract-variable-should-extract-variable-from-region ()
  (elpy-testcase ((:project project-root "test.py"))
    (find-file (f-join project-root "test.py"))
    (insert (concat
             "foo = 3.1\n"
             "x = int(foo + 1)"))
    (save-buffer)
    (elpy/mark-region 19 26)
    (elpy-refactor-extract-variable "bar")
    (should (string= (buffer-string)
                     (concat
                      "foo = 3.1\n"
                      "bar = foo + 1\n"
                      "x = int(bar)\n")))))

(ert-deftest elpy-refactor-extract-variable-should-abort-when-invalid-var-name ()
  (elpy-testcase ((:project project-root "test.py"))
    (find-file (f-join project-root "test.py"))
    (insert (concat
             "foo = 3.1\n"
             "x = int(foo + 1)"))
    (save-buffer)
    (elpy/mark-region 19 26)
    (should-error (elpy-refactor-rename "2Bar" t))
    (should-error (elpy-refactor-rename "128376" t))
    (should-error (elpy-refactor-rename "#var" t))))

(ert-deftest elpy-refactor-extract-variable-should-abort-when-invalid-selection ()
  (elpy-testcase ((:project project-root "test.py"))
    (find-file (f-join project-root "test.py"))
    (insert (concat
             "foo = 3.1\n"
             "x = int(foo + 1)"))
    (save-buffer)
    (elpy/mark-region 17 26)
    (should-error (elpy-refactor-rename "Bar" t))))

)
