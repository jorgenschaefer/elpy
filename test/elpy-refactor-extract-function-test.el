(unless (or (version< (elpy-rpc--get-python-version) "3.6" )
            (version< emacs-version "25.0")  ;; diffs are not parsed properly before emacs 25
            (string= (alist-get 'success (elpy-rpc-get-rename-diff "foo"))
                     "Not available"))


(ert-deftest elpy-refactor-rpc-extract-function-should-return-diff ()
  (elpy-testcase ((:project project-root "test.py"))
    (find-file (f-join project-root "test.py"))
    (insert (concat
             "global_var = 3\n"
             "\n"
             "def x():\n"
             "   foo = 3.1\n"
             "   x = int(foo + 1 + global_var)\n"))
    (save-buffer)
    (let* ((diff (elpy-rpc-get-extract-function-diff
                  "bar" 5 5 11 30))
           (proj-path (alist-get 'project_path diff))
           (success (alist-get 'success diff))
           (diff (alist-get 'diff diff)))
      (should success)
      (should (file-exists-p proj-path))
      (should (string= diff
                       (concat
                       "--- test.py\n"
                       "+++ test.py\n"
                       "@@ -1,6 +1,10 @@\n"
                       " global_var = 3\n"
                       "+\n"
                       "+def bar(foo):\n"
                       "+    return foo + 1 + global_var\n"
                       "+\n"
                       " \n"
                       " def x():\n"
                       "    foo = 3.1\n"
                       "-   x = int(foo + 1 + global_var)\n"
                       "+   x = int(bar(foo))\n"))))))

(ert-deftest elpy-refactor-extract-function-should-extract-function ()
  (elpy-testcase ((:project project-root "test.py"))
    (find-file (f-join project-root "test.py"))
    (insert (concat
             "global_var = 3\n"
             "\n"
             "def x():\n"
             "   foo = 3.1\n"
             "   x = int(foo + 1 + global_var)\n"))
    (save-buffer)
    (elpy/mark-region 50 69)
    (elpy-refactor-extract-function "bar")
    (should (string= (buffer-string)
                     (concat
                      "global_var = 3\n"
                      "\n"
                      "def bar(foo):\n"
                      "    return foo + 1 + global_var\n"
                      "\n"
                      "\n"
                      "def x():\n"
                      "   foo = 3.1\n"
                      "   x = int(bar(foo))\n"
                      )))))

(ert-deftest elpy-refactor-extract-function-should-abort-when-invalid-fun-name ()
  (elpy-testcase ((:project project-root "test.py"))
    (find-file (f-join project-root "test.py"))
    (insert (concat
             "global_var = 3\n"
             "\n"
             "def x():\n"
             "   foo = 3.1\n"
             "   x = int(foo + 1 + global_var)\n"))
    (save-buffer)
    (elpy/mark-region 50 69)
    (should-error (elpy-refactor-extract-function "2Bar" t))
    (should-error (elpy-refactor-extract-function "128376" t))
    (should-error (elpy-refactor-extract-function "#var" t))))

(ert-deftest elpy-refactor-extract-function-should-abort-when-invalid-selection ()
  (elpy-testcase ((:project project-root "test.py"))
    (find-file (f-join project-root "test.py"))
    (insert (concat
             "global_var = 3\n"
             "\n"
             "def x():\n"
             "   foo = 3.1\n"
             "   x = int(foo + 1 + global_var)\n"))
    (save-buffer)
    (elpy/mark-region 48 69)
    (should-error (elpy-refactor-extract-function "Bar" t))))


)
