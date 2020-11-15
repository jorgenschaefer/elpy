(unless (or (version< (elpy-rpc--get-python-version) "3.6" )
            (version< emacs-version "25.0")  ;; diffs are not parsed properly before emacs 25
            (string= (alist-get 'success (elpy-rpc-get-rename-diff "foo"))
                     "Not available"))

(ert-deftest elpy-refactor-rpc-rename-should-return-diff ()
  (elpy-testcase ((:project project-root "test.py"))
    (find-file (f-join project-root "test.py"))
    (insert (concat
             "def bar(foo):\n"
             "   return foo + 1 + global_var\n"
             "\n"
             "def x(foo):\n"
             "   x = int(bar(foo))\n"))
    (save-buffer)
    (goto-char 75)
    (let* ((diff (elpy-rpc-get-rename-diff "new_foo"))
           (proj-path (alist-get 'project_path diff))
           (success (alist-get 'success diff))
           (diff (alist-get 'diff diff)))
      (should success)
      (should (file-exists-p proj-path))
      (should (string= diff
                       (concat
                        "--- test.py\n"
                        "+++ test.py\n"
                        "@@ -1,6 +1,6 @@\n"
                        " def bar(foo):\n"
                        "    return foo + 1 + global_var\n"
                        " \n"
                        "-def x(foo):\n"
                        "-   x = int(bar(foo))\n"
                        "+def x(new_foo):\n"
                        "+   x = int(bar(new_foo))\n"))))))

(ert-deftest elpy-refactor-rename-should-rename ()
  (elpy-testcase ((:project project-root "test.py"))
    (find-file (f-join project-root "test.py"))
    (insert (concat
             "def bar(foo):\n"
             "   return foo + 1 + global_var\n"
             "\n"
             "def x(foo):\n"
             "   x = int(bar(foo))\n"))
    (save-buffer)
    (goto-char 75)
    (elpy-refactor-rename "new_foo" t)
    (goto-char 6)
    (elpy-refactor-rename "new_bar" t)
    (should (string= (buffer-string)
                     "def new_bar(foo):
   return foo + 1 + global_var

def x(new_foo):
   x = int(new_bar(new_foo))
"))))


(ert-deftest elpy-refactor-rpc-rename-should-rename-variable ()
  (elpy-testcase ((:project project-root "test.py"))
    (find-file (f-join project-root "test.py"))
    (set-buffer-string-with-point
     "def bar(foo):"
     "   return foo + 1 + global_var"
     ""
     "def x(f_|_oo):"
     "   x = int(bar(foo))")
    (elpy-refactor-rename "new_foo" t)
    (should (string= (buffer-string)
                     (concat
                     "def bar(foo):\n"
                     "   return foo + 1 + global_var\n"
                     "\n"
                     "def x(new_foo):\n"
                     "   x = int(bar(new_foo))\n")))))

(ert-deftest elpy-refactor-rpc-rename-should-rename-function ()
  (elpy-testcase ((:project project-root "test.py"))
    (find-file (f-join project-root "test.py"))
    (set-buffer-string-with-point
     "def ba_|_r(foo):"
     "   return foo + 1 + global_var"
     ""
     "def x(foo):"
     "   x = int(bar(foo))")
    (elpy-refactor-rename "new_bar" t)
    (should (string= (buffer-string)
                     (concat
                     "def new_bar(foo):\n"
                     "   return foo + 1 + global_var\n"
                     "\n"
                     "def x(foo):\n"
                     "   x = int(new_bar(foo))\n")))))

(ert-deftest elpy-refactor-rpc-rename-should-rename-class ()
  (elpy-testcase ((:project project-root "test.py"))
    (find-file (f-join project-root "test.py"))
    (set-buffer-string-with-point
     "class Foo(object):"
     "   def __init__(self, a, b):"
     "      self.res = a + b"
     ""
     "foo = Fo_|_o(1, 2)"
     "print(foo.res)")
    (elpy-refactor-rename "Bar" t)
    (should (string= (buffer-string)
                     (concat
     "class Bar(object):\n"
     "   def __init__(self, a, b):\n"
     "      self.res = a + b\n"
     "\n"
     "foo = Bar(1, 2)\n"
     "print(foo.res)\n")))))

(ert-deftest elpy-refactor-rpc-rename-should-rename-in-multiple-files ()
  (elpy-testcase ((:project project-root ("test.py" "Classes.py")))
    (find-file (f-join project-root "Classes.py"))
    (set-buffer-string-with-point
     "class Foo_|_(object):"
     "   def __init__(self, a, b):"
     "      self.res = a + b")
    (save-buffer)
    (find-file (f-join project-root "test.py"))
    (set-buffer-string-with-point
     "from Classes import Foo"
     "foo = F_|_oo(1, 2)"
     "print(foo.res)")
    (save-buffer)
    (let ((diff (alist-get 'diff (elpy-rpc-get-rename-diff "new_foo"))))
      (should (string= diff
                       (concat
                        "--- Classes.py\n"
                        "+++ Classes.py\n"
                        "@@ -1,4 +1,4 @@\n"
                        "-class Foo(object):\n"
                        "+class new_foo(object):\n"
                        "    def __init__(self, a, b):\n"
                        "       self.res = a + b\n"
                        "--- test.py\n"
                        "+++ test.py\n"
                        "@@ -1,4 +1,4 @@\n"
                        "-from Classes import Foo\n"
                        "-foo = Foo(1, 2)\n"
                        "+from Classes import new_foo\n"
                        "+foo = new_foo(1, 2)\n"
                        " print(foo.res)\n"))))))

(ert-deftest elpy-refactor-rpc-rename-should-abort-when-not-on-symbol ()
  (elpy-testcase ((:project project-root "test.py"))
    (find-file (f-join project-root "test.py"))
    (set-buffer-string-with-point
     "class Foo(object):"
     "   def __init__(self, a, b):"
     "      self.res = a + b"
     ""
     "foo = Foo(1, _|_2)"
     "print(foo.res)")
    (should-error (elpy-refactor-rename "Bar" t))
    (kill-region (point-min) (point-max))
    (set-buffer-string-with-point
     "class Foo(object):"
     "   def __init__(self, a, b):"
     "      self.res = a + b"
     ""
     "foo = Foo(1, 2)_|_"
     "print(foo.res)")
    (should-error (elpy-refactor-rename "Bar" t))
    (kill-region (point-min) (point-max))
    (set-buffer-string-with-point
     "class Foo(object):"
     "   d_|_ef __init__(self, a, b):"
     "      self.res = a + b"
     ""
     "foo = Foo(1, 2)"
     "print(foo.res)")
    (should-error (elpy-refactor-rename "Bar" t))
    (kill-region (point-min) (point-max))
    (set-buffer-string-with-point
     "class Foo(object):"
     "   def __init__(self, a, b):"
     "      self.res = a + b"
     ""
     "foo =_|_ Foo(1, 2)"
     "print(foo.res)")
    (should-error (elpy-refactor-rename "Bar" t))))


(ert-deftest elpy-refactor-rpc-rename-should-abort-when-invalid-new-name ()
  (elpy-testcase ((:project project-root "test.py"))
    (find-file (f-join project-root "test.py"))
    (set-buffer-string-with-point
     "class Foo(object):"
     "   def __init__(self, a, b):"
     "      self.res = a + b"
     ""
     "foo = F_|_oo(1, 2)"
     "print(foo.res)")
    (should-error (elpy-refactor-rename "2Bar" t))
    (should-error (elpy-refactor-rename "128376" t))
    (should-error (elpy-refactor-rename "#var" t))))

(ert-deftest elpy-refactor-rpc-rename-should-ignore-regions ()
  (elpy-testcase ((:project project-root "test.py"))
    (find-file (f-join project-root "test.py"))
    (set-buffer-string-with-point
     "def bar(foo):"
     "   return foo + 1 + global_var"
     ""
     "def x(fo_|_o):"
     "   x = int(bar(foo))")
    (elpy/mark-region 12 (point))
    (elpy-refactor-rename "new_foo" t)
    (should (string= (buffer-string)
                     (concat
                     "def bar(foo):\n"
                     "   return foo + 1 + global_var\n"
                     "\n"
                     "def x(new_foo):\n"
                     "   x = int(bar(new_foo))\n")))))

)
