(ert-deftest elpy-rpc-get-completions ()
  (elpy-testcase ()
    (mletf* ((called-args nil)
             (elpy-rpc (&rest args) (setq called-args args)))

      (elpy-rpc-get-completions)

      (should (equal called-args
                     '("get_completions"
                       (nil "" 0)
                       nil nil))))))

(ert-deftest elpy-rpc-get-completions-should-return-completion ()
    (elpy-testcase ((:project project-root "test.py")
                    (:emacs-required "25.1"))
        (find-file (f-join project-root "test.py"))
        (elpy-enable)
        (python-mode)
        (insert "def addition(x, y):\n"
                "    return x + y\n"
                "def addition2(x, y):\n"
                "    return x + y\n"
                "var1 = add")
        (let* ((compls (elpy-rpc-get-completions))
               (compl1 (car compls))
               (compl2 (car (cdr compls))))
          (should (string= (alist-get 'name compl1) "addition"))
          (should (string= (alist-get 'suffix compl1) "ition"))
          (should (string= (alist-get 'meta compl1) "def addition"))
          (should (string= (alist-get 'annotation compl1) "function (addition(x, y))"))
          (should (string= (alist-get 'name compl2) "addition2"))
          (should (string= (alist-get 'suffix compl2) "ition2"))
          (should (string= (alist-get 'meta compl2) "def addition2"))
          (should (string= (alist-get 'annotation compl2) "function (addition2(x, y))")))))

(ert-deftest elpy-rpc-get-completions-should-include-type-hints ()
    (elpy-testcase ((:project project-root "test.py")
                    (:emacs-required "25.1"))
        (find-file (f-join project-root "test.py"))
        (elpy-enable)
        (python-mode)
        (insert "test_var: int = 3\n"
                "test_")
        (let* ((compls (elpy-rpc-get-completions))
               (compl1 (car compls))
               (compl2 (car (cdr compls))))
          (should (string= (alist-get 'name compl1) "test_var"))
          (should (string= (alist-get 'suffix compl1) "var"))
          (should (string= (alist-get 'meta compl1) "test_var: int = 3"))
          (should (string= (alist-get 'annotation compl1) "statement (int)")))))

(ert-deftest elpy-rpc-get-completions-should-not-return-completion-for-numbers ()
    (elpy-testcase ((:project project-root "test.py")
                    (:emacs-required "25.1"))
        (find-file (f-join project-root "test.py"))
        (elpy-enable)
        (python-mode)
        (insert "def foo(x, y):\n"
                "    return x + y\n"
                "a = 893")
        (let ((compl (elpy-rpc-get-completions)))
          (should (equal compl nil)))))

(ert-deftest elpy-rpc-get-completions-should-return-completion-for-variable-with-numbers ()
    (elpy-testcase ((:project project-root "test.py")
                    (:emacs-required "25.1"))
        (find-file (f-join project-root "test.py"))
        (elpy-enable)
        (python-mode)
        (insert "def foo12345(x, y):\n"
                "    return x + y\n"
                "a = foo12")
        (let ((compl (car (elpy-rpc-get-completions))))
          (should (string= (alist-get 'name compl) "foo12345"))
          (should (string= (alist-get 'suffix compl) "345"))
          (should (string= (alist-get 'meta compl) "def foo12345"))
          (should (string= (alist-get 'annotation compl) "function (foo12345(x, y))")))))
