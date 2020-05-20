(defsubst elpy-rpc-get-completions--type-hints-supported ()
  (if (boundp '*elpy-rpc-get-completions--type-hints-supported*)
      *elpy-rpc-get-completions--type-hints-supported*
    (setq *elpy-rpc-get-completions--type-hints-supported*
          (not (string< (or (getenv "TRAVIS_PYTHON_VERSION")
                            (with-temp-buffer
                              (call-process elpy-rpc-python-command
                                            nil '(t t) nil "--version")
                              (goto-char (point-min))
                              (re-search-forward "\\([0-9.]+\\)" nil t)
                              (or (match-string 1) "")))
                        "3.6")))))

(ert-deftest elpy-rpc-get-completions ()
  (elpy-testcase ()
    (mletf* ((called-args nil)
             (elpy-rpc (&rest args) (setq called-args args)))

      (elpy-rpc-get-completions)

      (should (equal called-args
                     '("get_completions"
                       (nil "" 0)
                       nil nil))))))

;; temporary workaround for bug in jedi==0.17.0
;; see https://github.com/davidhalter/jedi/pull/1589
(setq annot1 "function"
      annot2 "function")
(when (elpy-rpc-get-completions--type-hints-supported)
  (setq annot1 "function (addition(x, y))"
        annot2 "function (addition2(x, y))"))

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
          (should (string= (alist-get 'annotation compl1) annot1))
          (should (string= (alist-get 'name compl2) "addition2"))
          (should (string= (alist-get 'suffix compl2) "ition2"))
          (should (string= (alist-get 'meta compl2) "def addition2"))
          (should (string= (alist-get 'annotation compl2) annot2)))))


(when (elpy-rpc-get-completions--type-hints-supported)
 (ert-deftest elpy-rpc-get-completions-should-include-type-hints ()
    (elpy-testcase ((:project project-root "test.py")
                    (:emacs-required "25.1"))
        (find-file (f-join project-root "test.py"))
        (elpy-enable)
        (python-mode)
        (insert "test_var: int = 3\n"
                "test_")
        (let ((compl (car (elpy-rpc-get-completions))))
          (should (string= (alist-get 'name compl) "test_var"))
          (should (string= (alist-get 'suffix compl) "var"))
          (should (string= (alist-get 'meta compl) "test_var: int = 3"))
          (should (string= (alist-get 'annotation compl) "statement (int)"))))))


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

;; temporary workaround for bug in jedi==0.17.0
;; see https://github.com/davidhalter/jedi/pull/1589
(setq annot3 "function")
(when (elpy-rpc-get-completions--type-hints-supported)
  (setq annot3 "function (foo12345(x, y))"))

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
          (should (string= (alist-get 'annotation compl) annot3)))))
