(ert-deftest elpy-xref--get-completion-table-should-return-completion-table ()
  (when (featurep 'xref)
    (elpy-testcase ((:project project-root "test.py")) (find-file (f-join project-root "test.py"))
        (python-mode)
        (setq elpy-rpc-backend "jedi")
        (elpy-mode)
        (insert "def foo(x, y):\n"
                "    return x + y\n"
                "var1 = foo(5, 2)")
        (let ((identifiers (elpy-xref--get-completion-table)))
        (should (equal identifiers (list "foo" "x" "y" "var1")))))))
