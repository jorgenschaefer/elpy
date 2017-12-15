(ert-deftest elpy-xref--get-completion-table-should-return-completion-table ()
  (when (featurep 'xref)
    (elpy-testcase ((:project project-root "test.py")) (find-file (f-join project-root "test.py"))
        (python-mode)
        (elpy-mode)
        (insert "def foo(x, y):\n"
                "    return x + y\n"
                "var1 = foo(5, 2)")
        (let ((identifiers (elpy-xref--get-completion-table)))
        (should (equal identifiers (list "var1" "y" "x" "foo")))))))
