(ert-deftest elpy-xref--get-completion-table-should-return-completion-table ()
  (when (featurep 'xref)
    (elpy-testcase ((:project project-root "test.py")) (find-file (f-join project-root "test.py"))
        (elpy-enable)
        (python-mode)
        (insert "def foo(x, y):\n"
                "    return x + y\n"
                "var1 = foo(5, 2)")
        (save-buffer)
        (let ((identifiers (elpy-xref--get-completion-table)))
        (should (equal identifiers
       (list "3: foo" "3: var1" "2: y" "2: x" "1: y" "1: x" "1: foo")))))))
