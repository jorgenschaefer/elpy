(ert-deftest elpy-xref--goto-identifier-should-go-to-identifier ()
  (when (featurep 'xref)
    (elpy-testcase ()
        (python-mode)
        (elpy-mode)
        (insert "def foo(x, y):\n"
                "    return x + y\n"
                "var1 = foo(5, 2)")
        (goto-char (point-min))
        (elpy-xref--goto-identifier "1: foo")
        (should (equal (point) 5))
        (elpy-xref--goto-identifier "3: var1")
        (should (equal (point) 33)))))
