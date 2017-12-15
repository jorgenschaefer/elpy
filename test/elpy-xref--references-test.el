(ert-deftest elpy-xref--references-test ()
  (when (featurep 'xref)
    (elpy-testcase ((:project project-root "test.py"))
        (find-file (f-join project-root "test.py"))
        (python-mode)
        (elpy-mode)
        (insert "def foo(x, y):\n"
                "    return x + y\n"
                "var1 = foo(5, 2)")
        (let ((file (f-join project-root "test.py"))
            (foo-refs (elpy-xref--references "foo")))
        (should (equal (length foo-refs) 2))
        (let* ((foo-refs1 (car foo-refs))
                (summary1 (xref-item-summary foo-refs1))
                (location1 (xref-item-location foo-refs1))
                (file1 (xref-elpy-location-file location1))
                (pos1 (xref-elpy-location-pos location1)))
            (should (string-equal summary1 "1:	def foo(x, y):"))
            (should (equal pos1 5))
            (should (string-equal file file1)))
        (let* ((foo-refs2 (car (cdr foo-refs)))
                (summary2 (xref-item-summary foo-refs2))
                (location2 (xref-item-location foo-refs2))
                (file2 (xref-elpy-location-file location2))
                (pos2 (xref-elpy-location-pos location2)))
            (should (string-equal summary2 "3:	var1 = foo(5, 2)"))
            (should (equal pos2 40))
            (should (string-equal file file2)))))))
