(ert-deftest elpy-xref--backend-should-return-true-when-jedi-available ()
  (when (featurep 'xref)
    (elpy-testcase ((:project project-root "test.py"))
        (find-file (f-join project-root "test.py"))
        (elpy-enable)
        (python-mode)
        (insert "def foo(x, y):\n"
                "    return x + y\n"
                "var1 = foo(5, 2)")
        ;; First start the rpc and assume jedi is available
        (should (string= (xref-find-backend) "elpy"))
        ;; Second check if jedi is available
        (let ((elpy-rpc--jedi-available t))
          (should (string= (xref-find-backend) "elpy"))))))

(ert-deftest elpy-xref--backend-should-return-false-when-jedi-not-available ()
  (when (featurep 'xref)
    (elpy-testcase ((:project project-root "test.py"))
        (find-file (f-join project-root "test.py"))
        (elpy-enable)
        (python-mode)
        (insert "def foo(x, y):\n"
                "    return x + y\n"
                "var1 = foo(5, 2)")
        ;; First start the rpc and assume jedi is available
        (should (string= (xref-find-backend) "elpy"))
        (let ((elpy-rpc--jedi-available nil))
          ;; Second check if jedi is available
          (should (not (string= (xref-find-backend) "elpy")))))))
