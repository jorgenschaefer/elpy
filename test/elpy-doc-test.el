(ert-deftest elpy-pydoc--completions-should-return-completions ()
  (with-elpy-file project-root "foo.py"
    (let ((names (elpy-pydoc--completions "")))
      (should (listp names))
      (should (member "sys" names)))
    (let ((names (elpy-pydoc--completions "sys")))
      (should (listp names))
      (should (member "exit" names)))))
