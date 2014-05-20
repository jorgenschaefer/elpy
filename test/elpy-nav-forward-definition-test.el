(ert-deftest elpy-nav-forward-definition-should-go-to-next-class ()
  (elpy-testcase ()
    (insert-source
     "import foo"
     ""
     "class Foo:"
     "    pass")
    (goto-char (point-min))
    (elpy-nav-forward-definition)
    (should (looking-at "class Foo:"))))

(ert-deftest elpy-nav-forward-definition-should-go-to-next-function ()
  (elpy-testcase ()
    (insert-source
     "import foo"
     ""
     "def foo():"
     "    pass")
    (goto-char (point-min))
    (elpy-nav-forward-definition)
    (should (looking-at "def foo"))))

(ert-deftest elpy-nav-forward-definition-should-go-to-end-of-buffer ()
  (elpy-testcase ()
    (insert-source
     "import foo"
     ""
     "bla")
    (goto-char (point-min))
    (elpy-nav-forward-definition)
    (should (equal (point) (point-max)))))
