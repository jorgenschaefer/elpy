(ert-deftest elpy-nav-backward-definition-should-go-to-next-class ()
  (elpy-testcase ()
    (insert-source
     "import foo"
     ""
     "class Foo:"
     "    pass")
    (goto-char (point-max))
    (elpy-nav-backward-definition)
    (should (looking-at "class Foo:"))))

(ert-deftest elpy-nav-backward-definition-should-go-to-next-function ()
  (elpy-testcase ()
    (insert-source
     "import foo"
     ""
     "def foo():"
     "    pass")
    (goto-char (point-max))
    (elpy-nav-backward-definition)
    (should (looking-at "def foo"))))

(ert-deftest elpy-nav-backward-definition-should-go-to-end-of-buffer ()
  (elpy-testcase ()
    (insert-source
     "import foo"
     ""
     "bla")
    (goto-char (point-max))
    (elpy-nav-backward-definition)
    (should (equal (point) (point-min)))))
