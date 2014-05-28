(ert-deftest elpy-nav-move-iblock-right-should-indent-block ()
  (elpy-testcase ()
    (python-mode)
    (set-buffer-string-with-point
     "def foo():"
     "    fred"
     "    qux"
     "_|_if bar:"
     "    baz")

    (elpy-nav-move-iblock-right)

    (should
     (buffer-be
      "def foo():"
      "    fred"
      "    qux"
      "_|_    if bar:"
      "        baz"))))
