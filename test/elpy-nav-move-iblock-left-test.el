(ert-deftest elpy-nav-move-iblock-left-should-dedent-block ()
  (elpy-testcase ((:emacs-required "24.3"))
    (set-buffer-string-with-point
     "def foo():"
     "    fred"
     "    qux"
     "    _|_if bar:"
     "        baz")

    (elpy-nav-move-iblock-left)

    (should
     (buffer-be
      "def foo():"
      "    fred"
      "    qux"
      "_|_if bar:"
      "    baz"))))
