(ert-deftest elpy-nav-previous-iblock-should-move-backward-one-line ()
  (elpy-testcase ((:emacs-required "24.3"))
    (set-buffer-string-with-point
     "def foo():"
     "    x = 1"
     "    _|_y = 2"
     "    z = 3")

    (elpy-nav-previous-iblock)

    (should
     (buffer-be
      "def foo():"
      "    _|_x = 1"
      "    y = 2"
      "    z = 3"))))

(ert-deftest elpy-nav-previous-iblock-should-move-from-def-to-def ()
  (elpy-testcase ((:emacs-required "24.3"))
    (set-buffer-string-with-point
     "class Test(object):"
     "    def foo(self):"
     "        pass"
     ""
     "    _|_def bar(self):"
     "        pass")

    (elpy-nav-previous-iblock)

    (should
     (buffer-be
      "class Test(object):"
      "    _|_def foo(self):"
      "        pass"
      ""
      "    def bar(self):"
      "        pass"))))

(ert-deftest elpy-nav-previous-iblock-should-move-over-complex-statement ()
  (elpy-testcase ((:emacs-required "24.3"))
    (set-buffer-string-with-point
     "def foo(self):"
     "    if ("
     "        foo"
     "    ):"
     "        pass"
     ""
     "    _|_x = 1")

    (elpy-nav-previous-iblock)

    (should
     (buffer-be
      "def foo(self):"
      "    _|_if ("
      "        foo"
      "    ):"
      "        pass"
      ""
      "    x = 1"))))

(ert-deftest elpy-nav-previous-iblock-should-stop-at-beginning-of-class ()
  (elpy-testcase ((:emacs-required "24.3"))
    (set-buffer-string-with-point
     "class Test(object):"
     "    _|_def foo(self):"
     "        pass"
     ""
     "    def bar(self):"
     "        pass"
     ""
     "class Test2:"
     "    pass")

    (elpy-nav-previous-iblock)

    (should
     (buffer-be
      "class Test(object):"
      "    _|_def foo(self):"
      "        pass"
      ""
      "    def bar(self):"
      "        pass"
      ""
      "class Test2:"
      "    pass"))))

(ert-deftest elpy-nav-previous-iblock-should-stop-at-beginning-of-file ()
  (elpy-testcase ((:emacs-required "24.3"))
    (set-buffer-string-with-point
     ""
     "_|_class Test2:"
     "    pass")

    (elpy-nav-previous-iblock)

    (should
     (buffer-be
      "_|_"
      "class Test2:"
      "    pass"))))
