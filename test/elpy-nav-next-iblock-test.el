(ert-deftest elpy-nav-next-iblock-should-move-forward-one-line ()
  (elpy-testcase ((:emacs-required "24.3"))
    (set-buffer-string-with-point
     "def foo():"
     "    _|_x = 1"
     "    y = 2"
     "    z = 3")

    (elpy-nav-next-iblock)

    (should
     (buffer-be
      "def foo():"
      "    x = 1"
      "    _|_y = 2"
      "    z = 3"))))

(ert-deftest elpy-nav-next-iblock-should-move-from-def-to-def ()
  (elpy-testcase ((:emacs-required "24.3"))
    (set-buffer-string-with-point
     "class Test(object):"
     "    _|_def foo(self):"
     "        pass"
     ""
     "    def bar(self):"
     "        pass")

    (elpy-nav-next-iblock)

    (should
     (buffer-be
      "class Test(object):"
      "    def foo(self):"
      "        pass"
      ""
      "    _|_def bar(self):"
      "        pass"))))

(ert-deftest elpy-nav-next-iblock-should-move-over-complex-statement ()
  (elpy-testcase ((:emacs-required "24.3"))
    (set-buffer-string-with-point
     "def foo(self):"
     "    _|_if ("
     "        foo"
     "    ):"
     "        pass"
     ""
     "    x = 1")

    (elpy-nav-next-iblock)

    (should
     (buffer-be
      "def foo(self):"
      "    if ("
      "        foo"
      "    ):"
      "        pass"
      ""
      "    _|_x = 1"))))

(ert-deftest elpy-nav-next-iblock-should-stop-at-end-of-class ()
  (elpy-testcase ((:emacs-required "24.3"))
    (set-buffer-string-with-point
     "class Test(object):"
     "    def foo(self):"
     "        pass"
     ""
     "    _|_def bar(self):"
     "        pass"
     ""
     "class Test2:"
     "    pass")

    (elpy-nav-next-iblock)

    (should
     (buffer-be
      "class Test(object):"
      "    def foo(self):"
      "        pass"
      ""
      "    _|_def bar(self):"
      "        pass"
      ""
      "class Test2:"
      "    pass"))))

(ert-deftest elpy-nav-next-iblock-should-stop-at-end-of-file ()
  (elpy-testcase ((:emacs-required "24.3"))
    (set-buffer-string-with-point
     "_|_class Test2:"
     "    pass")

    (elpy-nav-next-iblock)
    (save-excursion (insert "\n"))

    (should
     (buffer-be
      "class Test2:"
      "    pass"
      "_|_"))))
