(ert-deftest elpy-nav-backward-block-should-go-to-next-line ()
  (elpy-testcase ((:emacs-required "24.3"))
    (set-buffer-string-with-point
     "def foo():"
     "    x = 1"
     "    _|_y = 2")

    (elpy-nav-backward-block)

    (should
     (buffer-be
      "def foo():"
      "    _|_x = 1"
      "    y = 2"))))

(ert-deftest elpy-nav-backward-block-should-skip-block ()
  (elpy-testcase ((:emacs-required "24.3"))
    (set-buffer-string-with-point
     "def foo():"
     "    if True:"
     "        pass"
     "    _|_y = 2")

    (elpy-nav-backward-block)

    (should
     (buffer-be
      "def foo():"
      "    _|_if True:"
      "        pass"
      "    y = 2"))))

(ert-deftest elpy-nav-backward-block-should-skip-multiline-block ()
  (elpy-testcase ((:emacs-required "24.3"))
    (set-buffer-string-with-point
     "def foo():"
     "    if ("
     "        True"
     "    ):"
     "        pass"
     "    _|_y = 2")

    (elpy-nav-backward-block)

    (should
     (buffer-be
      "def foo():"
      "    _|_if ("
      "        True"
      "    ):"
      "        pass"
      "    y = 2"))))

(ert-deftest elpy-nav-backward-block-should-keep-current-indent ()
  (elpy-testcase ((:emacs-required "24.3"))
    (set-buffer-string-with-point
     "def foo():"
     "    if True:"
     "    _|_    pass"
     "    y = 2")

    (elpy-nav-backward-block)

    (should
     (buffer-be
      "def foo():"
      "    _|_if True:"
      "        pass"
      "    y = 2"))))

(ert-deftest elpy-nav-backward-block-should-keep-current-indent-2 ()
  (elpy-testcase ((:emacs-required "24.3"))
    (set-buffer-string-with-point
     "def foo():"
     "    if True:"
     "    _|_    pass"
     "    y = 2")

    (elpy-nav-backward-block)

    (should
     (buffer-be
      "def foo():"
      "    _|_if True:"
      "        pass"
      "    y = 2"))))
