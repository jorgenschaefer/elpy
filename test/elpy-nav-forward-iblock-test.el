(ert-deftest elpy-nav-forward-iblock-should-move-to-lower-level ()
  (elpy-testcase ()
    (set-buffer-string-with-point
     "_|_def foo():"
     "    x = 1"
     "    y = 2"
     "    z = 3")

    (elpy-nav-forward-iblock)

    (should
     (buffer-be
      "def foo():"
      "    _|_x = 1"
      "    y = 2"
      "    z = 3"))))

(ert-deftest elpy-nav-forward-iblock-should-move-over-complex-statement ()
  (elpy-testcase ()
    (set-buffer-string-with-point
     "def foo():"
     "    _|_if ("
     "        bar"
     "    ):"
     "        pass"
     "    x = 1"
     "    y = 2"
     "    z = 3")

    (elpy-nav-forward-iblock)

    (should
     (buffer-be
      "def foo():"
     "    if ("
     "        bar"
     "    ):"
     "        _|_pass"
      "    x = 1"
      "    y = 2"
      "    z = 3"))))

(ert-deftest elpy-nav-forward-iblock-does-not-go-to-end-of-file ()
  (elpy-testcase ()
    (set-buffer-string-with-point
     "def foo():"
     "    x = 1"
     "    y = 2"
     "    _|_z = 3"
     "")

    (elpy-nav-forward-iblock)

    (should
     (buffer-be
      "def foo():"
      "    x = 1"
      "    y = 2"
      "    _|_z = 3"
      ""))))

(ert-deftest elpy-nav-forward-iblock-does-work-at-end-of-file ()
  (elpy-testcase ()
    (set-buffer-string-with-point
     "def foo():"
     "    x = 1"
     "    y = 2"
     "    z = 3"
     "_|_")

    (elpy-nav-forward-iblock)

    (should
     (buffer-be
      "def foo():"
      "    x = 1"
      "    y = 2"
      "    z = 3"
      "_|_"))))
