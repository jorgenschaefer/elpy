(ert-deftest elpy-nav-backward-iblock-should-move-to-previous-def ()
  (elpy-testcase ()
    (set-buffer-string-with-point
     "def foo():"
     "    x = 1"
     "    _|_y = 2"
     "    z = 3")

    (elpy-nav-backward-iblock)

    (should
     (buffer-be
      "_|_def foo():"
      "    x = 1"
      "    y = 2"
      "    z = 3"))))

(ert-deftest elpy-nav-backward-iblock-should-move-over-complex-statement ()
  (elpy-testcase ()
    (set-buffer-string-with-point
     "def foo():"
     "    if ("
     "        bar"
     "    ):"
     "        pass"
     "    x = 1"
     "    _|_y = 2"
     "    z = 3")

    (elpy-nav-backward-iblock)

    (should
     (buffer-be
      "_|_def foo():"
     "    if ("
     "        bar"
     "    ):"
     "        pass"
      "    x = 1"
      "    y = 2"
      "    z = 3"))))

(ert-deftest elpy-nav-backward-iblock-does-not-go-to-start-of-file ()
  (elpy-testcase ()
    (set-buffer-string-with-point
     ""
     "_|_def foo():"
     "    x = 1"
     "    y = 2"
     "    z = 3")

    (elpy-nav-backward-iblock)

    (should
     (buffer-be
      ""
      "_|_def foo():"
      "    x = 1"
      "    y = 2"
      "    z = 3"))))

(ert-deftest elpy-nav-backward-iblock-does-work-at-beginning-of-file ()
  (elpy-testcase ()
    (set-buffer-string-with-point
     "_|_"
     "def foo():"
     "    x = 1"
     "    y = 2"
     "    z = 3")

    (elpy-nav-backward-iblock)

    (should
     (buffer-be
      "_|_"
      "def foo():"
      "    x = 1"
      "    y = 2"
      "    z = 3"))))
