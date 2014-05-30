(ert-deftest elpy-nav--iblock-should-move-to-next-line ()
  (elpy-testcase ((:emacs-required "24.3"))
    (set-buffer-string-with-point
     "def foo():"
     "    _|_x = 1"
     "    y = 2"
     "    z = 3")

    (elpy-nav--iblock 1 #'>)

    (should
     (buffer-be
      "def foo():"
      "    x = 1"
      "    _|_y = 2"
      "    z = 3"))))

(ert-deftest elpy-nav--iblock-should-move-to-previous-line ()
  (elpy-testcase ((:emacs-required "24.3"))
    (set-buffer-string-with-point
     "def foo():"
     "    x = 1"
     "    _|_y = 2"
     "    z = 3")

    (elpy-nav--iblock -1 #'>)

    (should
     (buffer-be
      "def foo():"
      "    _|_x = 1"
      "    y = 2"
      "    z = 3"))))

(ert-deftest elpy-nav--iblock-should-move-to-next-block ()
  (elpy-testcase ((:emacs-required "24.3"))
    (set-buffer-string-with-point
     "def foo():"
     "    _|_if foo:"
     "        pass"
     "    y = 2"
     "    z = 3")

    (elpy-nav--iblock 1 #'>)

    (should
     (buffer-be
      "def foo():"
      "    if foo:"
      "        pass"
      "    _|_y = 2"
      "    z = 3"))))

(ert-deftest elpy-nav--iblock-should-handle-multi-line-if ()
  (elpy-testcase ((:emacs-required "24.3"))
    (set-buffer-string-with-point
     "def foo():"
     "    _|_if ("
     "        foo "
     "    ):"
     "        pass"
     "    y = 2"
     "    z = 3")

    (elpy-nav--iblock 1 #'>)

    (should
     (buffer-be
      "def foo():"
      "    if ("
      "        foo "
      "    ):"
      "        pass"
      "    _|_y = 2"
      "    z = 3"))))

(ert-deftest elpy-nav--iblock-should-handle-end-of-line ()
  (elpy-testcase ((:emacs-required "24.3"))
    (set-buffer-string-with-point
     "def foo():"
     "    _|_if ("
     "        foo "
     "    ):"
     "        pass")

    (elpy-nav--iblock 1 #'>)
    ;; Add trailing newline
    (save-excursion
      (insert "\n"))

    (should
     (buffer-be
      "def foo():"
      "    if ("
      "        foo "
      "    ):"
      "        pass"
      "_|_"))))

(ert-deftest elpy-nav--iblock-should-work-with-incomplete-last-line ()
  (elpy-testcase ((:emacs-required "24.3"))
    (set-buffer-string-with-point
     "_|_foo")
    (save-excursion
      (goto-char (point-max))
      (delete-char -1))

    (elpy-nav--iblock 1 #'>)

    (save-excursion
      (goto-char (point-max))
      (insert "\n"))
    (should
     (buffer-be
      "_|_foo"))))
