(ert-deftest elpy-nav-move-iblock-up-should-move-from-end-of-def-up ()
  (elpy-testcase ()
    (set-buffer-string-with-point
     "def foo():"
     "    fred"
     "    qux"
     "    _|_if bar:"
     "        baz")

    (elpy-nav-move-iblock-up)

    (should
     (buffer-be
      "def foo():"
      "    fred"
      "    _|_if bar:"
      "        baz"
      "    qux"))))

(ert-deftest elpy-nav-move-iblock-up-should-move-block-to-start ()
  (elpy-testcase ()
    (set-buffer-string-with-point
     "def foo():"
     "    qux"
     "    _|_if bar:"
     "        baz")

    (elpy-nav-move-iblock-up)

    (should
     (buffer-be
      "def foo():"
      "    _|_if bar:"
      "        baz"
      "    qux"))))

(ert-deftest elpy-nav-move-iblock-up-should-stop-at-first-line ()
  (elpy-testcase ()
    (set-buffer-string-with-point
     "def foo():"
     "    _|_if bar:"
     "        baz"
     "    qux"
     )

    (elpy-nav-move-iblock-up)

    (should
     (buffer-be
      "def foo():"
      "    _|_if bar:"
      "        baz"
      "    qux"))))

(ert-deftest elpy-nav-move-iblock-down-should-work-at-start-of-file ()
  (elpy-testcase ()
    (set-buffer-string-with-point
     "foo"
     "_|_bar")
    (save-excursion
      (goto-char (point-max))
      (delete-char -1))

    (elpy-nav-move-iblock-up)

    (should
     (buffer-be
      "_|_bar"
      "foo"))))
