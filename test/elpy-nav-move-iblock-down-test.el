(ert-deftest elpy-nav-move-iblock-down-should-move-from-start-of-def-down ()
  (elpy-testcase ()
    (set-buffer-string-with-point
     "def foo():"
     "    _|_if bar:"
     "        baz"
     "    qux"
     "    fred")

    (elpy-nav-move-iblock-down)

    (should
     (buffer-be
      "def foo():"
      "    qux"
      "    _|_if bar:"
      "        baz"
      "    fred"))))

(ert-deftest elpy-nav-move-iblock-down-should-move-block-to-end ()
  (elpy-testcase ()
    (set-buffer-string-with-point
     "def foo():"
     "    _|_if bar:"
     "        baz"
     "    qux")

    (elpy-nav-move-iblock-down)

    (should
     (buffer-be
      "def foo():"
      "    qux"
      "    _|_if bar:"
      "        baz"))
    ))

(ert-deftest elpy-nav-move-iblock-down-should-stop-at-last-line ()
  (elpy-testcase ()
    (set-buffer-string-with-point
     "def foo():"
     "    qux"
     "    _|_if bar:"
     "        baz")

    (elpy-nav-move-iblock-down)

    (should
     (buffer-be
      "def foo():"
      "    qux"
      "    _|_if bar:"
      "        baz"))))

(ert-deftest elpy-nav-move-iblock-down-should-work-at-end-of-file ()
  (elpy-testcase ()
    (set-buffer-string-with-point
     "_|_foo"
     "bar")
    (save-excursion
      (goto-char (point-max))
      (delete-char -1))

    (elpy-nav-move-iblock-down)

    (should
     (buffer-be
      "bar"
      "_|_foo"))))
