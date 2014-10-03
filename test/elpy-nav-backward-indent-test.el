(defun elpy-nav-backward-indent-should-go-to-next-indent-level ()
  (elpy-testcase ((:emacs-required "24.3"))
    (set-buffer-string-with-point
     "def foo():"
     "    _|_x = 1")

    (elpy-nav-backward-indent)

    (should
     (buffer-be
      "def foo():"
      "_|_    x = 1"))))

(defun elpy-nav-backward-indent-should-skip-word-in-line ()
  (elpy-testcase ((:emacs-required "24.3"))
    (set-buffer-string-with-point
     "def foo():"
     "    xyzzyfoo_|_ = 1")

    (elpy-nav-backward-indent)

    (should
     (buffer-be
      "def foo():"
      "    _|_xyzzyfoo = 1"))))
