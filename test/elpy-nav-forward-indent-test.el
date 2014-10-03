(defun elpy-nav-forward-indent-should-go-to-next-indent-level ()
  (elpy-testcase ((:emacs-required "24.3"))
    (set-buffer-string-with-point
     "def foo():"
     "_|_    x = 1")

    (elpy-nav-forward-indent)

    (should
     (buffer-be
      "def foo():"
      "    _|_x = 1"))))

(defun elpy-nav-forward-indent-should-skip-word-in-line ()
  (elpy-testcase ((:emacs-required "24.3"))
    (set-buffer-string-with-point
     "def foo():"
     "    _|_xyzzyfoo = 1")

    (elpy-nav-forward-indent)

    (should
     (buffer-be
      "def foo():"
      "    xyzzyfoo_|_ = 1"))))
