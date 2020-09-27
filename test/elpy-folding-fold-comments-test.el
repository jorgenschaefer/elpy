(ert-deftest elpy-fold-at-point-should-fold-and-unfold-comments ()
  (elpy-testcase ()
    (add-to-list 'elpy-modules 'elpy-module-folding)
    (set-buffer-string-with-point
     "var1 = 45"
     "class foo(object):"
     "  def __init__(self, a, b):"
     "    self.a = a"
     "    self.b = b"
     "    # This is a _|_comment"
     "    # on several lines"
     "    # to test folding"
     "  def bar(mess):"
     "    mess *= 2"
     "    print(mess)"
     "    return mess"
     "var2 = foo(var1, 4)")
    (elpy-enable)
    (python-mode)
    (elpy-folding-toggle-at-point)
    (let* ((overlays (overlays-in (point-min) (point-max)))
           overlay)
      (setq overlay (elpy-get-overlay-at 111 'comment))
      (should overlay)
      (should (eq (overlay-get overlay 'hs) 'comment))
      (should (= (overlay-start overlay) 111))
      (should (= (overlay-end overlay) 156)))
    (should (or (= (point) 92)
                (= (point) 104)))
    ;; Unfold
    (elpy-folding-toggle-at-point)
    ;; Position
    (should (or (= (point) 92)
                (= (point) 104)))))

(ert-deftest elpy-fold-at-point-should-NOT-fold-and-unfold-oneline-comments ()
  (elpy-testcase ()
    (add-to-list 'elpy-modules 'elpy-module-folding)
    (set-buffer-string-with-point
     "var1 = 45"
     "class foo(object):"
     "  def __init__(self, a, b):"
     "    self.a = a"
     "    self.b = b"
     "    # This is a oneline _|_comment"
     "  def bar(mess):"
     "    mess *= 2"
     "    print(mess)"
     "    return mess"
     "var2 = foo(var1, 4)")
    (elpy-enable)
    (python-mode)
    (elpy-folding-toggle-at-point)
    (should (= (point) 112))))
