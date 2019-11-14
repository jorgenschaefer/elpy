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
    (python-mode)
    (elpy-mode)
    (elpy-folding-toggle-at-point)
    (let* ((overlays (overlays-in (point-min) (point-max)))
           overlay)
      (should (= 4 (length overlays)))
      (setq overlay (nth 1 overlays))
      (should (eq (overlay-get overlay 'hs) 'comment))
      (should (= (overlay-start overlay) 111))
      (should (= (overlay-end overlay) 156)))
    (should (= (point) 92))
    ;; Unfold
    (elpy-folding-toggle-at-point)
    (let* ((overlays (overlays-in (point-min) (point-max)))
           overlay)
      (should (= 3 (length overlays))))
    ;; Position
    (should (= (point) 92))))

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
    (python-mode)
    (elpy-mode)
    (elpy-folding-toggle-at-point)
    (let* ((overlays (overlays-in (point-min) (point-max)))
           overlay)
      (should (= 3 (length overlays))))
    (should (= (point) 112))))
