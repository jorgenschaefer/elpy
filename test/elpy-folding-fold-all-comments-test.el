(ert-deftest elpy-fold-all-comments-should-fold-all-comments ()
  (elpy-testcase ()
    (add-to-list 'elpy-modules 'elpy-module-folding)
    (set-buffer-string-with-point
     "var1 = 45"
     "# This is a comment to explain nothing"
     "# about the life and the universe"
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
     "    # Another comment here"
     "    return mess"
     "# Another comment on"
     "# two lines !"
     "var2 = foo(var1, 4)")
    (python-mode)
    (elpy-mode)
    ;; Fold all comments
    (elpy-folding-toggle-comments)
    (let* ((overlays (overlays-in (point-min) (point-max)))
           overlay)
      (should (= 6 (length overlays)))
      ;; first two lines comment
      (setq overlay (elpy-get-overlay-at 49 'comment))
      (should overlay)
      (should (eq (overlay-get overlay 'hs) 'comment))
      (should (= (overlay-start overlay) 49))
      (should (= (overlay-end overlay) 83))
      ;; second three lines comment
      (setq overlay (elpy-get-overlay-at 184 'comment))
      (should overlay)
      (should (eq (overlay-get overlay 'hs) 'comment))
      (should (= (overlay-start overlay) 184))
      (should (= (overlay-end overlay) 229))
      ;; third two lines comment
      (setq overlay (elpy-get-overlay-at 340 'comment))
      (should overlay)
      (should (eq (overlay-get overlay 'hs) 'comment))
      (should (= (overlay-start overlay) 340))
      (should (= (overlay-end overlay) 354)))
    ;; point shouldn't move
    (should (= (point) 177))
    ;; Unfold all comments
    (elpy-folding-toggle-comments)
    (let* ((overlays (overlays-in (point-min) (point-max)))
           overlay)
      (should (= 3 (length overlays))))
    ;; Position
    (should (= (point) 177))))
