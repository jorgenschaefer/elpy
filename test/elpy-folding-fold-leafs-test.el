
(ert-deftest elpy-fold-leafs-should-fold-them ()
  (elpy-testcase ()
    (add-to-list 'elpy-modules 'elpy-module-folding)
    (set-buffer-string-with-point
     "var1 = 45"
     "class foo(object):"
     "  def __init__(self, a, b):"
     "    self.a = a"
     "    self.b = b"
     "  def bar(mess):"
     "    mess *= 2"
     "    def bar2(mess):"
     "      print(_|_mess)"
     "    return mess"
     "var2 = foo(var1, 4)")
    (python-mode)
    (elpy-mode)
    (elpy-folding-hide-leafs)
    (let* ((overlays (overlays-in (point-min) (point-max)))
           overlay)
      (should (= 6 (length overlays)))
      (setq overlay (nth 3 overlays))
      (should (eq (overlay-get overlay 'hs) 'code))
      (should (= (overlay-start overlay) 57))
      (should (or (= (overlay-end overlay) 87)
                  (= (overlay-end overlay) 88)))
      (setq overlay (nth 0 overlays))
      (should (eq (overlay-get overlay 'hs) 'code))
      (should (= (overlay-start overlay) 138))
      (should (or (= (overlay-end overlay) 156)
                  (= (overlay-end overlay) 157))))

    (should (= (point) 151))
    ;; Unfold
    (hs-show-all)
    (let* ((overlays (overlays-in (point-min) (point-max)))
           overlay)
      (should (= 4 (length overlays))))
    ;; Position
    (should (= (point) 151))))
