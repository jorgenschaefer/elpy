(ert-deftest elpy-fold-at-point-should-fold-and-unfold-functions ()
  (elpy-testcase ()
    (add-to-list 'elpy-modules 'elpy-module-folding)
    (set-buffer-string-with-point
     "var1 = 45"
     "def foo(_|_a, b):"
     "  c = a + b"
     "var2 = foo(var1, 4)")
    (elpy-enable)
    (python-mode)
    (elpy-folding-toggle-at-point)
    (let* ((overlays (apply 'nconc (overlay-lists)))
           overlay)
      (setq overlay (elpy-get-overlay-at 25 'code))
      (should overlay)
      (should (eq (overlay-get overlay 'hs) 'code))
      (should (= (overlay-start overlay) 25))
      (should (or (= (overlay-end overlay) 37)
                  (= (overlay-end overlay) 38))))
    (should (= (point) 14))
    ;; Unfold
    (elpy-folding-toggle-at-point)
    ;; Position
    (should (= (point) 14))))

(ert-deftest elpy-fold-at-point-should-fold-and-unfold-functions-from-inside ()
  (elpy-testcase ()
    (add-to-list 'elpy-modules 'elpy-module-folding)
    (set-buffer-string-with-point
     "var1 = 45"
     "def foo(a, b):"
     "  c = _|_a + b"
     "var2 = foo(var1, 4)")
    (elpy-enable)
    (python-mode)
    (elpy-folding-toggle-at-point)
    (let* ((overlays (apply 'nconc (overlay-lists)))
           overlay)
      (setq overlay (elpy-get-overlay-at 25 'code))
      (should overlay)
      (should (eq (overlay-get overlay 'hs) 'code))
      (should (= (overlay-start overlay) 25))
      (should (or (= (overlay-end overlay) 37)
                  (= (overlay-end overlay) 38))))
    (should (= (point) 14))
    ;; Unfold
    (elpy-folding-toggle-at-point)
    ;; Position
    (should (= (point) 14))))

(ert-deftest elpy-fold-at-point-should-NOT-fold-and-unfold-functions-from-after ()
  (elpy-testcase ()
    (add-to-list 'elpy-modules 'elpy-module-folding)
    (set-buffer-string-with-point
     "var1 = 45"
     "def foo(a, b):"
     "  c = a + b"
     "  return c"
     "_|_"
     "var2 = foo(var1, 4)")
    (elpy-enable)
    (python-mode)
    (elpy-folding-toggle-at-point)
    ;; Position
    (should (= (point) 49))))

(ert-deftest elpy-fold-at-point-should-fold-and-unfold-nested-functions ()
  (elpy-testcase ()
    (add-to-list 'elpy-modules 'elpy-module-folding)
    (set-buffer-string-with-point
     "var1 = 45"
     "def foo(a, b):"
     "  c = a + b"
     "  def bar(mess):"
     "    mess *= 2"
     "    print(mess)"
     "    return _|_mess"
     "var2 = foo(var1, 4)")
    (elpy-enable)
    (python-mode)
    (elpy-folding-toggle-at-point)
    (let* ((overlays (apply 'nconc (overlay-lists)))
           overlay)
      (setq overlay (elpy-get-overlay-at 54 'code))
      (should overlay)
      (should (eq (overlay-get overlay 'hs) 'code))
      (should (= (overlay-start overlay) 54))
      (should (or (= (overlay-end overlay) 100)
                  (= (overlay-end overlay) 101))))
    (should (= (point) 43))
    ;; Unfold
    (elpy-folding-toggle-at-point)
    ;; Position
    (should (= (point) 43))))

(ert-deftest elpy-fold-at-point-should-fold-and-unfold-methods ()
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
     "    print(mess)"
     "    return _|_mess"
     "var2 = foo(var1, 4)")
    (elpy-enable)
    (python-mode)
    (elpy-folding-toggle-at-point)
    (let* ((overlays (apply 'nconc (overlay-lists)))
           overlay)
      (setq overlay (elpy-get-overlay-at 104 'code))
      (should overlay)
      (should (eq (overlay-get overlay 'hs) 'code))
      (should (= (overlay-start overlay) 104))
      (should (or (= (overlay-end overlay) 150)
                  (= (overlay-end overlay) 151))))
    (should (= (point) 93))
    ;; Unfold
    (elpy-folding-toggle-at-point)
    ;; Position
    (should (= (point) 93))))

(ert-deftest elpy-fold-at-point-should-fold-and-unfold-classes ()
  (elpy-testcase ()
    (add-to-list 'elpy-modules 'elpy-module-folding)
    (set-buffer-string-with-point
     "var1 = 45"
     "class foo_|_(object):"
     "  def __init__(self, a, b):"
     "    self.a = a"
     "    self.b = b"
     "  def bar(mess):"
     "    mess *= 2"
     "    print(mess)"
     "    return mess"
     "var2 = foo(var1, 4)")
    (elpy-enable)
    (python-mode)
    (elpy-folding-toggle-at-point)
    (let* ((overlays (apply 'nconc (overlay-lists)))
           overlay)
      (setq overlay (elpy-get-overlay-at 29 'code))
      (should overlay)
      (should (eq (overlay-get overlay 'hs) 'code))
      (should (= (overlay-start overlay) 29))
      (should (or (= (overlay-end overlay) 150)
                  (= (overlay-end overlay) 151))))
    (should (= (point) 16))
    ;; Unfold
    (elpy-folding-toggle-at-point)
    ;; Position
    (should (= (point) 16))))
