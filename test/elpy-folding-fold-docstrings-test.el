(ert-deftest elpy-fold-at-point-should-fold-multiline-docstrings ()
  (elpy-testcase ()
    (add-to-list 'elpy-modules 'elpy-module-folding)
    (set-buffer-string-with-point
     "var1 = 45"
     "class foo(object):"
     "  def __init__(self, a, b):"
     "    self.a = a"
     "    self.b = b"
     "  def bar(mess):"
     "    \"\"\" This is docstring for BAR"
     ""
     "    And here are the pa_|_rameters"
     ""
     "    And there the return values"
     "    \"\"\""
     "    mess *= 2"
     "    print(mess)"
     "    return mess"
     "var2 = foo(var1, 4)")
    (python-mode)
    (elpy-mode)
    (elpy-folding-toggle-at-point)
    (let* ((overlays (overlays-in (point-min) (point-max)))
           overlay)
      (setq overlay (elpy-get-overlay-at 138 'docstring))
      (should overlay)
      (should (eq (overlay-get overlay 'hs) 'docstring))
      (should (= (overlay-start overlay) 138))
      (should (= (overlay-end overlay) 212)))
    (should (= (point) 109))
    ;; Unfold
    (elpy-folding-toggle-at-point)
    ;; Position
    (should (= (point) 109))))

(ert-deftest elpy-fold-at-point-should-fold-multiline-docstrings-2 ()
  (elpy-testcase ()
    (add-to-list 'elpy-modules 'elpy-module-folding)
    (set-buffer-string-with-point
     "var1 = 45"
     "class foo(object):"
     "  def __init__(self, a, b):"
     "    self.a = a"
     "    self.b = b"
     "  def bar(mess):"
     "    \"\"\""
     "    This is docstring for BAR"
     ""
     "    And here are the pa_|_rameters"
     ""
     "    And there the return values"
     "    \"\"\""
     "    mess *= 2"
     "    print(mess)"
     "    return mess"
     "var2 = foo(var1, 4)")
    (python-mode)
    (elpy-mode)
    (elpy-folding-toggle-at-point)
    (let* ((overlays (overlays-in (point-min) (point-max)))
           overlay)
      (setq overlay (elpy-get-overlay-at 142 'docstring))
      (should overlay)
      (should (eq (overlay-get overlay 'hs) 'docstring))
      (should (= (overlay-start overlay) 142))
      (should (= (overlay-end overlay) 216)))
    (should (= (point) 117))
    ;; Unfold
    (elpy-folding-toggle-at-point)
    ;; Position
    (should (= (point) 117))))

(ert-deftest elpy-fold-at-point-should-NOT-fold-oneline-docstrings ()
  (elpy-testcase ()
    (add-to-list 'elpy-modules 'elpy-module-folding)
    (set-buffer-string-with-point
     "var1 = 45"
     "class foo(object):"
     "  def __init__(self, a, b):"
     "    self.a = a"
     "    self.b = b"
     "  def bar(mess):"
     "    \"\"\" This is do_|_cstring for BAR \"\"\""
     "    mess *= 2"
     "    print(mess)"
     "    return mess"
     "var2 = foo(var1, 4)")
    (python-mode)
    (elpy-mode)
    (elpy-folding-toggle-at-point)
    (let* ((overlays (overlays-in (point-min) (point-max))))
      (dolist (overlay overlays)
        (should-not (overlay-get overlay 'hs))))))

(ert-deftest elpy-fold-at-point-should-NOT-fold-strings ()
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
     "    \" This is just _|_a string\""
     "    print(mess)"
     "    return mess"
     "var2 = foo(var1, 4)")
    (python-mode)
    (elpy-mode)
    (let ((nmb-overlays (length (overlays-in (point-min) (point-max)))))
      (elpy-folding-toggle-at-point)
      (let* ((overlays (overlays-in (point-min) (point-max)))
             overlay)
        (dolist (overlay overlays)
          (should-not (eq (overlay-get overlay 'hs) 'docstring)))))))

(ert-deftest elpy-fold-at-point-should-NOT-fold-strings-2 ()
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
     "    \"\"\" This is just _|_a string\"\"\""
     "    print(mess)"
     "    return mess"
     "var2 = foo(var1, 4)")
    (python-mode)
    (elpy-mode)
    (let ((nmb-overlays (length (overlays-in (point-min) (point-max)))))
      (elpy-folding-toggle-at-point)
      (let* ((overlays (overlays-in (point-min) (point-max)))
             overlay)
        (dolist (overlay overlays)
          (should-not (eq (overlay-get overlay 'hs) 'docstring)))))))

(ert-deftest elpy-fold-at-point-should-NOT-fold-strings-3 ()
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
     "    mess = \"\"\" This is just _|_a string\"\"\""
     "    print(mess)"
     "    return mess"
     "var2 = foo(var1, 4)")
    (python-mode)
    (elpy-mode)
    (elpy-folding-toggle-at-point)
    (let* ((overlays (overlays-in (point-min) (point-max)))
           overlay)
      (setq overlay (elpy-get-overlay-at 104 'code))
      (should overlay)
      (should (eq (overlay-get overlay 'hs) 'code))
      (should (= (overlay-start overlay) 104))
      (should (or (= (overlay-end overlay) 190)
                  (= (overlay-end overlay) 191))))
    (should (= (point) 93))))

(ert-deftest elpy-fold-at-point-should-NOT-fold-strings-4 ()
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
     "    mess = \"\"\""
     "    This is just _|_a string"
     ""
     "    Even if it is multinline"
     "    \"\"\""
     "    print(mess)"
     "    return mess"
     "var2 = foo(var1, 4)")
    (python-mode)
    (elpy-mode)
    (elpy-folding-toggle-at-point)
    (let* ((overlays (overlays-in (point-min) (point-max)))
           overlay)
      (setq overlay (elpy-get-overlay-at 104 'code))
      (should overlay)
      (should (eq (overlay-get overlay 'hs) 'code))
      (should (= (overlay-start overlay) 104))
      (should (or (= (overlay-end overlay) 229)
                  (= (overlay-end overlay) 230))))
    (should (= (point) 93))))
