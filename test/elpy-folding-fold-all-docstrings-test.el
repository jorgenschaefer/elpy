(ert-deftest elpy-fold-docstrings-should-fold-all-docstrings ()
  (elpy-testcase ()
    (add-to-list 'elpy-modules 'elpy-module-folding)
    (set-buffer-string-with-point
     "var1 = 45"
     ""
     "class foo(object):"
     "  def __init__(self, a, b):"
     "    \"\"\" "
     "    First docstring spawning "
     "    several lines."
     "    \"\"\""
     "    self.a = a"
     "    self.b = b"
     ""
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
     ""
     "def bar2(mess):"
     "    \"\"\" This is a oneline docstring\"\"\""
     "    print(mess)"
     ""
     "var2 = foo(var1, 4)")
    (elpy-enable)
    (python-mode)
    (elpy-folding-toggle-docstrings)
    (let* ((overlays (overlays-in (point-min) (point-max)))
           overlay)
      (should (= 6 (length overlays)))
      ;; First docstring
      (setq overlay (elpy-get-overlay-at 97 'docstring))
      (should overlay)
      (should (eq (overlay-get overlay 'hs) 'docstring))
      (should (= (overlay-start overlay) 97))
      (should (= (overlay-end overlay) 124))
      ;; Second docstring
      (setq overlay (elpy-get-overlay-at 206 'docstring))
      (should overlay)
      (should (eq (overlay-get overlay 'hs) 'docstring))
      (should (= (overlay-start overlay) 206))
      (should (= (overlay-end overlay) 280)))
    (should (= (point) 231))
    ;; Unfold
    (elpy-folding-toggle-docstrings)
    (let* ((overlays (overlays-in (point-min) (point-max)))
           overlay)
      (should (= 4 (length overlays))))
    ;; Position
    (should (= (point) 231))))
