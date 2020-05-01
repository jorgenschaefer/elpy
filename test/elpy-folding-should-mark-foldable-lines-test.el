(ert-deftest elpy-folding-module-should-mark-foldable-lines ()
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
    (let* ((overlays (overlays-in (point-min) (point-max)))
           overlay)
      (should (= 3 (length overlays)))
      ;; Second mark
      (setq overlay (elpy-get-overlay-at 11 nil))
      (should overlay)
      (should (eq (overlay-get overlay 'hs) nil))
      (should (= (overlay-start overlay) 11))
      (should (= (overlay-end overlay) 29))
      ;; Second mark
      (setq overlay (elpy-get-overlay-at 30 nil))
      (should overlay)
      (should (eq (overlay-get overlay 'hs) nil))
      (should (= (overlay-start overlay) 30))
      (should (= (overlay-end overlay) 57))
      ;; Third mark
      (setq overlay (elpy-get-overlay-at 88 nil))
      (should overlay)
      (should (eq (overlay-get overlay 'hs) nil))
      (should (= (overlay-start overlay) 88))
      (should (= (overlay-end overlay) 104)))))
