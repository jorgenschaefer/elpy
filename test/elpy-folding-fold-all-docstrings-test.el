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

(ert-deftest elpy-fold-docstrings-handle-comments ()
  (elpy-testcase ()
    (add-to-list 'elpy-modules 'elpy-module-folding)
    (set-buffer-string-with-point
     "var1 = 45"
     ""
     "class foo(object):"
     "  def __init__(self, a, b): # FIXME: \"strange\" \'bug\'."
     "    #  By \"strange\" I \'mean\' ..."
     "    \"\"\" "
     "    First docstring spawning "
     "    several li_|_nes."
     "    \"\"\""
     "    self.a = a"
     "    self.b = b"
     ""
     "var2 = foo(var1, 4)")
    (elpy-enable)
    (python-mode)
    (elpy-folding-toggle-docstrings)
    (let* ((overlays (overlays-in (point-min) (point-max)))
           overlay)
      (setq overlay (elpy-get-overlay-at 156 'docstring))
      (should overlay)
      (should (eq (overlay-get overlay 'hs) 'docstring))
      (should (= (overlay-start overlay) 156))
      (should (= (overlay-end overlay) 183)))
    (should (= (point) 171))
    ;; Unfold
    (elpy-folding-toggle-docstrings)
    ;; Position
    (should (= (point) 171))))

(ert-deftest elpy-fold-docstrings-handle-class-and-module-docstring ()
  (elpy-testcase ()
    (add-to-list 'elpy-modules 'elpy-module-folding)
    (set-buffer-string-with-point
     "\'\'\'This is a module-level docstring with different delimiters."
     ""
     "Apart from this, we should be able to handle docstrings of classes.\'\'\'"
     "var1 = 45"
     ""
     "class foo(object):"
     "  \'\'\'This class might seem simple, but don't stop reading."
     ""
     "  What did you expe_|_ct - there is no magic!"
     "  \'\'\'"
     "  def __init__(self, a, b): # FIXME: \"strange\" \'bug\'."
     "    #  By \"strange\" I \'mean\' ..."
     "    \"\"\" "
     "    First docstring spawning "
     "    several lines."
     "    \"\"\""
     "    self.a = a"
     "    self.b = b"
     ""
     "note = \"\"\"Clearly, this is not a docstring"
     "and folding it would be bad."
     "But we are on the safe side (well, maybe).\"\"\""
     ""
     "var2 = foo(var1, 4)")
    (elpy-enable)
    (python-mode)
    (elpy-folding-toggle-docstrings)
    (let* ((overlays (overlays-in (point-min) (point-max)))
           overlay)
      ;; Module-level docstring
      (setq overlay (elpy-get-overlay-at 63 'docstring))
      (should overlay)
      (should (eq (overlay-get overlay 'hs) 'docstring))
      (should (= (overlay-start overlay) 63))
      (should (= (overlay-end overlay) 135))
      ;; Class docstring
      (setq overlay (elpy-get-overlay-at 224 'docstring))
      (should overlay)
      (should (eq (overlay-get overlay 'hs) 'docstring))
      (should (= (overlay-start overlay) 224))
      (should (= (overlay-end overlay) 274))
      ;; Method docstring
      (setq overlay (elpy-get-overlay-at 400 'docstring))
      (should overlay)
      (should (eq (overlay-get overlay 'hs) 'docstring))
      (should (= (overlay-start overlay) 400))
      (should (= (overlay-end overlay) 427))
      )
    (should (= (point) 245))
    ;; Unfold
    (elpy-folding-toggle-docstrings)
    ;; Position
    (should (= (point) 245))))
