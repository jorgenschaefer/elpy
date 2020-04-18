(ert-deftest elpy-fold-at-point-should-fold-and-unfold-on-fringe-click ()
  (elpy-testcase ()
    (add-to-list 'elpy-modules 'elpy-module-folding)
    (set-buffer-string-with-point
     "var1 = 45"
     "def foo(_|_a, b):"
     "  c = a + b"
     "var2 = foo(var1, 4)")
    (python-mode)
    (elpy-mode)
    (mletf* ((mouse-set-point (event) (goto-char (point))))
      (elpy-folding--click-fringe nil))
    (let* ((overlays (apply 'nconc (overlay-lists)))
           overlay)
      (should (= 2 (length overlays)))
      (setq overlay (elpy-get-overlay-at 25 'code))
      (should overlay)
      (should (eq (overlay-get overlay 'hs) 'code))
      (should (= (overlay-start overlay) 25))
      (should (or (= (overlay-end overlay) 37)
                  (= (overlay-end overlay) 38))))
    (should (= (point) 14))
    ;; Unfold
    (mletf* ((mouse-set-point (event) (goto-char (point))))
      (elpy-folding--click-fringe nil))
    (let* ((overlays (apply 'nconc (overlay-lists)))
           overlay)
      (should (= 1 (length overlays))))
    ;; Position
    (should (= (point) 14))))

(ert-deftest elpy-fold-at-point-should-unfold-on-indicator-click ()
  (elpy-testcase ()
    (add-to-list 'elpy-modules 'elpy-module-folding)
    (set-buffer-string-with-point
     "var1 = 45"
     "def foo(_|_a, b):"
     "  c = a + b"
     "var2 = foo(var1, 4)")
    (python-mode)
    (elpy-mode)
    (mletf* ((mouse-set-point (event) (goto-char (point))))
      (elpy-folding--click-fringe nil))
    (let* ((overlays (apply 'nconc (overlay-lists)))
           overlay)
      (should (= 2 (length overlays)))
      (setq overlay (elpy-get-overlay-at 25 'code))
      (should overlay)
      (should (eq (overlay-get overlay 'hs) 'code))
      (should (= (overlay-start overlay) 25))
      (should (or (= (overlay-end overlay) 37)
                  (= (overlay-end overlay) 38))))
    (should (= (point) 14))
    ;; Unfold
    (end-of-line)
    (mletf* ((posn-window (position) (get-buffer-window))
             (posn-point (position) (point))
             (window-buffer (window) (current-buffer)))
      (elpy-folding--click-text nil))
    (let* ((overlays (apply 'nconc (overlay-lists)))
           overlay)
      (should (= 1 (length overlays))))
    ;; Position
    (should (= (point) 25))))
