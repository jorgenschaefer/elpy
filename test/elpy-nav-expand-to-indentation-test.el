(ert-deftest elpy-nav-expand-to-indentation-should-select-surrounding-lines ()
  (elpy-testcase ()
                 (set-buffer-string-with-point
                  "def foo():"
                  "    if _|_True:"
                  "        x = 1"
                  )
                 (elpy-nav-expand-to-indentation)
                 (should mark-active)
                 (should (= (region-beginning) 12))
                 (should (= (region-end) 38))))


(ert-deftest elpy-nav-expand-to-indentation-should-restore-original-position-on-quit ()
  (elpy-testcase ()
                 (set-buffer-string-with-point
                  "def foo():"
                  "    if _|_True:"
                  "        x = 1"
                  )
                 (elpy-nav-expand-to-indentation)
                 (keyboard-quit)
                 (should
                  (buffer-be
                   "def foo():"
                   "    if _|_True:"
                   "        x = 1"
                   ))))
