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

