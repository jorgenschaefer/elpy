(ert-deftest elpy-nav-normalize-region-should-select-partially-selected-line ()
  (elpy-testcase ()
                 (set-buffer-string-with-point
                  "print_|_('one')"
                  "print('two')"
                  )

                 (elpy/mark-region (point)
                                   (- (point-max) 5))
                 (elpy-nav-normalize-region)
                 (should mark-active)
                 (should (= (region-beginning) 1))
                 (should (= (region-end) 26))))


(ert-deftest elpy-nav-normalize-region-should-not-select-not-selected-line ()
  (elpy-testcase ()
                 (set-buffer-string-with-point
                  "print_|_('one')"
                  "print('two')"
                  )

                 (elpy/mark-region (point)
                                   (- (point-max) 13))
                 (elpy-nav-normalize-region)
                 (should mark-active)
                 (should (= (region-beginning) 1))
                 (should (= (region-end) 14))))
