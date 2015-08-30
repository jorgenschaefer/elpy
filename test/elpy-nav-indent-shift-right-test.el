(ert-deftest elpy-nav-indent-shift-right-should-indent-right-single-statement ()
  (elpy-testcase ()
                 (set-buffer-string-with-point
                  "print_|_('one')")

                 (elpy-nav-indent-shift-right)

                 (should
                  (buffer-be
                   "    print_|_('one')"))))

(ert-deftest elpy-nav-indent-shift-right-should-indent-right-region ()
  (elpy-testcase ()
                 (set-buffer-string-with-point
                  "print_|_('one')"
                  "print('two')"
                  )

                 (elpy/mark-region (point)
                                   (- (point-max) 5))
                 (elpy-nav-indent-shift-right)

                 (should
                  (buffer-be
                   "    print('one')"
                   "    print('two')_|_"))))

(ert-deftest elpy-nav-indent-shift-right-should-indent-right-two-spaces ()
  (elpy-testcase ()
                 (set-buffer-string-with-point
                  "print_|_('one')")

                 (let ((current-prefix-arg '(2)))
                   (call-interactively 'elpy-nav-indent-shift-right))

                 (should
                  (buffer-be
                   "  print_|_('one')"))))
