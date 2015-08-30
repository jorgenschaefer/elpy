(ert-deftest elpy-nav-indent-shift-left-should-indent-left-single-statement ()
  (elpy-testcase ()
                 (set-buffer-string-with-point
                  "    print_|_('one')")

                 (elpy-nav-indent-shift-left)

                 (should
                  (buffer-be
                   "print_|_('one')"))))

(ert-deftest elpy-nav-indent-shift-left-should-indent-left-region ()
  (elpy-testcase ()
                 (set-buffer-string-with-point
                  "    print_|_('one')"
                  "    print('two')"
                  )

                 (elpy/mark-region (point)
                                   (- (point-max) 5))
                 (elpy-nav-indent-shift-left)

                 (should
                  (buffer-be
                   "print('one')"
                   "print('two')_|_"))))

(ert-deftest elpy-nav-indent-shift-left-should-indent-left-two-spaces ()
  (elpy-testcase ()
                 (set-buffer-string-with-point
                  "    print_|_('one')")

                 (let ((current-prefix-arg '(2)))
                   (call-interactively 'elpy-nav-indent-shift-left))

                 (should
                  (buffer-be
                   "  print_|_('one')"))))
