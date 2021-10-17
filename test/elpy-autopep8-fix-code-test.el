(ert-deftest elpy-autopep8-fix-code-should-retain-line-and-column ()
  (elpy-testcase ()
                 (set-buffer-string-with-point
                  "x  =  1"
                  "y = 2_|_"
                  )
                 (elpy-autopep8-fix-code)
                 (should
                  (buffer-be
                   "x = 1"
                   "y = 2_|_"))))
