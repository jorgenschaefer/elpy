(ert-deftest elpy-black-fix-code-should-retain-line-and-column ()
  (let* ((pyversion (getenv "TRAVIS_PYTHON_VERSION"))
         (black-not-supported (string< pyversion "3.6")))
    (unless black-not-supported
      (elpy-testcase ()
                     (set-buffer-string-with-point
                      "x  =  1"
                      "y = 2_|_"
                      )

                     (elpy-black-fix-code)
                     (should
                      (buffer-be
                       "x = 1"
                       "y = 2_|_"))))))

(ert-deftest elpy-black-fix-code-in-region-should-retain-line-and-column ()
  (let* ((pyversion (getenv "TRAVIS_PYTHON_VERSION"))
         (black-not-supported (string< pyversion "3.6")))
    (unless black-not-supported
      (elpy-testcase ()
                     (set-buffer-string-with-point
                      "_|_y =  2"
                      "z = 3"
                      "x = 3"
                      )
                     (elpy/mark-region (point) 13)

                     (elpy-black-fix-code)

                     (should (not mark-active))
                     (should
                      (buffer-be
                       "y = 2"
                       "z = 3"
                       "_|_x = 3"
                       ))))))

(ert-deftest elpy-black-fix-code-should-throw-error-for-invalid-code ()
  (let* ((pyversion (getenv "TRAVIS_PYTHON_VERSION"))
         (black-not-supported (string< pyversion "3.6")))
    (unless black-not-supported
      (elpy-testcase ()
                     (set-buffer-string-with-point
                      "x =_|_"
                      )
                     (should-error (elpy-black-fix-code))))))
