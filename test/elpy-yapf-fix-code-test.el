(ert-deftest elpy-yapf-fix-code-should-retain-line-and-column ()
  (let* ((pyversion (elpy-get-python-version))
         (yapf-not-supported (or (string< pyversion "2.7")
                                 (and (not (string< pyversion "3.0"))
                                      (string< pyversion "3.4")))))
    (unless yapf-not-supported
      (elpy-testcase ()
                     (set-buffer-string-with-point
                      "x  =  1"
                      "y = 2_|_"
                      )

                     (elpy-yapf-fix-code)
                     (should
                      (buffer-be
                       "x = 1"
                       "y = 2_|_"))))))

(ert-deftest elpy-yapf-fix-code-in-region-should-retain-line-and-column ()
  (let* ((pyversion (elpy-get-python-version))
         (yapf-not-supported (or (string< pyversion "2.7")
                                 (and (not (string< pyversion "3.0"))
                                      (string< pyversion "3.4")))))
    (unless yapf-not-supported
      (elpy-testcase ()
                     (set-buffer-string-with-point
                      "_|_y =  2"
                      "z = 3"
                      "x = 3"
                      )
                     (elpy/mark-region (point) 13)

                     (elpy-yapf-fix-code)

                     (should (not mark-active))
                     (should
                      (buffer-be
                       "y = 2"
                       "z = 3"
                       "_|_x = 3"
                       ))))))

(ert-deftest elpy-yapf-fix-code-should-throw-error-for-invalid-code ()
  (let* ((pyversion (elpy-get-python-version))
         (yapf-not-supported (or (string< pyversion "2.7")
                                 (and (not (string< pyversion "3.0"))
                                      (string< pyversion "3.4")))))
    (unless yapf-not-supported
      (elpy-testcase ()
                     (set-buffer-string-with-point
                      "x =_|_"
                      )
                     (should-error (elpy-yapf-fix-code))))))
