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

(ert-deftest elpy-black-fix-code-should-do-nothing-if-already-formatted ()
  (let* ((pyversion (getenv "TRAVIS_PYTHON_VERSION"))
         (black-not-supported (string< pyversion "3.6")))
    (unless black-not-supported
      (elpy-testcase ()
                     (set-buffer-string-with-point
                      "_|_y =  2"
                      "z = 3"
                      "x = 3"
                      )
                     (elpy-black-fix-code)
                     (should
                      (buffer-be
                       "_|_y = 2"
                       "z = 3"
                       "x = 3"
                       ))
                     (elpy-black-fix-code)
                     (should
                      (buffer-be
                       "_|_y = 2"
                       "z = 3"
                       "x = 3"
                       ))))))

(ert-deftest elpy-black-fix-code-should-follow-pyproject-config ()
  (let* ((pyversion (getenv "TRAVIS_PYTHON_VERSION"))
         (black-not-supported (string< pyversion "3.6")))
    (unless black-not-supported
      (elpy-testcase ()
         ;; create pyproject.toml
         (with-current-buffer (find-file-noselect "pyproject.toml")
           (insert "[tool.black]\nline-length = 10")
           (save-buffer))
         (set-buffer-string-with-point
          "_|_print(1, 2, 3, 4, 5, 6, 7)"
          "x, y, z, a, b, c = 1, 2, 3, 4, 5, 6"
          )
         (elpy-black-fix-code)
         (should
          (buffer-be
           "_|_print("
           "    1,"
           "    2,"
           "    3,"
           "    4,"
           "    5,"
           "    6,"
           "    7,"
           ")"
           "x, y, z, a, b, c = ("
           "    1,"
           "    2,"
           "    3,"
           "    4,"
           "    5,"
           "    6,"
           ")"))
         (delete-file "pyproject.toml")
         ))))
