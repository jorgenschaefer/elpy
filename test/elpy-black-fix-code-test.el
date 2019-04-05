(defsubst elpy-black-fix-code--black-supported ()
  (if (boundp '*elpy-black-fix-code--black-supported*)
      *elpy-black-fix-code--black-supported*
    (setq *elpy-black-fix-code--black-supported*
          (not (string< (or (getenv "TRAVIS_PYTHON_VERSION")
                            (with-temp-buffer
                              (call-process "python" nil '(t t) nil "--version")
                              (goto-char (point-min))
                              (re-search-forward "\\([0-9.]+\\)" nil t)
                              (or (match-string 1) "")))
                        "3.6")))))

(when (elpy-black-fix-code--black-supported)
  (ert-deftest elpy-black-fix-code-should-retain-line-and-column ()
    (elpy-testcase ()
                   (set-buffer-string-with-point
                    "x  =  1"
                    "y = 2_|_"
                    )

                   (elpy-black-fix-code)
                   (should
                    (buffer-be
                     "x = 1"
                     "y = 2_|_")))))

(when (elpy-black-fix-code--black-supported)
  (ert-deftest elpy-black-fix-code-in-region-should-retain-line-and-column ()
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
                     )))))

(when (elpy-black-fix-code--black-supported)
  (ert-deftest elpy-black-fix-code-should-throw-error-for-invalid-code ()
    (elpy-testcase ()
                   (set-buffer-string-with-point
                    "x =_|_"
                    )
                   (should-error (elpy-black-fix-code)))))

(when (elpy-black-fix-code--black-supported)
  (ert-deftest elpy-black-fix-code-should-do-nothing-if-already-formatted ()
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
                     )))))
