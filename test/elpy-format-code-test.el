(ert-deftest elpy-should-format-code-with-default-formatter ()
  (elpy-testcase ()
     (set-buffer-string-with-point
      "_|_y=  2"
      "z=3"
      "x  = 3")

     (elpy-format-code)

     (should
      (buffer-be
       "_|_y = 2"
       "z = 3"
       "x = 3"
       ))))

(ert-deftest elpy-should-format-code-with-yapf-formatter ()
      (let ((elpy-formatter 'yapf)
            backend-called)
        (elpy-testcase ()
           (set-buffer-string-with-point
            "_|_y=  2"
            "z=3"
            "x  = 3")

           (mletf* ((elpy-yapf-fix-code ()
                      (setq backend-called "yapf")))
             (call-interactively 'elpy-format-code))

           (should (string= backend-called "yapf"))
           )))

(ert-deftest elpy-should-format-code-with-autopep8-formatter ()
  (let ((elpy-formatter 'autopep8)
        backend-called)
    (elpy-testcase ()
       (set-buffer-string-with-point
        "_|_y=  2"
        "z=3"
        "x  = 3")

       (mletf* ((elpy-autopep8-fix-code ()
                   (setq backend-called "autopep8")))
         (call-interactively 'elpy-format-code))

       (should (string= backend-called "autopep8"))
       )))

(ert-deftest elpy-should-format-code-with-black-formatter ()
  (let ((elpy-formatter 'black)
        backend-called)
    (elpy-testcase ()
       (set-buffer-string-with-point
        "_|_y=  2"
        "z=3"
        "x  = 3")

       (mletf* ((elpy-black-fix-code ()
                   (setq backend-called "black"))
                (elpy-config--package-available-p (formatter)
                   t))
         (call-interactively 'elpy-format-code))

       (should (string= backend-called "black"))
       )))
