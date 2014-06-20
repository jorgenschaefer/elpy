(ert-deftest elpy-promise-buffer ()
  (elpy-testcase ()
    (let ((promise (elpy-promise "success" "error"))
          (buf (current-buffer)))
      (with-temp-buffer
        (should (equal (elpy-promise-buffer promise)
                       buf))))))
