(ert-deftest elpy-insert--popup-should-popup-window ()
  (elpy-testcase ()
    (elpy-insert--popup "*Test Popup*"
      (insert "Foo!"))

    (should (get-buffer-window "*Test Popup*"))
    (should (string-match "Foo!"
                          (with-current-buffer "*Test Popup*"
                            (buffer-string))))))
