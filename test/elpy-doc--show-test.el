(ert-deftest elpy-doc--show ()
  (elpy-testcase ()
    (elpy-doc--show "Bla Bla\nFFOOOO\n")
    (with-current-buffer "*Python Doc*"
      (goto-char (point-min))
      (should (looking-at "Bla Bla"))
      (forward-line 1)
      (should (eq (get-text-property (point) 'face)
                  'bold)))))
