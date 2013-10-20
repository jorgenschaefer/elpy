(ert-deftest test-elpy-refactor-mode ()
  "Test that we can run `elpy-refactor-mode' at all."
  (with-temp-buffer
    (elpy-refactor-mode)))
