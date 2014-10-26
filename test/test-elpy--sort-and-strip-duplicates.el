(ert-deftest elpy-check-should-fail-in-buffer-without-file ()
  (should (equal '("A" "B" "C")
                 (elpy--sort-and-strip-duplicates '("B" "C" "A" "A" "C")))))
