(ert-deftest elpy--sort-and-strip-duplicates-should-sort-and-strip-duplicates ()
  (should (equal '("A" "B" "C")
                 (elpy--sort-and-strip-duplicates '("B" "C" "A" "A" "C")))))

(ert-deftest elpy--sort-and-strip-and-duplicates-should-work-on-empty-list ()
  (should (equal nil
                 (elpy--sort-and-strip-duplicates nil))))
