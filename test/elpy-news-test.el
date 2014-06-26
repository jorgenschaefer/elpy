(ert-deftest elpy-news-should-insert-file ()
  (elpy-testcase ()
    (elpy-news)
    (with-current-buffer "*Elpy News*"
      (should (get-buffer-window (current-buffer)))
      (should (re-search-forward "New in Elpy 1.4.0")))))
