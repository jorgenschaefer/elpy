(ert-deftest elpy-insert--para-should-insert-and-fill-text ()
  (elpy-testcase ()
    (elpy-insert--para "Lorem ipsum dolor sit amet, consectetur adipisicing "
                       "elit, sed do eiusmod tempor incididunt ut labore et "
                       "dolore magnaaliqua. Ut enim ad minim veniam, quis "
                       "nostrud exercitation ullamco laboris nisi ut aliquip "
                       "ex ea commodo consequat. Duis aute irure dolor in "
                       "reprehenderit in voluptate velit esse cillum dolore "
                       "eu fugiat nulla pariatur. Excepteur sint occaecat "
                       "cupidatat non proident, sunt in culpa qui officia "
                       "deserunt mollit anim id est laborum.")
    (goto-char (point-min))
    ;; Insert
    (should (re-search-forward "adipisicing"))
    ;; Fill
    (should (re-search-forward "\n"))))
