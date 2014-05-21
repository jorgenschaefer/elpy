;;; This is a highly interactive function. It's annoyingly difficult
;;; to test those, so just check that it works.

(ert-deftest elpy-doc ()
  (elpy-testcase ()
    (python-mode)
    (elpy-mode)
    (insert "sys")
    (elpy-doc)
    (with-current-buffer "*Python Doc*"
      (should (re-search-forward "This module provides access")))))
