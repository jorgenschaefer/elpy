(ert-deftest elpy-open-and-indent-line-above ()
  (elpy-testcase ()
    (elpy-enable)
    (python-mode)
    (insert-source
     "def foo():"
     "    x = 5")
    (goto-char (point-min))
    (re-search-forward "x")
    (elpy-open-and-indent-line-above)
    (should (equal (buffer-string)
                   "def foo():\n    \n    x = 5\n"))
    (should (looking-at "\n    x = 5"))))
