
(ert-deftest elpy-ve--display-a-detailled-variable-should-display-properly ()
  (elpy-testcase ()
    (let ((var '(c "['this', 'is', 'a', 'list']" "list[4]")))
      (with-temp-buffer
        (elpy-ve--display-a-variable var 20)
        (should (string= (substring-no-properties (buffer-string))
                         "--------------------------------------------------------------------------------
 c                   list[4]             ['this', 'is', 'a', 'list']
"))))))
