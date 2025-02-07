
(ert-deftest elpy-ve--display-a-variable-should-display-a-variable ()
  (elpy-testcase ()
    (let ((var '(c "['this', 'is', 'a', 'list']" "list[4]")))
      (with-temp-buffer
        (elpy-ve--display-a-variable var 20)
        (should (string= (substring-no-properties (buffer-string))
                         "--------------------------------------------------------------------------------
 c                   list[4]             ['this', 'is', 'a', 'list']
"))))))

(ert-deftest elpy-ve--display-a-variable-should-crop-horizontally ()
  (elpy-testcase ()
    (let ((var '(c "['this', 'is', 'a', 'list']" "list[4]")))
      (with-temp-buffer
        (elpy-ve--display-a-variable var 10)
        (should (string= (substring-no-properties (buffer-string))
                         "----------------------------------------
 c         list[4]   ['this', 'is', ...
"))))))

(ert-deftest elpy-ve--display-a-variable-should-crop-vertically ()
  (elpy-testcase ()
    (let ((var '(c "[['this', 'is', 'a', 'list'],
 ['this', 'is', 'a', 'list'],
 ['this', 'is', 'a', 'list'],
 ['this', 'is', 'a', 'list'],
 ['this', 'is', 'a', 'list'],
 ['this', 'is', 'a', 'list'],
 ['this', 'is', 'a', 'list'],
 ['this', 'is', 'a', 'list'],
 ['this', 'is', 'a', 'list'],
 ['this', 'is', 'a', 'list']]" "list[10]")))
      (with-temp-buffer
        (let ((elpy-ve-row-max-height 5))
          (elpy-ve--display-a-variable var 20))
        (should (string= (substring-no-properties (buffer-string))
                         "--------------------------------------------------------------------------------
 c                   list[10]            [['this', 'is', 'a', 'list'],
                                          ['this', 'is', 'a', 'list'],
                                          ['this', 'is', 'a', 'list'],
                                          ['this', 'is', 'a', 'list'],
                                          ['this', 'is', 'a', 'list'],
                                         ...
"))))))
