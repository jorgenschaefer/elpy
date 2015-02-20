(ert-deftest elpy-rpc--handle-unexpected-line-should-display-general-error ()
  (elpy-testcase ()
    (elpy-rpc--handle-unexpected-line "The world is dark and cruel\n")

    (with-current-buffer "*Elpy Output*"
      (goto-char (point-min))
      (re-search-forward "The world is dark and cruel"))))

(ert-deftest elpy-rpc--handle-unexpected-line-should-append-to-buffer ()
  (elpy-testcase ()
    (elpy-rpc--handle-unexpected-line "The world is dark\n")
    (elpy-rpc--handle-unexpected-line "and cruel\n")

    (with-current-buffer "*Elpy Output*"
      (goto-char (point-min))
      (re-search-forward "The world is dark\nand cruel"))))
