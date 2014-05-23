(ert-deftest elpy-company--cache-completions-should-initialize-cache ()
  (elpy-testcase ()
    (elpy-company--cache-completions "prefix" nil)

    (should (hash-table-p elpy-company-candidate-cache))))

(ert-deftest elpy-company--cache-completions-should-add-docstrings ()
  (elpy-testcase ()
    (elpy-company--cache-completions
     "prefix-"
     '(("candidate-1" "doc-1")
       ("candidate-2" "doc-2")))

    (should (hash-table-p elpy-company-candidate-cache))
    (should (equal (gethash "prefix-candidate-1" elpy-company-candidate-cache)
                   "doc-1"))
    (should (equal (gethash "prefix-candidate-2" elpy-company-candidate-cache)
                   "doc-2"))))
