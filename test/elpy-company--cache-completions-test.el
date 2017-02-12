(ert-deftest elpy-company--cache-completions-should-initialize-cache ()
  (elpy-testcase ()
    (elpy-company--cache-completions "prefix" nil)

    (should (hash-table-p elpy-company--cache))))

(ert-deftest elpy-company--cache-completions-should-add-info ()
  (elpy-testcase ()
    (require 'company)
    (elpy-company--cache-completions
     "prefix-"
     '(((name . "candidate-1")
        (annotation . "anno-1"))
       ((name . "candidate-2")
        (annotation . "anno-2"))))

    (should (equal (elpy-company--cache-annotation "prefix-candidate-1")
                   "anno-1"))
    (should (equal (elpy-company--cache-annotation "prefix-candidate-2")
                   "anno-2"))))
