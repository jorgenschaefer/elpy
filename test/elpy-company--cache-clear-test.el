(ert-deftest elpy-company--cache-clear-should-initialize-cache ()
  (elpy-testcase ()
    (let ((elpy-company--cache nil))

      (elpy-company--cache-clear)

      (should-not (null elpy-company--cache)))))

(ert-deftest elpy-company--cache-clear-should-clear-cache ()
  (elpy-testcase ()
    (let ((elpy-company--cache (make-hash-table :test #'equal)))
      (puthash "foo" "foo" elpy-company--cache)

      (elpy-company--cache-clear)

      (should (eq 'frob (gethash "foo" elpy-company--cache 'frob))))))
