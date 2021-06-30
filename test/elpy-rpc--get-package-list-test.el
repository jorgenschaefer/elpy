;;; -*-coding: utf-8-*-

(ert-deftest elpy-rpc-get-package-list-should-return-packages ()
  (elpy-testcase ()
    (let ((package-list (elpy-rpc--get-package-list)))
      (should (string-match "jedi" (apply 'concat package-list)))
      (should (string-match "yapf" (apply 'concat package-list))))))
