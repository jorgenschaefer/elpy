;;; -*-coding: utf-8-*-

(ert-deftest elpy-rpc-get-version-should-return-python-version ()
  (elpy-testcase ()
    (should
     (string-match "[0-9]+\\.[0-9]+\\.[0-9]+"
                   (elpy-rpc--get-python-version)))))
