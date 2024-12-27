;;; -*-coding: utf-8-*-

(ert-deftest elpy-rpc-get-pip-dependencies-should-return-packages-dependent-on-current-python-version ()
  (elpy-testcase ()
                 (should (equal '("jedi" "flake8" "autopep8" "yapf") (elpy-rpc--get-pip-dependencies "3.5.0")))
                 (should (equal '("black" "jedi" "flake8" "autopep8" "yapf") (elpy-rpc--get-pip-dependencies "3.6.0")))
                 (should (equal '("setuptools" "black" "jedi" "flake8" "autopep8" "yapf") (elpy-rpc--get-pip-dependencies "3.12.0")))))
