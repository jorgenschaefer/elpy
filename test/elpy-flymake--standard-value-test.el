(ert-deftest elpy-flymake--standard-value-should-get-standard-value ()
  (elpy-testcase ()
    (require 'python)
    (cond
     ((version< emacs-version "24.3")
      (should (equal (elpy-flymake--standard-value 'python-check-command)
                     "pychecker --stdlib")))
     ((version< emacs-version "25.0")
      (should (equal (elpy-flymake--standard-value 'python-check-command)
                     "pyflakes")))
     (t
      (should (equal (elpy-flymake--standard-value 'python-check-command)
                     (executable-find "pyflakes")))))))
