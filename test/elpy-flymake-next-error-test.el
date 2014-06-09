;;; This function is not complicated and as such silly to test (we'd
;;; test flymake-goto-next-error, not this function). So, just call it
;;; to make sure it does not throw an error.

(ert-deftest elpy-flymake-next-error ()
  (elpy-testcase ()
    (elpy-flymake-next-error)))
