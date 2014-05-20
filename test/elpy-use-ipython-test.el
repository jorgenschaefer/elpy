;;; This function is not complicated, it just sets a whole bunch of
;;; variables. We can not actually test it, as ipython does not run in
;;; batch mode. So we just test that it does not throw an error.

(ert-deftest elpy-use-ipython ()
  (elpy-testcase ()
    (elpy-use-ipython)
    ;; Restore old values
    (elpy-use-cpython)))
