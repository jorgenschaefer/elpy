(ert-deftest elpy-rpc-set-backend ()
  (elpy-testcase ()
    (mletf* ((called-args nil)
             (elpy-rpc (&rest args) (setq called-args args)))

      (elpy-rpc-set-backend "test")

      (should (equal called-args
                     '("set_backend"
                       ("test")
                       nil nil))))))

(ert-deftest elpy-set-backend-should-be-an-alias ()
  (elpy-testcase ()
    (should (equal (symbol-function 'elpy-set-backend)
                   'elpy-rpc-set-backend))))
