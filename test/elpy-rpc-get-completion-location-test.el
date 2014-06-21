(ert-deftest elpy-rpc-get-completion-location ()
  (elpy-testcase ()
    (mletf* ((called-args nil)
             (elpy-rpc (&rest args) (setq called-args args)))

      (elpy-rpc-get-completion-location "foo")

      (should (equal called-args
                     '("get_completion_location"
                       ("foo")
                       nil nil))))))
