;;; Test that the custom options with side effects work.

(ert-deftest defcustom-elpy-enable ()
  "The user option `elpy-enable'"
  (elpy-testcase ()
    ;; Should call elpy-enable when set to t
    (mocker-let ((elpy-enable () ((:input '()))))
      (customize-set-variable 'elpy-enable t))
    ;; Should call elpy-disable when set to nil
    (mocker-let ((elpy-disable () ((:input '()))))
      (customize-set-variable 'elpy-enable nil))))
