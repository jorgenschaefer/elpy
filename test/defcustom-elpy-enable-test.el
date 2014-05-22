;;; Test that the custom options with side effects work.

(ert-deftest defcustom-elpy-enable-should-call-elpy-enable ()
  (elpy-testcase ()
    (mletf* ((elpy-enable-called nil)
             (elpy-enable () (setq elpy-enable-called t)))
      (customize-set-variable 'elpy-enable t)
      (should elpy-enable-called))))

(ert-deftest defcustom-elpy-enable-should-call-elpy-disable ()
  (elpy-testcase ()
    (mletf* ((elpy-disable-called nil)
             (elpy-disable () (setq elpy-disable-called t)))
      (customize-set-variable 'elpy-enable nil)
      (should elpy-disable-called))))
