(ert-deftest elpy-version-should-emit-the-elpy-version ()
  (elpy-testcase ()
    (mletf* ((output nil)
             (message (fmt &rest args) (setq output (apply 'format fmt args))))

      (elpy-version)

      (should (equal output
                     (format "Elpy %s (use M-x elpy-config for details)"
                             elpy-version))))))
