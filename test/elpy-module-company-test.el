(ert-deftest elpy-module-company ()
  (elpy-testcase ()
    (elpy-module-company 'global-init)
    (python-mode)
    (elpy-mode)
    (elpy-module-company 'buffer-init)

    (should company-mode)
    (should (< (abs (- company-idle-delay 0.1))
               0.001))
    (should (eq company-tooltip-align-annotations t))
    (should (member 'elpy-company-backend company-backends))

    (elpy-module-company 'buffer-stop)

    (should-not company-mode)))
