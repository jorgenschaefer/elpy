;; This is deliberately short. The function is primarily concerned
;; with output. We will test its constituent functions later, and just
;; make sure it doesn't throw an error here.

(ert-deftest elpy-config-should-not-fail ()
  (elpy-testcase ()
    (elpy-config)))

(ert-deftest elpy-config-should-not-fail-without-virtualenv ()
  (elpy-testcase ()
    (let ((process-environment (cons "VIRTUAL_ENV" process-environment)))
      (elpy-config))))
