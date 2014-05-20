(ert-deftest elpy-test-should-save-some-buffers ()
  (elpy-testcase ()
    (mocker-let ((save-some-buffers
                  ()
                  ((:input '() :output nil)))
                 (nosetests-all
                  ()
                  ((:input '() :output nil))))
      (elpy-test))))

(ert-deftest elpy-test-should-test-all-with-no-args ()
  (elpy-testcase ()
    (mocker-let ((nosetests-all
                  ()
                  ((:input '() :output nil))))
      (elpy-test))))

(ert-deftest elpy-test-should-test-one-with-single-arg ()
  (elpy-testcase ()
    (mocker-let ((nosetests-one
                  ()
                  ((:input '() :output nil))))
      (elpy-test 4))))

(ert-deftest elpy-test-should-test-module-with-two-args ()
  (elpy-testcase ()
    (mocker-let ((nosetests-module
                  ()
                  ((:input '() :output nil))))
      (elpy-test 16))))
