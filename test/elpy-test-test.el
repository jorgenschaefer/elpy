(ert-deftest elpy-test-should-save-some-buffers ()
  (elpy-testcase ()
    (mletf* ((save-some-buffers-called nil)
             (save-some-buffers () (setq save-some-buffers-called t))
             (nosetests-all () nil))

      (elpy-test)

      (should save-some-buffers-called))))

(ert-deftest elpy-test-should-test-all-with-no-args ()
  (elpy-testcase ()
    (mletf* ((nosetests-all-called nil)
             (nosetests-all () (setq nosetests-all-called t)))

      (elpy-test)

      (should nosetests-all-called))))

(ert-deftest elpy-test-should-test-one-with-single-arg ()
  (elpy-testcase ()
    (mletf* ((nosetests-one-called nil)
             (nosetests-one () (setq nosetests-one-called t)))

      (elpy-test 4)

      (should nosetests-one-called))))

(ert-deftest elpy-test-should-test-module-with-two-args ()
  (elpy-testcase ()
    (mletf* ((nosetests-module-called nil)
             (nosetests-module () (setq nosetests-module-called t)))

      (elpy-test 16)

      (should nosetests-module-called))))
