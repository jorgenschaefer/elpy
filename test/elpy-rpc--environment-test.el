(ert-deftest elpy-rpc--environment-should-add-directory ()
  (elpy-testcase ()
    (mletf* ((process-environment '("PYTHONPATH=/foo:/bar"))
             (elpy-rpc-pythonpath "/baz/dir")
             (file-exists-p (path) t))

      (should (equal (car (elpy-rpc--environment))
                     "PYTHONPATH=/baz/dir:/foo:/bar")))))

(ert-deftest elpy-rpc--environment-should-ignore-nil ()
  (elpy-testcase ()
    (let ((process-environment '("PYTHONPATH=/foo:/bar"))
          (elpy-rpc-pythonpath nil))

      (should (equal (elpy-rpc--environment)
                     '("PYTHONPATH=/foo:/bar"))))))
