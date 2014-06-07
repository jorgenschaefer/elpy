(ert-deftest elpy--module-path ()
  (elpy-testcase ()
    (mletf* ((buffer-file-name "/project/test/test")
             (elpy-library-root () "/project/"))
      (should (equal "/project/file"
                     (elpy--module-path "file")))
      (should (equal "/project/package/file"
                     (elpy--module-path "package.file")))
      (should (equal "/project/test/file"
                     (elpy--module-path ".file")))
      (should (equal "/project/test/package/file"
                     (elpy--module-path ".package.file")))
      (should (equal "/project/file"
                     (elpy--module-path "..file")))
      (should (equal "/project/package/file"
                     (elpy--module-path "..package.file")))
      )))
