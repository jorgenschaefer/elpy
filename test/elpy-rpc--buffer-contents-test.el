(ert-deftest elpy-rpc--buffer-contents-should-return-string-if-small ()
  (elpy-testcase ()
    (insert "small buffer")

    (let ((elpy-rpc-large-buffer-size 1024))
      (should (equal (elpy-rpc--buffer-contents)
                     "small buffer")))))

(ert-deftest elpy-rpc--buffer-contents-should-return-file-if-large ()
  (elpy-testcase ()
    (insert "large buffer")

    (let* ((elpy-rpc-large-buffer-size 5)
           (fileobj (elpy-rpc--buffer-contents))
           (file-name (cdr (assq 'filename fileobj)))
           (delete-after-use (cdr (assq 'delete_after_use fileobj))))

      (unwind-protect
          (progn
            (should delete-after-use)
            (should (equal (with-temp-buffer
                             (insert-file-contents-literally file-name)
                             (buffer-string))
                           "large buffer")))
        (delete-file file-name)))))
