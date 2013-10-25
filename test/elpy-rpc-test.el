(ert-deftest elpy-rpc--process-buffer-p ()
  "Test that this finds actual process buffers."
  (with-temp-buffer
    (setq elpy-rpc--buffer-p t)
    (should (elpy-rpc--process-buffer-p (current-buffer))))
  (with-temp-buffer
    (should (not (elpy-rpc--process-buffer-p (current-buffer))))))

(ert-deftest elpy-rpc--live-p ()
  "Test that it finds live buffers."
  (should (not (elpy-rpc--live-p nil)))
  (with-temp-buffer
    (should (not (elpy-rpc--live-p (current-buffer)))))
  (with-temp-buffer
    (let ((proc (start-process "*elpy-rpc-test*" (current-buffer) "cat")))
      (set-process-query-on-exit-flag proc nil)
      (should (elpy-rpc--live-p (current-buffer)))
      (delete-process proc)
      (should (not (elpy-rpc--live-p (current-buffer)))))))

(ert-deftest elpy-rpc--get-rpc-buffer ()
  "Test that buffers are found."
  ;; Error outside of elpy-mode buffers
  (with-temp-buffer
    (setq elpy-mode nil)
    (should-error (elpy-rpc--get-rpc-buffer)))
  ;; If we already have a buffer, return that
  (let (rpc-buf prg-buf)
    (with-temp-buffer
      (setq rpc-buf (current-buffer)
            elpy-rpc--buffer-p t)
      ;; A stupid test process
      (set-process-query-on-exit-flag
       (start-process "elpy-rpc-test" rpc-buf "cat")
       nil)
      (with-temp-buffer
        (setq prg-buf (current-buffer)
              elpy-mode t
              elpy-rpc--buffer rpc-buf)
        (should (equal rpc-buf (elpy-rpc--get-rpc-buffer))))))
  ;; Find existing buffer for this project
  (let (rpc-buf prg-buf)
    (with-temp-buffer
      (setq rpc-buf (current-buffer)
            elpy-rpc--buffer-p t)
      (with-temp-buffer
        (setq prg-buf (current-buffer))
        ))))

(ert-deftest elpy-rpc-echo ()
 "Test that the backend communication works at all."
 (let ((elpy-rpc--timeout 10))
   (with-temp-buffer
     (setq elpy-mode t)
     (dolist (args '(("foo" nil 1 2 3)))
       (should (equal args
                      (apply #'elpy-rpc "echo" (list args))))))))
