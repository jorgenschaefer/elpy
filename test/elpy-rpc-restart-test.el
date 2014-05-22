(ert-deftest elpy-rpc-restart-should-kill-process-and-buffer ()
  (elpy-testcase ()
    (mletf* ((buf (current-buffer))
             (elpy-rpc--process-buffer-p (buffer) (equal buffer buf))
             (get-buffer-process (buffer) (list 'process buffer))
             (killed-process nil)
             (kill-process (proc) (setq killed-process proc))
             (killed-buffer nil)
             (kill-buffer (buf) (setq killed-buffer buf)))

      (elpy-rpc-restart)

      (should (equal killed-process (list 'process buf)))
      (should (equal killed-buffer buf)))))
