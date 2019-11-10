(ert-deftest elpy-shell-get-or-create-process-should-return-process ()
  (elpy-testcase ()
    ;; This was left set by some other test, apparently, which breaks
    ;; in 24.1.
    (setq python-buffer nil)

    (let ((proc (elpy-shell-get-or-create-process)))
      (with-current-buffer (process-buffer proc)
        (should (eq major-mode 'inferior-python-mode))))))

(ert-deftest elpy-shell-get-or-create-process-should-add-project-root-to-path ()
    (elpy-testcase ((:project project-root "test.py"))
        (elpy-enable)
        (find-file (f-join project-root "test.py"))
        (python-mode)
        (mletf* ((elpy-project-root () project-root))
          (elpy-shell-switch-to-shell)
          ;; wait for shell to start
          (python-shell-send-string "print('OK1')")
          (elpy-shell-switch-to-shell)
          (elpy/wait-for-output "OK1")
          ;; check if path has been appended
          (elpy-shell-switch-to-buffer)
          (python-shell-send-string "import sys;print(sys.path)")
          (python-shell-send-string "print('OK2')")
          (elpy-shell-switch-to-shell)
          (elpy/wait-for-output "OK2")
          (should (string-match (format "'%s'" project-root)
                                (buffer-string))))))
