(ert-deftest elpy-shell-should-not-echo-inputs-when-deactivated ()
  (when (<= 25 emacs-major-version)
  (elpy-testcase ()
    (python-mode)
    (elpy-mode)
    (setq elpy-shell-echo-input nil)
    (setq elpy-shell-echo-output nil)
    (insert "def foo():
    1+1
    for i in range(10):
        a = 2+2
        4+3
        b = a+i
")

    ;; on "for i in range(10):"
    (goto-char 30)
    (elpy-shell-kill t)
    (elpy-shell-send-statement)
    (python-shell-send-string "print('OK')\n")
    (should (not (string-match ">>> for i in range(10):"
                               (with-current-buffer "*Python*"
                                 (elpy/wait-for-output "OK" 30)
                                 (buffer-string)))))
    )))

(ert-deftest elpy-shell-should-echo-inputs ()
  (when (<= 25 emacs-major-version)
  (elpy-testcase ()
    (python-mode)
    (elpy-mode)
    (setq elpy-shell-echo-input t)
    (setq elpy-shell-echo-output nil)
    (insert "def foo():
    1+1
    for i in range(10):
        a = 2+2
        4+3
        b = a+i
")
    ;; on "for i in range(10):"
    (goto-char 30)
    (elpy-shell-kill t)
    (elpy-shell-send-statement)
    (python-shell-send-string "print('OK')\n")
    (should (string-match ">>> for i in range(10):"
                          (with-current-buffer "*Python*"
                            (elpy/wait-for-output "OK" 30)
                            (buffer-string))))
    (should (string-match "...:     a = 2\\+2"
                          (with-current-buffer "*Python*"
                            (buffer-string))))
    ;; on "a = 2+2"
    (goto-char 52)
    (elpy-shell-kill t)
    (elpy-shell-send-statement)
    (python-shell-send-string "print('OK')\n")
    (should (string-match ">>> a = 2\\+2"
                          (with-current-buffer "*Python*"
                            (elpy/wait-for-output "OK" 30)
                            (buffer-string))))
    ;; on "a = 2+2" and "4+3" lines
    (set-mark 52)
    (goto-char 72)
    (activate-mark)
    (elpy-shell-kill t)
    (elpy-shell-send-region-or-buffer)
    (python-shell-send-string "print('OK')\n")
    (should (string-match ">>> a = 2\\+2"
                          (with-current-buffer "*Python*"
                            (elpy/wait-for-output "OK" 30)
                            (buffer-string))))
    (should (string-match "...: 4\\+3"
                          (with-current-buffer "*Python*"
                            (buffer-string))))
    )))

(ert-deftest elpy-shell-should-echo-outputs ()
  (when (<= 25 emacs-major-version)
  (elpy-testcase ()
    (python-mode)
    (elpy-mode)
    (setq python-shell-interpreter "python2")
    (setq elpy-shell-echo-input nil)
    (setq elpy-shell-echo-output t)
    (insert "def foo():
    1+1
    for i in range(10):
        a = 2+2
        4+3
        b = a+i
")

    ;; on "1+1"
    (goto-char 18)
    (elpy-shell-kill t)
    (elpy-shell-send-statement)
    (python-shell-send-string "print('OK')\n")
    (should (string-match "^2" (with-current-buffer "*Python*"
                                 (elpy/wait-for-output "OK" 30)
                                 (buffer-string))))
    ;; on "4+3"
    (goto-char 70)
    (elpy-shell-kill t)
    (elpy-shell-send-statement)
    (python-shell-send-string "print('OK')\n")
    (should (string-match "^7" (with-current-buffer "*Python*"
                                 (elpy/wait-for-output "OK" 30)
                                 (buffer-string))))
    ;; on "a = 2+2" and "4+3" lines
    (set-mark 52)
    (goto-char 72)
    (activate-mark)
    (elpy-shell-kill t)
    (elpy-shell-send-region-or-buffer)
    (python-shell-send-string "print('OK')\n")
    (should (string-match "^7" (with-current-buffer "*Python*"
                                 (elpy/wait-for-output "OK" 30)
                                 (buffer-string))))
    )))
