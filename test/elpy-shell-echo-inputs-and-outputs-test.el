(ert-deftest elpy-shell-should-not-echo-inputs-when-deactivated ()
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
    ))

(ert-deftest elpy-shell-should-echo-inputs ()
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
    (when (version< "3.0.0" (elpy-shell-get-python-version))
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
  (elpy-testcase ()
    (python-mode)
    (elpy-mode)
    (setq elpy-shell-echo-input nil)
    (setq elpy-shell-echo-output t)
    (insert "def foo():\n    1+1\n    for i in range(10):\n        a = 2+2\n        4+3\n        b = a+i\n")

    ;; on "1+1"
    (goto-char 18)
    (elpy-shell-kill t)
    (elpy-shell-send-statement)
    (python-shell-send-string "print('OK')\n")
    (if (<= emacs-major-version 24)
        (should (string-match ">>> 2" (with-current-buffer "*Python*"
                                         (elpy/wait-for-output "OK" 30)
                                         (buffer-string))))
      (should (string-match "^2" (with-current-buffer "*Python*"
                                   (elpy/wait-for-output "OK" 30)
                                   (buffer-string)))))
    ;; on "4+3"
    (goto-char 69)
    (elpy-shell-kill t)
    (elpy-shell-send-statement)
    (python-shell-send-string "print('OK')\n")
    (if (<= emacs-major-version 24)
        (should (string-match ">>> 7" (with-current-buffer "*Python*"
                                         (elpy/wait-for-output "OK" 30)
                                         (buffer-string))))
      (should (string-match "^7" (with-current-buffer "*Python*"
                                   (elpy/wait-for-output "OK" 30)
                                   (buffer-string)))))
    ;; on "a = 2+2" and "4+3" lines
    (set-mark 52)
    (goto-char 72)
    (activate-mark)
    (elpy-shell-kill t)
    (message "use-region: %s" (use-region-p))
    (elpy-shell-send-region-or-buffer)
    (python-shell-send-string "print('OK')\n")
    (if (<= emacs-major-version 24)
        (should (string-match ">>> 7" (with-current-buffer "*Python*"
                                         (elpy/wait-for-output "OK" 30)
                                         (buffer-string))))
      (should (string-match "^7" (with-current-buffer "*Python*"
                                   (elpy/wait-for-output "OK" 30)
                                   (buffer-string)))))
    ))
