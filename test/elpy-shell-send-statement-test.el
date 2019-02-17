(ert-deftest elpy-shell-send-statement-should-send-statement-at-point ()
  (elpy-testcase ()
    (python-mode)
    (elpy-mode)
    (setq elpy-shell-echo-input nil)
    (setq elpy-shell-echo-output nil)
    (setq elpy-shell-capture-last-multiline-output nil)
    (insert "def foo():
    1+1
    for i in range(10):
        a = 2+2
        4+3
        b = a+i
    print(b)
")

    ;; on "a = 2+2"
    (goto-char 52)
    (elpy-shell-kill t)
    (elpy-shell-send-statement)
    (python-shell-send-string "print(a)\n")
    (python-shell-send-string "print('OK')\n")
    (should (string-match ">>> 4"
                          (with-current-buffer "*Python*"
                            (elpy/wait-for-output "OK" 30)
                            (buffer-string))))
    ;; on "for i in range(10):"
    (goto-char 30)
    (elpy-shell-kill t)
    (elpy-shell-send-statement)
    (python-shell-send-string "print(b)\n")
    (python-shell-send-string "print('OK')\n")
    (should (string-match ">>> 13"
                          (with-current-buffer "*Python*"
                            (elpy/wait-for-output "OK" 30)
                            (buffer-string))))
    (python-shell-send-string "del b")
    ;; on "def foo():"
    (goto-char 6)
    (elpy-shell-kill t)
    (elpy-shell-send-statement)
    (python-shell-send-string "foo()\n")
    (python-shell-send-string "print('OK')\n")
    (should (string-match ">>> 13"
                          (with-current-buffer "*Python*"
                            (elpy/wait-for-output "OK" 30)
                            (buffer-string))))
    ))
