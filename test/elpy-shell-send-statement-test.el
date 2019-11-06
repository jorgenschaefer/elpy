(ert-deftest elpy-shell-send-statement-should-send-statement-at-point ()
  (elpy-testcase ()
    (elpy-enable)
    (python-mode)
    (setq elpy-shell-echo-input nil)
    (setq elpy-shell-echo-output nil)
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

(ert-deftest elpy-shell-send-statement-should-send-statement-and-decorator ()
  (elpy-testcase ()
    (elpy-enable)
    (python-mode)
    (setq elpy-shell-echo-input nil)
    (setq elpy-shell-echo-output nil)
    (insert "def deco(f):
    def f2():
        print('in decorator')
        f()
    return f2

@deco
def foo():
    1+1
    for i in range(10):
        a = 2+2
        4+3
        b = a+i
    print(b)
")

    ;; on "foo"
    (elpy-shell-kill t)
    ;; send deco definition
    (goto-char 4)
    (elpy-shell-send-statement)
    (goto-char 96)
    (elpy-shell-send-statement)
    (python-shell-send-string "foo()\n")
    (python-shell-send-string "print('OK')\n")
    (should (string-match "in decorator"
                          (with-current-buffer "*Python*"
                            (elpy/wait-for-output "OK" 30)
                            (buffer-string))))
    ;; on "@deco"
    (elpy-shell-kill t)
    ;; send deco definition
    (goto-char 4)
    (elpy-shell-send-statement)
    (goto-char 86)
    (elpy-shell-send-statement)
    (python-shell-send-string "foo()\n")
    (python-shell-send-string "print('OK')\n")
    (should (string-match "in decorator"
                          (with-current-buffer "*Python*"
                            (elpy/wait-for-output "OK" 30)
                            (buffer-string))))))
