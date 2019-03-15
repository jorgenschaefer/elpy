(ert-deftest elpy-pdb-debug-last-exception-should-debug-last-exception ()
  (elpy-testcase ((:project project-root "test.py"))
    (find-file (f-join project-root "test.py"))
    (python-mode)
    (elpy-mode 1)
    (insert "def foo(a):\n"
            "  b = 1 + 2\n"
            "  c = a/B\n"
            "  return c\n"
            "\n"
            "foo(2)\n")
    (save-buffer)
    (elpy-shell-send-buffer)
    (with-current-buffer "*Python*"
      (elpy/wait-for-output "NameError"))
    (elpy-pdb-debug-last-exception)
    (let ((output (with-current-buffer "*Python*"
                    (elpy/wait-for-output "c = a/B\n(Pdb)")
                    (buffer-string))))
      (should (string-match "test.py(3)foo()\n-> c = a/B\n(Pdb)" output)))))

(ert-deftest elpy-pdb-debug-last-exception-should-ignore-breakpoints ()
  (elpy-testcase ((:project project-root "test.py"))
    (find-file (f-join project-root "test.py"))
    (python-mode)
    (elpy-mode 1)
    (insert "def foo(a):\n"
            "  b = 1 + 2\n"
            "  c = a/B\n"
            "  return c\n"
            "\n"
            "foo(2)\n")
    ;; set breakpoints at "b = 1 + 2"
    (goto-char 19)
    (elpy-pdb-toggle-breakpoint-at-point)
    (save-buffer)
    ;; send buggy code
    (elpy-shell-send-buffer)
    (with-current-buffer "*Python*"
      (elpy/wait-for-output "NameError"))
    (elpy-pdb-debug-last-exception)
    (let ((output (with-current-buffer "*Python*"
                    (elpy/wait-for-output "c = a/B\n(Pdb)")
                    (buffer-string))))
      (should (string-match "test.py(3)foo()\n-> c = a/B\n(Pdb)" output)))))
