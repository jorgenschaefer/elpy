(ert-deftest elpy-pdb-debug-buffer-from-beginning-should-enter-pdb ()
  (elpy-testcase ((:project project-root "test.py")
                   (:emacs-required "25.0"))
    (find-file (f-join project-root "test.py"))
    (python-mode)
    (elpy-mode 1)
    (insert "def foo(a):\n"
            "  b = 1 + 2\n"
            "  c = a/b\n"
            "  return c\n"
            "\n"
            "foo(2)\n")
    (elpy-pdb-debug-buffer-from-beginning)
    (let ((output (with-current-buffer "*Python*"
                    (elpy/wait-for-output "foo(a):\n(Pdb)")
                    (buffer-string))))
      (should (string-match "-> def foo(a):\n(Pdb)" output)))))

(ert-deftest elpy-pdb-debug-buffer-should-always-begin-at-first-line ()
  (elpy-testcase ((:project project-root "test.py")
                   (:emacs-required "25.0"))
    (find-file (f-join project-root "test.py"))
    (python-mode)
    (elpy-mode 1)
    (insert "def foo(a):\n"
            "  b = 1 + 2\n"
            "  c = a/b\n"
            "  return c\n"
            "\n"
            "foo(2)\n")
    ;; set breakpoint at "c = a/b"
    (goto-char 29)
    (elpy-pdb-toggle-breakpoint-at-point)
    (elpy-pdb-debug-buffer-from-beginning)
    (let ((output (with-current-buffer "*Python*"
                    (elpy/wait-for-output "foo(a):\n(Pdb)")
                    (buffer-string))))
      (should (string-match "-> def foo(a):\n(Pdb)" output)))))

(ert-deftest elpy-pdb-debug-buffer-should-ignore-breakpoints ()
  (elpy-testcase ((:project project-root "test.py")
                   (:emacs-required "25.0"))
    (find-file (f-join project-root "test.py"))
    (python-mode)
    (elpy-mode 1)
    (insert "def foo(a):\n"
            "  b = 1 + 2\n"
            "  c = a/b\n"
            "  return c\n"
            "\n"
            "foo(2)\n")
    ;; set breakpoint at "c = a/b"
    (goto-char 29)
    (elpy-pdb-toggle-breakpoint-at-point)
    (elpy-pdb-debug-buffer-from-beginning)
    (python-shell-send-string "continue\n")
    (let ((output (with-current-buffer "*Python*"
                    (elpy/wait-for-output ">>>")
                    (buffer-string))))
      (should (string-match ">>>" output)))))
