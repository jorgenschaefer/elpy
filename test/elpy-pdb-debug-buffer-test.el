(ert-deftest elpy-pdb-debug-buffer-should-enter-pdb ()
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
    (elpy-pdb-debug-buffer)
    (let ((output (with-current-buffer "*Python*"
                    (elpy/wait-for-output "(Pdb)")
                    (buffer-string))))
      (should (string-match "-> def foo(a):\n(Pdb)" output)))))

(ert-deftest elpy-pdb-debug-buffer-should-stop-at-the-first-breakpoint ()
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
    (elpy-pdb-debug-buffer)
    (let ((output (with-current-buffer "*Python*"
                    (elpy/wait-for-output "a/b\n(Pdb)")
                    (buffer-string))))
      (should (string-match "test.py(3)foo()\n-> c = a/b\n(Pdb)" output)))))

(ert-deftest elpy-pdb-debug-buffer-should-continue-with-second-breakpoint ()
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
    ;; set breakpoint at "b = 1 + 2"
    (goto-char 19)
    (elpy-pdb-toggle-breakpoint-at-point)
    ;; set breakpoint at "c = a/b"
    (goto-char 29)
    (elpy-pdb-toggle-breakpoint-at-point)
    (elpy-pdb-debug-buffer)
    (let ((output (with-current-buffer "*Python*"
                    (elpy/wait-for-output "b = 1 \\+ 2\n(Pdb)")
                    (buffer-string))))
      (should (string-match "test.py(2)foo()\n-> b = 1 \\+ 2\n(Pdb)" output)))
    (python-shell-send-string "continue\n")
    (let ((output (with-current-buffer "*Python*"
                    (elpy/wait-for-output "a/b\n(Pdb)")
                    (buffer-string))))
      (should (string-match "test.py(3)foo()\n-> c = a/b\n(Pdb)" output)))))

(ert-deftest elpy-pdb-debug-buffer-should-forget-previous-breakpoints ()
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
    ;; set breakpoint at "b = 1 + 2"
    (goto-char 19)
    (elpy-pdb-toggle-breakpoint-at-point)
    ;; set breakpoint at "c = a/b"
    (goto-char 29)
    (elpy-pdb-toggle-breakpoint-at-point)
    ;; run pdm a first time
    (elpy-pdb-debug-buffer)
    (python-shell-send-string "continue\n")
    (python-shell-send-string "continue\n")
    ;; remove the first breakpoints
    (goto-char 19)
    (elpy-pdb-toggle-breakpoint-at-point)
    ;; run pdb a second time,
    (elpy-pdb-debug-buffer)
    ;; pdb should not stop at the first breakpoint
    (let ((output (with-current-buffer "*Python*"
                    (elpy/wait-for-output "a/b\n(Pdb)")
                    (buffer-string))))
      (should (string-match "test.py(3)foo()\n-> c = a/b\n(Pdb)" output)))))
