(ert-deftest elpy-pdb-break-at-point-should-break-at-point ()
  (elpy-testcase ((:project project-root "test.py")
                   (:emacs-required "25.0"))
    (find-file (f-join project-root "test.py"))
    (elpy-enable)
    (python-mode)
    (insert "def foo(a):\n"
            "  b = 1 + 2\n"
            "  c = a/b\n"
            "  return c\n"
            "\n"
            "foo(2)\n")
    ;; goto "c = a/b"
    (goto-char 29)
    (elpy-pdb-break-at-point)
    (let ((output (with-current-buffer "*Python*"
                    (elpy/wait-for-output "c = a/b\n(Pdb)")
                    (buffer-string))))
      (should (string-match "test.py(3)foo()\n-> c = a/b\n(Pdb)" output)))))

(ert-deftest elpy-pdb-debug-buffer-and-break-at-point-should-ignore-breakpoints ()
  (elpy-testcase ((:project project-root "test.py")
                   (:emacs-required "25.0"))
    (find-file (f-join project-root "test.py"))
    (elpy-enable)
    (python-mode)
    (insert "def foo(a):\n"
            "  b = 1 + 2\n"
            "  c = a/b\n"
            "  return c\n"
            "\n"
            "foo(2)\n")
    ;; set breakpoint at "b = 1 + 2"
    (goto-char 19)
    (elpy-pdb-toggle-breakpoint-at-point)
    ;; goto "c = a/b"
    (goto-char 29)
    (elpy-pdb-break-at-point)
    (let ((output (with-current-buffer "*Python*"
                    (elpy/wait-for-output "c = a/b\n(Pdb)")
                    (buffer-string))))
      (should (string-match "test.py(3)foo()\n-> c = a/b\n(Pdb)" output)))
    (python-shell-send-string "continue")
    (let ((output (with-current-buffer "*Python*"
                    (elpy/wait-for-output ">>>")
                    (buffer-string))))
      (should (string-match ">>>" output)))))
