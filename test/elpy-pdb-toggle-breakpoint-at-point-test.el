(ert-deftest elpy-pdb-toggle-breakpoint-at-point-should-add-breakpoints ()
  (elpy-testcase ((:project project-root "test.py")
                   (:emacs-required "25.0"))
    (find-file (f-join project-root "test.py"))
    (elpy-enable)
    (python-mode)
    (insert "def foo(a):\n"
            "  b = 1 + 2\n"
            "  c = a/B\n"
            "  return c\n"
            "\n"
            "foo(2)\n")
    ;; set breakpoint at "b = 1 + 2"
    (goto-char 19)
    (elpy-pdb-toggle-breakpoint-at-point)
    ;; set breakpoint at "c = a/b"
    (goto-char 29)
    (elpy-pdb-toggle-breakpoint-at-point)
    ;;
    (should (equal (elpy-pdb--get-breakpoint-positions)
                   '(2 3)))))

(ert-deftest elpy-pdb-toggle-breakpoint-at-point-should-remove-breakpoints ()
  (elpy-testcase ((:project project-root "test.py")
                   (:emacs-required "25.0"))
    (find-file (f-join project-root "test.py"))
    (elpy-enable)
    (python-mode)
    (insert "def foo(a):\n"
            "  b = 1 + 2\n"
            "  c = a/B\n"
            "  return c\n"
            "\n"
            "foo(2)\n")
    ;; set breakpoint at "b = 1 + 2"
    (goto-char 19)
    (elpy-pdb-toggle-breakpoint-at-point)
    ;; set breakpoint at "c = a/b"
    (goto-char 29)
    (elpy-pdb-toggle-breakpoint-at-point)
    ;; remove breakpoint at "b = 1 + 2"
    (goto-char 19)
    (elpy-pdb-toggle-breakpoint-at-point)
    ;;
    (should (equal (elpy-pdb--get-breakpoint-positions)
                   '(3)))))
