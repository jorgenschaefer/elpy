(ert-deftest elpy-shell-send-current-statement-should-send-current-statement-only ()
  (elpy-testcase ()
                 (python-mode)
                 (elpy-mode)
                 (insert "print('''"
                         "First statement"
                         "''')\n"
                         "print('Second statement')\n")
                 (goto-char (point-min))
                 (forward-line)
                 (elpy-shell-send-current-statement)
                 (let ((output (with-current-buffer "*Python*"
                                 (elpy/wait-for-output "Only")
                                 (buffer-string))))
                   (should (string-match "First statement" output))
                   (should (not (string-match "Second statement" output))))))

(ert-deftest elpy-shell-send-current-statement-should-navigate-to-next-statement ()
  (elpy-testcase ()
                 (set-buffer-string-with-point
                  "print('''"
                  "_|_First statement"
                  "''')\n"
                  "print('Second statement')\n")
                 (elpy-shell-send-current-statement)
                 (should (= (point) 33))))

