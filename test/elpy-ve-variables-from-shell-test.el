
(ert-deftest elpy-ve-variables-from-shell-should-return-variables ()
  (elpy-testcase ()
    (python-mode)
    (elpy-mode)
    (insert "a = 3\n")
    (insert "b = \"this is a string\"\n")
    (insert "c = ['this', 'is', 'a', 'list']\n")
    (insert "print('OK')\n")
    (elpy-shell-send-region-or-buffer)
    (with-current-buffer "*Python*"
      (elpy/wait-for-output "OK"))
    (while (not (elpy-shell--check-if-shell-available))
      (sleep-for 0.1))
    (should (equal (elpy-ve-get-variables-from-shell)
                   '((a "3
" "int") (b "\"this is a string\"" "str[16]") (c "['this', 'is', 'a', 'list']
" "list[4]"))))))

(ert-deftest elpy-ve-variables-from-shell-should-ignore-function-and-modules ()
  (elpy-testcase ()
    (python-mode)
    (elpy-mode)
    (insert "import sys\n")
    (insert "def foo(a, b):\n")
    (insert "    return a + b\n")
    (insert "a = 3\n")
    (insert "b = \"this is a string\"\n")
    (insert "c = ['this', 'is', 'a', 'list']\n")
    (insert "print('OK')\n")
    (elpy-shell-send-region-or-buffer)
    (with-current-buffer "*Python*"
      (elpy/wait-for-output "OK"))
    (while (not (elpy-shell--check-if-shell-available))
      (sleep-for 0.1))
    (should (equal (elpy-ve-get-variables-from-shell)
                   '((a "3
" "int") (b "\"this is a string\"" "str[16]") (c "['this', 'is', 'a', 'list']
" "list[4]"))))))
