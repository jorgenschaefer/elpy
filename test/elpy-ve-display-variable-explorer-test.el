(ert-deftest elpy-ve-display-variable-explorer-should-display ()
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
    (mletf* ((window-width () 100))
      (elpy-ve-display-variable-explorer))
    (should (string= (substring-no-properties (buffer-string))
                     "----------------------------------------------------------------------------------------------------
 a                        int                      3
----------------------------------------------------------------------------------------------------
 b                        str[16]                  \"this is a string\"
----------------------------------------------------------------------------------------------------
 c                        list[4]                  ['this', 'is', 'a', 'list']
----------------------------------------------------------------------------------------------------
"))))

(ert-deftest elpy-ve-display-variable-explorer-should-display-detailed-var ()
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
    (mletf* ((window-width () 40))
      (elpy-ve-display-variable-explorer)
      (elpy-ve-display-variable-at-point))
    (other-window 1)
    (should (search-forward "a  int"))
    (should (search-forward "3"))))

(ert-deftest elpy-ve-goto-next-entry-should-work ()
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
    (mletf* ((window-width () 40))
      (elpy-ve-display-variable-explorer))
    (elpy-ve-goto-next-entry)
    (should (string-match "str\\[16\\] *\"this is"
                          (buffer-substring-no-properties
                           (line-beginning-position)
                           (line-end-position))))
    (elpy-ve-goto-next-entry)
    (should (string-match "list\\[4\\] *\\['this',"
                          (buffer-substring-no-properties
                           (line-beginning-position)
                           (line-end-position))))
    (elpy-ve-goto-prev-entry)
    (should (string-match "str\\[16\\] *\"this is"
                          (buffer-substring-no-properties
                           (line-beginning-position)
                           (line-end-position))))
    ))
