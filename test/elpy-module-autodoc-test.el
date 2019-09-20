(defun elpy-doc-test-insert-functions ()
  (insert
   "def fun1():
    \"\"\"
    fun1 documentation ##9ghtye
    \"\"\"
    pass
def fun2():
    \"\"\"
    fun2 documentation #yhe38R
    \"\"\"
    pass\n"
   ))

(ert-deftest elpy-doc-should-be-updated-automatically ()
  (elpy-testcase ()
    (let ((elpy-autodoc-delay 0.01))
      (add-to-list 'elpy-modules 'elpy-module-autodoc)
      (python-mode)
      (elpy-mode)
      (elpy-doc-test-insert-functions)
      (insert "fun1")
      (elpy-doc)
      (with-current-buffer "*Python Doc*"
        (should (re-search-forward "fun1 documentation ##9ghtye")))
      (insert "\nfun2")
      (run-hooks 'post-command-hook)
      (sleep-for 0.1)
      (with-current-buffer "*Python Doc*"
        (should (re-search-forward "fun2 documentation #yhe38R"))))))

(ert-deftest elpy-doc-should-not-be-updated-if-doc-not-visible ()
  (elpy-testcase ()
    (let ((elpy-autodoc-delay 0.01))
      (add-to-list 'elpy-modules 'elpy-module-autodoc)
      (python-mode)
      (elpy-doc-test-insert-functions)
      (insert "fun1")
      (elpy-doc)
      (with-current-buffer "*Python Doc*"
        (should (re-search-forward "fun1 documentation ##9ghtye")))
      (delete-window (get-buffer-window "*Python Doc*"))
      (insert "\nfun2")
      (run-hooks 'post-command-hook)
      (sleep-for 0.1)
      (with-current-buffer "*Python Doc*"
        (should (re-search-forward "fun1 documentation ##9ghtye"))))))

(ert-deftest elpy-doc-should-not-be-updated-if-deactivated ()
  (elpy-testcase ()
    (let ((elpy-autodoc-delay 0.01))
      (add-to-list 'elpy-modules 'elpy-module-autodoc)
      (python-mode)
      (elpy-mode)
      (setq elpy-autodoc-delay nil)
      (elpy-doc-test-insert-functions)
      (insert "fun1")
      (elpy-doc)
      (with-current-buffer "*Python Doc*"
        (should (re-search-forward "fun1 documentation ##9ghtye")))
      (insert "\nfun2")
      (run-hooks 'post-command-hook)
      (sleep-for 0.1)
      (with-current-buffer "*Python Doc*"
        (goto-char (point-min))
        (should (re-search-forward "fun1 documentation ##9ghtye"))))))
