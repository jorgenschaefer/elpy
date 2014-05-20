(ert-deftest elpy-remove-modeline-lighter-should-set-lighter-empty ()
  (elpy-testcase ()
    (let ((minor-mode-alist '((test-mode "NoisyTestMode"))))
      (elpy-remove-modeline-lighter 'test-mode)
      (should (equal minor-mode-alist
                     '((test-mode "")))))))

(ert-deftest elpy-remove-modeline-lighter-should-handle-eldoc-minor-mode ()
  (elpy-testcase ()
    (let ((minor-mode-alist '((eldoc-minor-mode "NoisyTestMode")))
          (eldoc-minor-mode-string "frob"))
      (elpy-remove-modeline-lighter 'eldoc-minor-mode)
      (should (equal eldoc-minor-mode-string nil)))))
