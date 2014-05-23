(ert-deftest elpy-module-highlight-indentation-global-init ()
  (elpy-testcase ()
    (mletf* ((required-feature nil)
             (require (feature) (setq required-feature feature)))

      (elpy-module-highlight-indentation 'global-init)

      (should (eq required-feature 'highlight-indentation)))))

(ert-deftest elpy-module-highlight-indentation-buffer-init ()
  (elpy-testcase ()
    (elpy-module-highlight-indentation 'buffer-init)

    (should (if (boundp 'highlight-indentation-mode)
                highlight-indentation-mode
              highlight-indent-active))))

(ert-deftest elpy-module-highlight-indentation-buffer-stop ()
  (elpy-testcase ()
    (highlight-indentation-mode 1)

    (elpy-module-highlight-indentation 'buffer-stop)

    (should-not (if (boundp 'highlight-indentation-mode)
                    highlight-indentation-mode
                  highlight-indent-active))))
