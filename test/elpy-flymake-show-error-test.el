(ert-deftest elpy-flymake-show-error ()
  ;; not used for emacs >= 26.1
  (when (version< emacs-version "26.1")
      (elpy-testcase ()
        (mletf* ((flymake-find-err-info
                  (info line)
                  (list (list (flymake-ler-make-ler "" "" "" "test-error-1")
                              (flymake-ler-make-ler "" "" "" "test-error-2"))))
                 (output nil)
                 (message (fmt &rest args) (setq output (apply 'format fmt args))))

          (elpy-flymake-show-error)

          (should (equal output "test-error-1, test-error-2"))))))
