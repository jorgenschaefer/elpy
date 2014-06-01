(ert-deftest elpy-multiedit-should-edit-symbol ()
  (elpy-testcase ((:teardown (elpy-multiedit-stop)))
    (insert "foo_bar\n\nfoo_bar\n")
    (goto-char (point-min))

    (call-interactively 'elpy-multiedit)
    (insert "my_")

    (should (equal (buffer-string)
                   "my_foo_bar\n\nmy_foo_bar\n"))))

(ert-deftest elpy-multiedit-should-edit-region ()
  (elpy-testcase ((:teardown (elpy-multiedit-stop)))
    (insert "foo bar\n\nfoo bar\n")
    (goto-char (point-min))
    (elpy/mark-region (line-beginning-position)
                      (line-end-position))
    (call-interactively 'elpy-multiedit)
    (forward-word 1)
    (delete-char 1)
    (should (equal (buffer-string)
                   "foobar\n\nfoobar\n"))))

(ert-deftest elpy-multiedit-should-stop-if-enabled ()
  (elpy-testcase ((:teardown (elpy-multiedit-stop)))
    (mletf* ((stop-called nil)
             (elpy-multiedit-stop () (setq stop-called t))
             (elpy-multiedit-overlays '(t t)))

      (elpy-multiedit)

      (should stop-called))))
