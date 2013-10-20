(ert-deftest test-elpy-shell-get-or-create-process ()
  "Test that we can run `elpy-shell-get-or-create-process' at all.

Backwards compatibility issues, see #124."
  (let ((proc (elpy-shell-get-or-create-process)))
    (and (processp proc)
         (process-live-p proc))))

(ert-deftest test-elpy--region-without-indentation ()
  "Test the function."
  (with-temp-buffer
    (insert "def foo():\n"
            "  pass")
    (should (equal (elpy--region-without-indentation (point-min)
                                                     (point-max))
                   "def foo():\n  pass")))
  (with-temp-buffer
    (insert "  \n"
            "  def foo():\n"
            "    pass\n"
            "  ")
    (should (equal (elpy--region-without-indentation (point-min)
                                                     (point-max))
                   "\ndef foo():\n  pass\n")))

  (with-temp-buffer
    (insert "  def foo():\n"
            "    return 23\n"
            "\n"
            "  print foo()")
    (should (equal (elpy--region-without-indentation (point-min)
                                                     (point-max))
                   "def foo():\n  return 23\n\nprint foo()")))

  (with-temp-buffer
    (insert "  def foo():\n"
            "    pass\n"
            "\n"
            "meh\n")
    (should-error (elpy--region-without-indentation (point-min)
                                                    (point-max)))))
