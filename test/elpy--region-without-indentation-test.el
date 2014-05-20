(ert-deftest elpy--region-without-indentation-should-strip-indentation ()
  (elpy-testcase ()
    (with-temp-buffer
      (should (equal (elpy--region-without-indentation (point-min)
                                                       (point-max))
                     "")))

    (with-temp-buffer
      (insert "print 'Foo'")
      (should (equal (elpy--region-without-indentation (point-min)
                                                       (point-max))
                     "print 'Foo'")))

    (with-temp-buffer
      (insert "print 'Foo'\n"
              "print 'Bar'\n")
      (should (equal (elpy--region-without-indentation (point-min)
                                                       (point-max))
                     "print 'Foo'\nprint 'Bar'\n")))
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
                                                      (point-max))))))
