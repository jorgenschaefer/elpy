(ert-deftest elpy-multiedit--usages-should-mark-for-edit ()
  (elpy-testcase ((:project project-root
                            ("test1.py" "foo\nbar\nbaz")))

    (elpy-multiedit--usages
     `(((filename . ,(f-join project-root "test1.py"))
        (name . "bar")
        (offset . 4))))

    (find-file (f-join project-root "test1.py"))
    (should (overlay-get (car (overlays-at 5))
                         'elpy-multiedit))))

(ert-deftest elpy-multiedit--usages-should-pop-up-buffer-for-multi-buffer-edits ()
  (elpy-testcase ((:project project-root
                            ("test1.py" "foo\nbar\nbaz")
                            ("test2.py" "bar\nbaz\nfoo")))
    (elpy-multiedit--usages
     `(((filename . ,(f-join project-root "test1.py"))
        (this-name . "bar")
        (offset . 5))
       ((filename . ,(f-join project-root "test2.py"))
        (this-name . "bar")
        (offset . 1))))

    (with-current-buffer "*Elpy Edit Usages*"
      (goto-char (point-min))
      (should (re-search-forward "test1.py" nil t))
      (should (re-search-forward "test2.py" nil t)))))
