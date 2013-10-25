;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(Given "^I have the following Python file:$"
  (lambda (arg)
    (elpy-enable)
    (auto-save-mode 0)
    (let ((tmpdir (make-temp-file "elpy-test-" t)))
      (find-file (format "%s/test.py" tmpdir))
      (add-cleanup 'delete-directory tmpdir t)
      (add-cleanup 'kill-buffer-no-questions (current-buffer)))
    (insert arg)
    (goto-char (point-min))))

(Given "^I have an empty Python file$"
  (lambda ()
    (elpy-enable)
    (auto-save-mode 0)
    (let ((tmpdir (make-temp-file "elpy-test-" t)))
      (find-file (format "%s/test.py" tmpdir))
      (add-cleanup 'delete-directory tmpdir t)
      (add-cleanup 'kill-buffer-no-questions (current-buffer)))))

(Then "^the cursor should be at end of buffer$"
  (lambda ()
    (let ((actual (buffer-substring-no-properties (point)
                                                  (min (point-max)
                                                       (+ (point) 5)))))
      (assert (eobp)
              nil
              "Expected to be at the end of the buffer but was before '%s'."
              actual))))

(Then "^Elpy should be enabled$"
  (lambda ()
    (assert (memq 'elpy-mode python-mode-hook)
            nil
            "Expected `elpy-mode' in `python-mode-hook'")))

(Then "^the project root should be \"\\([^\"]*\\)\"$"
  (lambda (arg)
    (assert (equal arg elpy-project-root)
            nil
            "Expected the project root to be '%s', but was '%s'"
            arg elpy-project-root)))
