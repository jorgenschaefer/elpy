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
      (add-cleanup 'kill-buffer (current-buffer)))
    (insert arg)
    (goto-char (point-min))))

(Then "^I should see the message \"\\([^\"]*\\)\"$"
  (lambda (arg)
    (When "I switch to buffer \"*Messages*\"")
    (Then (format "I should see \"%s\"" arg))))

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
            "Expected `elpy-mode' in `python-mode-hook'")
    ))
