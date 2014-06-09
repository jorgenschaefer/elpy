;; Basically, this is a purely interactive function. Very tricky to
;; test.

(ert-deftest elpy-project--read-project-variable ()
  (elpy-testcase ()
    (mletf* ((called-prompt nil)
             (completing-read (prompt vals func require-match default history)
                              (setq called-prompt prompt)
                              "user-input"))
      (should (eq 'user-input
                  (elpy-project--read-project-variable "The Prompt")))
      (should (equal called-prompt "The Prompt")))))
