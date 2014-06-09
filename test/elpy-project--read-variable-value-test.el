;; Basically, this is a purely interactive function. Very tricky to
;; test. Worse yet, it pretty much only uses widget functionality.
;; Meh.

(ert-deftest elpy-project--read-variable-value ()
  (elpy-testcase ()
    (should (functionp 'elpy-project--read-variable-value))))
