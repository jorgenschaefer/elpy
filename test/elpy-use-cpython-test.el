;;; This function is not complicated, it just sets a whole bunch of
;;; variables. As these are the default values, we just see if the
;;; function works.

;; From `describe-variable'
(defun original-value (symbol)
  (let ((sv (get symbol 'standard-value)))
    (and (consp sv)
         (ignore-errors
           (eval (car sv))))))

(ert-deftest elpy-use-cpython ()
  (elpy-testcase ()
    (elpy-use-cpython)
    ;; This will error if our hardcoded values at some point mismatch
    ;; the actual defaults. We might want to use this code in elpy.el
    ;; directly. Maybe.
    (dolist (sym '(python-shell-interpreter
                   python-shell-interpreter-args
                   python-shell-prompt-regexp
                   python-shell-prompt-output-regexp
                   python-shell-completion-setup-code
                   python-shell-completion-module-string-code
                   python-shell-completion-string-code))
      (let ((orig (original-value sym)))
        (when orig
          (message "%S" sym)
          (should (equal (symbol-value sym)
                         orig)))))))
