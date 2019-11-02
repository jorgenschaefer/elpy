(ert-deftest elpy-shell-send-statement-should-add-to-shell-history ()
  (elpy-testcase ()
    (python-mode)
    (elpy-mode)
    (let ((elpy-shell-add-to-shell-history t))
      (insert "a = 2 + 4")
      (elpy-shell-send-statement)
      (python-shell-send-string "print('OK')\n")
      (with-current-buffer "*Python*"
        (elpy/wait-for-output "OK")
        (let ((last-hist
               (ring-ref comint-input-ring
                         (comint-previous-matching-input-string-position
                          "." 1))))
          (should (string= "a = 2 + 4" last-hist)))))))

(ert-deftest elpy-shell-send-statement-should-NOT-add-to-shell-history ()
  (elpy-testcase ()
    (python-mode)
    (elpy-mode)
    (let ((elpy-shell-add-to-shell-history nil))
      (insert "a = 2 + 4")
      (elpy-shell-send-statement)
      (python-shell-send-string "print('OK')\n")
      (with-current-buffer "*Python*"
        (elpy/wait-for-output "OK")
        (should-error (comint-previous-matching-input-string-position "."
                                                                      1))))))

(ert-deftest elpy-shell-send-statement-should-add-multilines-statements-to-shell-history ()
  (elpy-testcase ()
    (python-mode)
    (elpy-mode)
    (let ((elpy-shell-add-to-shell-history t))
      (insert "def foo(a):\n"
              "  d = a + 2\n"
              "  return d\n")
      (goto-char 4)
      (elpy-shell-send-statement)
      (python-shell-send-string "print('OK')\n")
      (with-current-buffer "*Python*"
        (elpy/wait-for-output "OK")
        (let ((last-hist
               (ring-ref comint-input-ring
                         (comint-previous-matching-input-string-position
                          "." 1))))
          (should (string= "def foo(a):\n  d = a + 2\n  return d"
                           last-hist)))))))

(ert-deftest elpy-shell-send-region-should-add-to-shell-history ()
  (elpy-testcase ()
    (python-mode)
    (elpy-mode)
    (let ((elpy-shell-add-to-shell-history t))
      (insert "a = 2 + 4")
      (elpy/mark-region 0 10)
      (elpy-shell-send-region-or-buffer)
      (python-shell-send-string "print('OK')\n")
      (with-current-buffer "*Python*"
        (elpy/wait-for-output "OK")
        (let ((last-hist
               (ring-ref comint-input-ring
                         (comint-previous-matching-input-string-position
                          "." 1))))
          (should (string= "a = 2 + 4" last-hist)))))))

(ert-deftest elpy-shell-send-region-should-NOT-add-to-shell-history ()
  (elpy-testcase ()
    (python-mode)
    (elpy-mode)
    (let ((elpy-shell-add-to-shell-history nil))
      (insert "a = 2 + 4")
      (elpy/mark-region 0 10)
      (elpy-shell-send-region-or-buffer)
      (python-shell-send-string "print('OK')\n")
      (with-current-buffer "*Python*"
        (elpy/wait-for-output "OK")
        (should-error (comint-previous-matching-input-string-position "." 1))))))

(ert-deftest elpy-shell-send-region-should-add-multilines-statements-to-shell-history ()
  (elpy-testcase ()
    (python-mode)
    (elpy-mode)
    (let ((elpy-shell-add-to-shell-history t))
      (insert "def foo(a):\n"
              "  d = a + 2\n"
              "  return d")
      (elpy/mark-region 0 35)
      (elpy-shell-send-region-or-buffer)
      (python-shell-send-string "print('OK')\n")
      (with-current-buffer "*Python*"
        (elpy/wait-for-output "OK")
        (let ((last-hist
               (ring-ref comint-input-ring
                         (comint-previous-matching-input-string-position
                          "." 1))))
          (should (string= "def foo(a):\n  d = a + 2\n  return d"
                           last-hist)))))))
