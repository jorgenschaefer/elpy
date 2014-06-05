(ert-deftest elpy-multiedit-python-symbol-at-point-should-stop-if-active ()
  (elpy-testcase ()
    (mletf* ((stop-called nil)
             (elpy-multiedit-stop () (setq stop-called t))
             (elpy-multiedit-overlays '(t)))

      (elpy-multiedit-python-symbol-at-point)

      (should stop-called))))

(ert-deftest elpy-multiedit-python-symbol-at-point-should-edit-symbol-with-prefix-arg ()
  (elpy-testcase ()
    (mletf* ((interactive-called nil)
             (call-interactively (symbol) (setq interactive-called symbol)))

      (elpy-multiedit-python-symbol-at-point '(4))

      (should (equal interactive-called 'elpy-multiedit)))))

(ert-deftest elpy-multiedit-python-symbol-at-point-should-call-rpc-usages ()
  (elpy-testcase ()
    (mletf* ((usages-called nil)
             (elpy-rpc-get-usages () (setq usages-called t) nil))

      (elpy-multiedit-python-symbol-at-point)

      (should usages-called))))

(ert-deftest elpy-multiedit-python-symbol-at-point-should-edit-symbol-if-backend-errors ()
  (elpy-testcase ()
    (mletf* ((interactive-called nil)
             (call-interactively (symbol) (setq interactive-called symbol))
             (elpy-rpc-get-usages
              ()
              (error "get_usages not implemented by current backend")))

      (elpy-multiedit-python-symbol-at-point)

      (should (equal interactive-called 'elpy-multiedit)))))

(ert-deftest elpy-multiedit-python-symbol-at-point-should-save-some-buffers ()
  (elpy-testcase ()
    (mletf* ((save-some-buffers-called nil)
             (save-some-buffers () (setq save-some-buffers-called t)))

      (elpy-multiedit-python-symbol-at-point)

      (should save-some-buffers-called))))
