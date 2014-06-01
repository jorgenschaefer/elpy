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

      (should interactive-called))))

(ert-deftest elpy-multiedit-python-symbol-at-point-should-call-rpc-usages ()
  (elpy-testcase ()
    (mletf* ((usages-called nil)
             (elpy-rpc-get-usages () (setq usages-called t) nil))

      (elpy-multiedit-python-symbol-at-point)

      (should usages-called))))
