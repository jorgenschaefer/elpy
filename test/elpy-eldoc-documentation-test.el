(ert-deftest elpy-eldoc-documentation-should-return-last-message ()
  (elpy-testcase ()
    (mletf* ((eldoc-last-message "last message")
             (elpy-rpc-get-calltip (callback) nil))

      (should (equal (elpy-eldoc-documentation)
                     "last message")))))

(ert-deftest elpy-eldoc-documentation-should-use-current-defun-if-no-calltip ()
  (elpy-testcase ()
    (mletf* ((elpy-rpc-get-calltip (callback) (funcall callback nil))
             (calltip nil)
             (eldoc-message (tip) (setq calltip tip))
             (python-info-current-defun () "FooClass.method"))

      (elpy-eldoc-documentation)

      (should (equal calltip "In: FooClass.method()")))))

(ert-deftest elpy-eldoc-documentation-should-not-display-current-defun-if-no-calltip-and-show-defun-disabled ()
  (elpy-testcase ()
    (mletf* ((elpy-rpc-get-calltip (callback) (funcall callback nil))
             (calltip nil)
             (eldoc-message (tip) (setq calltip tip))
             (python-info-current-defun () "FooClass.method")
             (elpy-eldoc-show-current-function nil))

      (elpy-eldoc-documentation)

      (should (null calltip)))))

(ert-deftest elpy-eldoc-documentation-should-return-nil-without-defun ()
  (elpy-testcase ()
    (mletf* ((elpy-rpc-get-calltip (callback) (funcall callback nil))
             (calltip 'nothing-set)
             (eldoc-message (tip) (setq calltip tip))
             (python-info-current-defun () nil))

      (elpy-eldoc-documentation)

      (should (equal calltip nil)))))

(ert-deftest elpy-eldoc-documentation-should-show-string-calltip ()
  (elpy-testcase ()
    (mletf* ((elpy-rpc-get-calltip
              (callback)
              (funcall callback "Queue.cancel_join_thread()"))
             (calltip nil)
             (eldoc-message (tip) (setq calltip tip)))

      (elpy-eldoc-documentation)

      (should (equal calltip "Queue.cancel_join_thread()")))))

(ert-deftest elpy-eldoc-documentation-should-show-object-calltip ()
  (elpy-testcase ()
    (mletf* ((elpy-rpc-get-calltip
              (callback)
              (funcall callback '((name . "cancel_join_thread")
                                  (index . 0)
                                  (params "foo" "bar"))))
             (calltip nil)
             (eldoc-message (tip) (setq calltip tip)))

      (elpy-eldoc-documentation)

      (should (equal calltip
                     (format "cancel_join_thread(%s, bar)"
                             (propertize "foo"
                                         'face
                                         'eldoc-highlight-function-argument))))
      )))

(ert-deftest elpy-eldoc-documentation-should-not-fail-for-index-nil ()
  (elpy-testcase ()
    (mletf* ((elpy-rpc-get-calltip
              (callback)
              (funcall callback '((name . "cancel_join_thread")
                                  (index . nil)
                                  (params . nil))))
             (calltip nil)
             (eldoc-message (tip) (setq calltip tip)))

      (elpy-eldoc-documentation)

      (should (equal calltip "cancel_join_thread()")))))
