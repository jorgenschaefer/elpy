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

(ert-deftest elpy-eldoc-documentation-should-return-nil-without-defun ()
  (elpy-testcase ()
    (mletf* ((elpy-rpc-get-calltip (callback) (funcall callback nil))
             (calltip 'nothing-set)
             (eldoc-message (tip) (setq calltip tip))
             (python-info-current-defun () nil))

      (elpy-eldoc-documentation)

      (should (equal calltip nil)))))

(ert-deftest elpy-eldoc-documentation-should-return-nil-without-defun ()
  (elpy-testcase ()
    (mletf* ((elpy-rpc-get-calltip
              (callback)
              (funcall callback "multiprocessing.queues.Queue.cancel_join_thread(self)"))
             (calltip nil)
             (eldoc-message (tip) (setq calltip tip)))

      (elpy-eldoc-documentation)

      (should (equal calltip "Queue.cancel_join_thread()")))))
