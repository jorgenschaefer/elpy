(ert-deftest elpy-eldoc-documentation-should-return-last-message ()
  (elpy-testcase ()
    (mletf* ((eldoc-last-message "last message")
             (elpy-rpc-get-calltip (callback) nil))

      (should (equal (elpy-eldoc-documentation)
                     "last message")))))

;; Calltip available: display that
(ert-deftest elpy-eldoc-documentation-should-show-string-calltip ()
  (elpy-testcase ()
    (mletf* ((elpy-rpc-get-calltip-or-oneline-docstring
              (callback)
              (funcall callback "Queue.cancel_join_thread()"))
             (calltip nil)
             (eldoc-message (tip) (setq calltip tip)))

      (elpy-eldoc-documentation)

      (should (equal calltip "Queue.cancel_join_thread()")))))

(ert-deftest elpy-eldoc-documentation-should-show-object-calltip ()
  (elpy-testcase ()
    (mletf* ((elpy-rpc-get-calltip-or-oneline-docstring
              (callback)
              (funcall callback '((name . "cancel_join_thread")
                                  (index . 0)
                                  (kind . "calltip")
                                  (params "foo" "bar"))))
             (calltip nil)
             (window-width (buff) 100000000)
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
    (mletf* ((elpy-rpc-get-calltip-or-oneline-docstring
              (callback)
              (funcall callback '((name . "cancel_join_thread")
                                  (index . nil)
                                  (kind . "calltip")
                                  (params . nil))))
             (calltip nil)
             ;; without UI, the minibuffer width is only 9,
             ;; leading to truncated eldoc messages
             (window-width (window &optional pixelwise) 100)
             (eldoc-message (tip) (setq calltip tip)))

      (elpy-eldoc-documentation)

      (should (equal calltip "cancel_join_thread()")))))


;; No calltip: display function oneline docstring
(ert-deftest elpy-eldoc-documentation-should-show-object-onelinedoc ()
  (elpy-testcase ()
    (mletf* ((elpy-rpc-get-calltip-or-oneline-docstring
              (callback)
              (funcall callback '((name . "cancel_join_thread()")
                                  (kind . "oneline_doc")
                                  (doc . "This function does things."))))
             (doc nil)
             (window-width (buff) 100000000)
             (eldoc-message (tip) (setq doc tip)))

      (elpy-eldoc-documentation)

      (should (equal doc (format "%s%s"
                                 (propertize "cancel_join_thread()"
                                             'face
                                             'font-lock-function-name-face)
                                 ": This function does things.")))
      )))



;; No calltip and docstring: display current edited function
(ert-deftest elpy-eldoc-documentation-should-use-current-defun-if-nothing-else ()
  (elpy-testcase ()
    (mletf* ((elpy-rpc-get-calltip-or-oneline-docstring (callback)
                                                        (funcall callback nil))
             (calltip nil)
             (eldoc-message (tip) (setq calltip tip))
             (python-info-current-defun () "FooClass.method"))

      (elpy-eldoc-documentation)

      (should (equal calltip "In: FooClass.method()")))))

;; No calltip, docstring or current function: display nothing
(ert-deftest elpy-eldoc-documentation-should-return-nil-without-defun ()
  (elpy-testcase ()
    (mletf* ((elpy-rpc-get-calltip-or-oneline-docstring (callback)
                                                        (funcall callback nil))
             (calltip nil)
             (eldoc-message (tip) (setq calltip tip))
             (python-info-current-defun () nil))

      (elpy-eldoc-documentation)

      (should (equal calltip nil)))))
