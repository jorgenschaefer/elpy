(ert-deftest elpy-enable-should-fail-for-old-emacsen ()
  (let ((emacs-major-version 23))
    (should-error (elpy-enable))))

(ert-deftest elpy-enable-should-fail-for-bad-python-modes ()
  ;; Make sure the API is the same still
  (should (string-match "/lisp/progmodes/python.el$"
                        (find-lisp-object-file-name 'python-mode
                                                    'symbol-function)))
  (mocker-let ((find-lisp-object-file-name
                (object type)
                ((:input '(python-mode symbol-function)
                  :output "/opt/emacs/lisp/progmodes/python-mode.el"))))
    (should-error (elpy-enable))))

(ert-deftest elpy-enable-should-add-hook ()
  (let ((python-mode-hook nil))
    (elpy-enable)
    (should (memq 'elpy-mode python-mode-hook))))

(ert-deftest elpy-disable-should-remove-elpy-mode ()
  (elpy-enable)
  (let ((python-mode-hook '(elpy-mode)))
    (elpy-disable)
    (should (eq python-mode-hook nil))))

(ert-deftest elpy-mode-should-fail-outside-of-python-mode ()
  (with-temp-buffer
    (should-error (elpy-mode 1))))

(ert-deftest elpy-mode-should-initialize-ffip-project-root ()
  (mocker-let ((elpy-project-root ()
                                  ((:output "/opt/root")))
               (flymake-mode (start)
                             ((:input '(1)
                               :output nil))))
    ;; Without file name
    (with-temp-buffer
      (python-mode)
      (let ((elpy-default-minor-modes nil))
        (elpy-mode 1))
      (should (equal ffip-project-root nil)))
    ;; With file name
    (with-temp-buffer
      (python-mode)
      (let ((buffer-file-name "/opt/root/mumble.py")
            (elpy-default-minor-modes nil))
        (elpy-mode 1))
      (should (equal ffip-project-root "/opt/root")))))
