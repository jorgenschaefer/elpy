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

(ert-deftest elpy-enable-should-initialize-variables ()
  (mocker-let ((elpy-initialize-variables ()
                                          ((:min-occur 1))))
    (elpy-enable))
  (mocker-let ((elpy-initialize-variables ()
                                          ((:min-occur 0
                                            :max-occur 0))))
    (elpy-enable t)))

(ert-deftest elpy-disable-should-remove-elpy-mode ()
  (let ((python-mode-hook '(elpy-mode)))
    (elpy-disable)
    (should (eq python-mode-hook nil))))

(ert-deftest elpy-mode-should-fail-outside-of-python-mode ()
  (with-temp-buffer
    (should-error (elpy-mode 1))))

(ert-deftest elpy-mode-should-initialize-ffip-project-root ()
  (mocker-let ((elpy-project-root ()
                                  ((:output "/opt/root"))))
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

(ert-deftest elpy-mode-should-configure-some-modes ()
  (with-temp-buffer
    (python-mode)
    (elpy-mode 1)
    (should (eq eldoc-documentation-function 'elpy-eldoc-documentation))
    (should (memq 'ac-source-elpy ac-sources))
    (should (memq 'ac-source-elpy-dot ac-sources))))

(ert-deftest elpy-mode-should-enable-minor-modes ()
  (mocker-let ((test-minor-mode (enable) ((:input '(1)
                                           :min-occur 1))))
    (let ((elpy-default-minor-modes '(test-minor-mode)))
      (with-temp-buffer
        (python-mode)
        (elpy-mode 1)))))

(ert-deftest elpy-clean-modeline-should-clean-modeline ()
  (with-temp-buffer
    (elpy-clean-modeline)
    (dolist ((mode-display minor-mode-alist))
      (when (memq (car mode-display)
                  '(elpy-mode yas/minor-mode auto-complete-mode flymake-mode))
        (should (equal (cdr mode-display)
                       ""))))))
