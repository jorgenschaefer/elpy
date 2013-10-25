(ert-deftest elpy-installation-instructions-should-run ()
  (unwind-protect
      (progn
        (elpy-installation-instructions "Hello, World")
        (with-current-buffer "*Elpy Installation*"
          (should (get-buffer-window (current-buffer)))
          (should (equal (point-min)
                         (window-start (selected-window))))
          (should (re-search-forward "Hello, World"))))
    (ignore-errors
      (kill-buffer "*Elpy Installation*"))))

(ert-deftest elpy-installation-command-should-use-pip ()
  (mocker-let ((getenv (name)
                       ((:input '("VIRTUAL_ENV") :output "somevenv")))
               (executable-find (name)
                                ((:input '("pip") :output "/usr/bin/pip"))))
    (with-temp-buffer
      (elpy-installation-command "module")
      (let ((command (get-text-property (point-min) 'command)))
        (should (equal command "pip install module")))))
  (mocker-let ((getenv (name)
                       ((:input '("VIRTUAL_ENV") :output nil)))
               (executable-find (name)
                                ((:input '("pip") :output "/usr/bin/pip"))))
    (with-temp-buffer
      (elpy-installation-command "module")
      (let ((command (get-text-property (point-min) 'command)))
        (should (equal command "pip install --user module"))))))

(ert-deftest elpy-installation-command-should-use-easy-install ()
  (mocker-let ((getenv (name)
                       ((:input '("VIRTUAL_ENV") :output "somevenv")))
               (executable-find (name)
                                ((:input '("pip") :output nil)
                                 (:input '("easy_install")
                                         :output "/usr/bin/easy_install"))))
    (with-temp-buffer
      (elpy-installation-command "module")
      (let ((command (get-text-property (point-min) 'command)))
        (should (equal command "easy_install module")))))
  (mocker-let ((getenv (name)
                       ((:input '("VIRTUAL_ENV") :output nil)))
               (executable-find (name)
                                ((:input '("pip") :output nil)
                                 (:input '("easy_install")
                                         :output "/usr/bin/easy_install"))))
    (with-temp-buffer
      (elpy-installation-command "module")
      (let ((command (get-text-property (point-min) 'command)))
        (should (equal command "easy_install --user module"))))))

(ert-deftest elpy-installation-command-fall-back-for-neither-pip-nor-ez ()
  (mocker-let ((getenv (name)
                       ((:input '("VIRTUAL_ENV") :output "somevenv")))
               (executable-find (name)
                                ((:input '("pip") :output nil)
                                 (:input '("easy_install") :output nil))))
    (with-temp-buffer
      (elpy-installation-command "module")
      (goto-char (point-min))
      (should (search-forward "It appears you have neither" nil t)))))
