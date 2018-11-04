;; Better way of doing tests? Since, I actually need a Django project
;; to test out the `runserver' command
(ert-deftest elpy-module-django-buffer-init ()
  "elpy-django should not be activated since it won't find the
`manage.py' file."
  (elpy-testcase ()
    (elpy-module-django 'buffer-init)

    (should (not elpy-django))))

(ert-deftest elpy-module-django-buffer-stop ()
  (elpy-testcase ()
    (elpy-module-django 'buffer-stop)

    (should (not elpy-django))))

(ert-deftest elpy-module-django-buffer-init-with-manage-file ()
  "When doing a buffer-init, elpy-django should activate when finding
the `manage.py' file."
  (elpy-testcase ()
    (with-temp-file (concat default-directory "manage.py"))
    (elpy-module-django 'buffer-init)

    (should elpy-django)
    (should (string= elpy-django-command (expand-file-name (concat default-directory "manage.py"))))

    (delete-file (concat default-directory "manage.py"))))

(ert-deftest elpy-module-django-manual-init ()
  "When turning elpy-django manually, `elpy-django-command' should
default to `django-admin.py'."
  (elpy-testcase ()
    (elpy-django 1)

    (should elpy-django)
    (should (string= "django-admin.py" elpy-django-command))))

(ert-deftest elpy-module-django-command ()
  (mletf* ((compile (arg) arg)
           (output (elpy-django-command "migrate")))
          (should (string= output "django-admin.py migrate"))))

(ert-deftest elpy-module-django-commands-with-required-arg ()
  (dolist (cmd elpy-django-commands-with-req-arg)
    (mletf* ((compile (arg) arg)
             (read-shell-command (arg1 agr2) "test")
             (output (elpy-django-command cmd)))
            (should (string= output (concat "django-admin.py " cmd " test"))))))

;; Test the parsing. Also, another way of shortening string?
;; Already made it shorter but still seems to long
(ert-deftest elpy-module-django-get-commands ()
  (mletf* ((shell-command-to-string (arg) "Type 'manage.py help <subcommand>' for help on a specific subcommand.

Available subcommands:

[auth]
    changepassword

[django]
    migrate

[sessions]
    clearsessions

[staticfiles]
    runserver
"))
    (should (member "runserver" (elpy-django--get-commands)))
    (should (member "clearsessions" (elpy-django--get-commands)))
    (should (member "migrate" (elpy-django--get-commands)))
    (should (member "changepassword" (elpy-django--get-commands)))))

(ert-deftest elpy-module-django-get-test-runner-should-error-if-no-django-settings-module-environment ()
  (setenv "DJANGO_SETTINGS_MODULE" nil)
  (should-error (elpy-django--get-test-runner)))

(ert-deftest elpy-module-django-get-test-runner-should-error-if-cannot-import-django-settings-module ()
  (setenv "DJANGO_SETTINGS_MODULE" "popcorn")
  (should-error (elpy-django--get-test-runner)))

(ert-deftest elpy-module-django-get-test-format-regex-match ()
  "elpy-django--get-test-runner should interpreter elpy-django-test-runner-formats
keys as regular expression"
  (mletf* ((elpy-django--get-test-runner () "package.mod.Runner")
           (elpy-django-test-runner-formats
            '(("package.mod.Runner2" . ".") ("Runner$" . "~"))))
          (should (equal (elpy-django--get-test-format) "~"))))

(ert-deftest elpy-module-django-get-test-format-dot-default ()
  "elpy-django--get-test-runner should default to `.`"
  (mletf* ((elpy-django--get-test-runner () "a.b.TestRunner")
           (should (equal (elpy-django--get-test-format) ".")))))


(ert-deftest elpy-module-django-get-test-format-should-error-with-no-matching-format ()
  "elpy-django--get-test-runner should default to `.`"
  (mletf* ((elpy-django--get-test-runner () "Runner3")
           (elpy-django-test-runner-formats
            '(("Runner2$" . ".") ("Runner4$" . "~"))))
          (should-error (elpy-django--get-test-format))))


(ert-deftest elpy-module-django-get-test-runner-should-cache-result ()
  (setenv "DJANGO_SETTINGS_MODULE" "popcorn")
  (mletf* ((call-process (&rest args) 0)
           (shell-command-to-string (&rest args) "my.Runner")
           (elpy-project-root () "/proj/root/"))
          (elpy-django--get-test-runner)
          (let ((cached-runner (cdr (assoc '("/proj/root/" "popcorn")
                                           elpy-django--test-runner-cache))))
            (should (equal cached-runner "my.Runner")))))


(ert-deftest elpy-module-django-get-test-runner-should-use-cached-result ()
  (setenv "DJANGO_SETTINGS_MODULE" "popcorn")
  (mletf* ((elpy-project-root () "/proj/root/")
           (elpy-django--test-runner-cache
            '((("/proj/root/" "popcorn") . "my.Runner"))))
          (should (equal (elpy-django--get-test-runner) "my.Runner"))))

(ert-deftest elpy-module-django-get-test-runner-cache-size-limit ()
  (mletf* ((elpy-django--test-runner-cache-max-size 7)
           (getenv (var) "djsettings")
           (call-process (&rest args) 0)
           )
          (dotimes (i 5)
            (mletf* ((elpy-project-root () (format "/proj/root/%d" i))
                     (shell-command-to-string (&rest args)
                                              (format "my.Runner%d" i)))
                    (elpy-django--get-test-runner)))

          (should (equal (length elpy-django--test-runner-cache) 5))

          (dotimes (i 5)
            (mletf* ((elpy-project-root () (format "/proj/root/%d" (+ i 5)))
                     (shell-command-to-string (&rest args)
                                              (format "my.Runner%d" (+ i 5))))
                    (elpy-django--get-test-runner)))

          (should (equal (length elpy-django--test-runner-cache) 7))))
          
