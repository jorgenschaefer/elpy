;;; elpy-django.el --- Django extension for elpy

;; Copyright (C) 2013-2019  Jorgen Schaefer

;; Author: Daniel Gopar <gopardaniel@gmail.com>
;; URL: https://github.com/jorgenschaefer/elpy

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file serves as an extension to elpy by adding django support

;;; Code:

(require 's)

;;;;;;;;;;;;;;;;;;;;;;
;;; User customization

(defcustom elpy-django-command "django-admin.py"
  "Command to use when running Django specific commands.
Best to set it to full path to 'manage.py' if it's available."
  :type 'string
  :safe 'stringp
  :group 'elpy-django)
(make-variable-buffer-local 'elpy-django-command)

(defcustom elpy-django-server-ipaddr "127.0.0.1"
  "What address Django will use when running the dev server."
  :type 'string
  :safe 'stringp
  :group 'elpy-django)
(make-variable-buffer-local 'elpy-django-server-ipaddr)

(defcustom elpy-django-server-port "8000"
  "What port Django will use when running the dev server."
  :type 'string
  :safe 'stringp
  :group 'elpy-django)
(make-variable-buffer-local 'elpy-django-server-port)

(defcustom elpy-django-server-command "runserver"
  "When executing `elpy-django-runserver' what should be the server
command to use."
  :type 'string
  :safe 'stringp
  :group 'elpy-django)
(make-variable-buffer-local 'elpy-django-server-command)

(defcustom elpy-django-always-prompt nil
  "When non-nil, it will always prompt for extra arguments
to pass with the chosen command."
  :type 'boolean
  :safe 'booleanp
  :group 'elpy-django)
(make-variable-buffer-local 'elpy-django-always-prompt)

(defcustom elpy-django-commands-with-req-arg '("startapp" "startproject"
                                               "loaddata" "sqlmigrate"
                                               "sqlsequencereset"
                                               "squashmigrations")
  "Used to determine if we should prompt for arguments. Some commands
require arguments in order for it to work."
  :type 'list
  :safe 'listp
  :group 'elpy-django)
(make-variable-buffer-local 'elpy-django-commands-with-req-arg)

(defcustom elpy-django-test-runner-formats '(("django_nose.NoseTestSuiteRunner" . ":")
                                             (".*" . "."))
  "List of test runners and their format for calling tests.

  The keys are the regular expressions to match the runner used in test,
while the values are the separators to use to build test target path.
Some tests runners are called differently. For example, Nose requires a ':' when calling specific tests,
but the default Django test runner uses '.'"
  :type 'list
  :safe 'listp
  :group 'elpy-django)
(make-variable-buffer-local 'elpy-django-test-runner-formats)

(defcustom elpy-django-test-runner-args '("test" "--noinput")
  "Arguments to pass to the test runner when calling tests."
  :type '(repeat string)
  :group 'elpy-django)
(make-variable-buffer-local 'elpy-django-test-runner-args)

(defcustom elpy-test-django-runner-command nil
  "Deprecated. Please define Django command in `elpy-django-command' and
test arguments in `elpy-django-test-runner-args'"
  :type '(repeat string)
  :group 'elpy-django)
(make-obsolete-variable 'elpy-test-django-runner-command nil "March 2018")

(defcustom elpy-test-django-runner-manage-command nil
  "Deprecated. Please define Django command in `elpy-django-command' and
test arguments in `elpy-django-test-runner-args'."
  :type '(repeat string)
  :group 'elpy-django)
(make-obsolete-variable 'elpy-test-django-runner-manage-command nil "March 2018")

(defcustom elpy-test-django-with-manage nil
  "Deprecated.  Please define Django command in `elpy-django-command' and
test arguments in `elpy-django-test-runner-args'."
  :type 'boolean
  :group 'elpy-django)
(make-obsolete-variable 'elpy-test-django-with-manage nil "March 2018")

;;;;;;;;;;;;;;;;;;;;;;
;; Key map

(defvar elpy-django-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") 'elpy-django-command)
    (define-key map (kbd "r") 'elpy-django-runserver)
    map)
  "Key map for django extension")

;;;;;;;;;;;;;;;;;;;;;;
;;; Helper Functions

(defun elpy-django-setup ()
  "Decides whether to start the minor mode or not."
  ;; Make sure we're in an actual file and we can find
  ;; manage.py. Otherwise user will have to manually
  ;; start this mode if they're using 'django-admin.py'
  (when (locate-dominating-file default-directory "manage.py")
    ;; Let's be nice and point to full path of 'manage.py'
    ;; This only affects the buffer if there's no directory
    ;; variable overwriting it.
    (setq elpy-django-command
          (expand-file-name (concat (locate-dominating-file default-directory "manage.py") "manage.py")))
    (elpy-django 1)))

(defun elpy-project-find-django-root ()
  "Return the current Django project root, if any.

This is marked with 'manage.py' or 'django-admin.py'."
  (or (locate-dominating-file default-directory "django-admin.py")
      (locate-dominating-file default-directory "manage.py")))

(defun elpy-django--get-commands ()
  "Return list of django commands."
  (let ((dj-commands-str nil)
        (help-output
         (shell-command-to-string (concat elpy-django-command " -h"))))
    (setq dj-commands-str
          (with-temp-buffer
            (progn
              (insert help-output)
              (goto-char (point-min))
              (delete-region (point) (search-forward "Available subcommands:" nil nil nil))
              ;; cleanup [auth] and stuff
              (goto-char (point-min))
              (save-excursion
                (while (re-search-forward "\\[.*\\]" nil t)
                  (replace-match "" nil nil)))
              (buffer-string))))
    ;; get a list of commands from the output of manage.py -h
    ;; What would be the pattern to optimize this ?
    (setq dj-commands-str (split-string dj-commands-str "\n"))
    (setq dj-commands-str (cl-remove-if (lambda (x) (string= x "")) dj-commands-str))
    (setq dj-commands-str (mapcar (lambda (x) (s-trim x)) dj-commands-str))
    (sort dj-commands-str 'string-lessp)))


(defvar elpy-django--test-runner-cache nil
  "Internal cache for elpy-django--get-test-runner.
The cache is keyed on project root and DJANGO_SETTINGS_MODULE env var")

(defvar elpy-django--test-runner-cache-max-size 100
  "Maximum number of entries in test runner cache")


(defun elpy-django--get-test-runner ()
  "Return the name of the django test runner.
Needs `DJANGO_SETTINGS_MODULE' to be set in order to work.
The result is memoized on project root and `DJANGO_SETTINGS_MODULE'"
  (let ((django-import-cmd "import django;django.setup();from django.conf import settings;print(settings.TEST_RUNNER)")
        (django-settings-env (getenv "DJANGO_SETTINGS_MODULE"))
        (default-directory (elpy-project-root)))
    ;; If no Django settings has been set, then nothing will work. Warn user
    (unless django-settings-env
      (error "Please set environment variable `DJANGO_SETTINGS_MODULE' if you'd like to run the test runner"))

    (let* ((runner-key (list default-directory django-settings-env))
           (runner (or (elpy-django--get-test-runner-from-cache runner-key)
                       (elpy-django--cache-test-runner
                        runner-key
                        (elpy-django--detect-test-runner django-settings-env)))))
      (elpy-django--limit-test-runner-cache-size)
      runner)))


(defun elpy-django--get-test-format ()
  "When running a Django test, some test runners require a different format that others.
Return the correct string format here."
  (let ((runner (elpy-django--get-test-runner))
        (found nil)
        (formats elpy-django-test-runner-formats))
    (while (and formats (not found))
      (let* ((entry (car formats)) (regex (car entry)))
        (when (string-match regex runner)
          (setq found (cdr entry))))
      (setq formats (cdr formats)))
    (or found (error (format "Unable to find test format for `%s'"
                             (elpy-django--get-test-runner))))))


(defun elpy-django--detect-test-runner (django-settings-env)
  "Detects django test runner in current configuration"
  ;; We have to be able to import the DJANGO_SETTINGS_MODULE to detect test
  ;; runner; if python process importing settings exits with error,
  ;; then warn the user that settings is not valid
  (unless (= 0 (call-process elpy-rpc-python-command nil nil nil
                             "-c" (format "import %s" django-settings-env)))
    (error (format "Unable to import DJANGO_SETTINGS_MODULE: '%s'"
                   django-settings-env)))
  (s-trim (shell-command-to-string
                       (format "%s -c '%s'" elpy-rpc-python-command
                               django-import-cmd))))


(defun elpy-django--get-test-runner-from-cache (key)
  "Retrieve from cache test runner with given caching key.
Return nil if the runner is missing in cache"
  (let ((runner (cdr (assoc key elpy-django--test-runner-cache))))
    ;; if present re-add to implement lru cache
    (when runner (elpy-django--cache-test-runner key runner))))


(defun elpy-django--cache-test-runner (key runner)
  "Store in test runner cache a runner with a key"""
  (push (cons key runner) elpy-django--test-runner-cache)
  runner)


(defun elpy-django--limit-test-runner-cache-size ()
  "Ensure elpy-django--test-runner-cache does not overflow a fixed size"
  (while (> (length elpy-django--test-runner-cache)
            elpy-django--test-runner-cache-max-size)
    (setq elpy-django--test-runner-cache (cdr elpy-django--test-runner-cache))))


;;;;;;;;;;;;;;;;;;;;;;
;;; User Functions

(defun elpy-django-command (cmd)
  "Prompt user for Django command. If called with `C-u',
it will prompt for other flags/arguments to run."
  (interactive (list (completing-read "Command: " (elpy-django--get-commands) nil nil)))
  ;; Called with C-u, variable is set or is a cmd that requires an argument
  (when (or current-prefix-arg
            elpy-django-always-prompt
            (member cmd elpy-django-commands-with-req-arg))
    (setq cmd (concat cmd " " (read-shell-command (concat cmd ": ") "--noinput"))))
  ;;
  (cond ((string= cmd "shell")
         (run-python (concat elpy-django-command " shell -i python") t t))
        (t
         (let* ((program (car (split-string elpy-django-command)))
                (args (cdr (split-string elpy-django-command)))
                (buffer-name (format "django-%s" (car (split-string cmd)))))
           (when (get-buffer (format "*%s*" buffer-name))
             (kill-buffer (format "*%s*" buffer-name)))
           (pop-to-buffer
            (apply 'make-comint buffer-name program nil
                   (append args (split-string cmd))))))))

(defun elpy-django-runserver (arg)
  "Start the server and automatically add the ipaddr and port.
Also create it's own special buffer so that we can have multiple
servers running per project.

When called with a prefix (C-u), it will prompt for additional args."
  (interactive "P")
  (let* ((cmd (concat elpy-django-command " " elpy-django-server-command))
         (proj-root (if (elpy-project-root)
                        (file-name-base (directory-file-name
                                         (elpy-project-root)))
                      (message "Elpy cannot find the root of the current django project. Starting the server in the current directory: '%s'."
                               default-directory)
                      default-directory))
         (buff-name (format "*runserver[%s]*" proj-root)))
    ;; Kill any previous instance of runserver since we might be doing something new
    (when (get-buffer buff-name)
      (kill-buffer buff-name))
    (setq cmd (concat cmd " " elpy-django-server-ipaddr ":" elpy-django-server-port))
    (when (or arg elpy-django-always-prompt)
      (setq cmd (concat cmd " "(read-shell-command (concat cmd ": ")))))
    (compile cmd)
    (with-current-buffer "*compilation*"
      (rename-buffer buff-name))))

(defun elpy-test-django-runner (top _file module test)
  "Test the project using the Django discover runner,
or with manage.py if elpy-test-django-with-manage is true.

This requires Django 1.6 or the django-discover-runner package."
  (interactive (elpy-test-at-point))
  (if module
      (apply #'elpy-test-run
             top
             (append
              (list elpy-django-command)
              elpy-django-test-runner-args
              (list (if test
                        (format "%s%s%s" module (elpy-django--get-test-format) test)
                      module))))
    (apply #'elpy-test-run
           top
           (append
            (list elpy-django-command)
            elpy-django-test-runner-args))))
(put 'elpy-test-django-runner 'elpy-test-runner-p t)

(define-minor-mode elpy-django
  "Minor mode for Django commands."
  :group 'elpy-django)

(provide 'elpy-django)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; elpy-django.el ends here
