;;; elpy-django.el --- Django extension for elpy

;; Copyright (C) 2013-2016  Jorgen Schaefer

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
  :group 'elpy)
(make-variable-buffer-local 'elpy-django-command)

(defcustom elpy-django-server-ipaddr "127.0.0.1"
  "What address Django will use when running the dev server."
  :type 'string
  :type 'stringp
  :group 'elpy)
(make-variable-buffer-local 'elpy-django-server-ipaddr)

(defcustom elpy-django-server-port "8000"
  "What port Django will use when running the dev server."
  :type 'string
  :type 'stringp
  :group 'elpy)
(make-variable-buffer-local 'elpy-django-server-port)

(defcustom elpy-django-always-prompt nil
  "When non-nil, it will always prompt for extra arguments
to pass with the chosen command."
  :type 'boolean
  :type 'booleanp
  :group 'elpy)
(make-variable-buffer-local 'elpy-django-always-prompt)

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
          (concat (locate-dominating-file default-directory "manage.py") "manage.py"))
    (elpy-django 1)))

(defun elpy-django--get-commands ()
  "Return list of django commands."
  (let ((dj-commands-str nil)
        (help-output
         (shell-command-to-string (concat elpy-django-command " -h"))))
    (setq dj-commands-str
          (with-temp-buffer
            (progn
              (insert help-output)
              (beginning-of-buffer)
              (delete-region (point) (search-forward "Available subcommands:" nil nil nil))
              ;; cleanup [auth] and stuff
              (beginning-of-buffer)
              (save-excursion
                (replace-regexp "\\[.*\\]" ""))
              (buffer-string))))
    ;; get a list of commands from the output of manage.py -h
    ;; What would be the pattern to optimize this ?
    (setq dj-commands-str (split-string dj-commands-str "\n"))
    (setq dj-commands-str (-remove (lambda (x) (string= x "")) dj-commands-str))
    (setq dj-commands-str (mapcar (lambda (x) (s-trim x)) dj-commands-str))
    (sort dj-commands-str 'string-lessp)))

;;;;;;;;;;;;;;;;;;;;;;
;;; User Functions

(defun elpy-django-command (cmd)
  "Prompt user for Django command. If called with `C-u',
it will prompt for other flags/arguments to run."
  (interactive (list (completing-read "Command: " (elpy-django--get-commands) nil nil)))
  ;; Called with C-u or variable is set
  (when (or current-prefix-arg elpy-django-always-prompt)
    (setq cmd (read-shell-command (concat cmd ": ") "--noinput")))
  (compile (concat elpy-django-command " " cmd)))

(defun elpy-django-runserver (arg)
  "Start the server and automatically add the ipaddr and port.
Also create it's own special buffer so that we can have multiple
servers running per project.

When called with a prefix (C-u), it will prompt for additional args."
  (interactive "P")
  (let* ((cmd (concat elpy-django-command " runserver"))
         (proj-root (file-name-base (directory-file-name (elpy-project-root))))
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

(define-minor-mode elpy-django
  "Minor mode to for Django commands."
  :group 'elpy)

(provide 'elpy-django)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; elpy-django.el ends here
