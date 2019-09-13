;;; elpy-rpc.el --- RPC protocol for elpy -*- lexical-binding: t -*-
;;
;; Copyright (C) 2012-2019  Jorgen Schaefer
;;
;; Author: Jorgen Schaefer <contact@jorgenschaefer.de>, Rainer Gemulla <rgemulla@gmx.de>, Gaby Launay <gaby.launay@protonmail.com>
;; URL: https://github.com/jorgenschaefer/elpy
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; elpy-rpc is a simple JSON-based RPC protocol. It's mostly JSON-RPC
;; 1.0, except we do not implement the full protocol as we do not need
;; all the features. Emacs starts a Python subprocess which runs a
;; special module. The module reads JSON-RPC requests and responds
;; with JSON-RPC responses.
;;
;;; Code:

(require 'json)
(require 'pyvenv)

(defcustom elpy-rpc-maximum-buffer-age (* 5 60)
  "Seconds after which Elpy automatically closes an unused RPC buffer.

Elpy creates RPC buffers over time, depending on python interpreters
and the project root. When there are many projects being worked on,
these can accumulate. Setting this variable to an integer will close
buffers and processes when they have not been used for this amount of
seconds.

Setting this variable to nil will disable the behavior."
  :type '(choice (const :tag "Never" nil)
                 integer)
  :group 'elpy)

(defcustom elpy-rpc-large-buffer-size 4096
  "Size for a source buffer up to which it will be sent directly.

The Elpy RPC protocol uses JSON as the serialization format.
Large buffers take a long time to encode, so Elpy can transmit
them via temporary files. If a buffer is larger than this value,
it is sent via a temporary file."
  :type 'integer
  :safe #'integerp
  :group 'elpy)

(defcustom elpy-rpc-ignored-buffer-size 102400
  "Size for a source buffer over which Elpy completion will not work.

To provide completion, Elpy's backends have to parse the whole
file every time. For very large files, this is slow, and can make
Emacs laggy. Elpy will simply not work on buffers larger than
this to prevent this from happening."
  :type 'integer
  :safe #'integerp
  :group 'elpy)

(defcustom elpy-rpc-python-command (if (equal system-type 'windows-nt)
                                       (or (executable-find "py")
                                           (executable-find "pythonw")
                                           "python")
                                     "python")
  "The Python interpreter for the RPC backend.

This should be the same interpreter the project will be run with,
and not an interactive shell like ipython."
  :type '(choice (const :tag "python" "python")
                 (const :tag "python2" "python2")
                 (const :tag "python3" "python3")
                 (const :tag "pythonw (Python on Windows)" "pythonw")
                 (const :tag "py (other Python on Windows)" "py")
                 (string :tag "Other"))
  :safe (lambda (val)
          (member val '("python" "python2" "python3" "pythonw")))
  ;; Make sure there is no obsolete rpc running
  :set (lambda (var val)                ;
         (set-default var val)
         (when (fboundp 'elpy-rpc-restart)
           (elpy-rpc-restart)))
  :group 'elpy)

(defcustom elpy-rpc-pythonpath (file-name-directory (locate-library "elpy"))
  "A directory to add to the PYTHONPATH for the RPC process.

This should be a directory where the elpy module can be found. If
this is nil, it's assumed elpy can be found in the standard path.
Usually, there is no need to change this."
  :type 'directory
  :safe #'file-directory-p
  :group 'elpy)

(defcustom elpy-rpc-timeout 1
  "Number of seconds to wait for a response when blocking.

When Elpy blocks Emacs to wait for a response from the RPC
process, it will assume it won't come or wait too long after this
many seconds. On a slow computer, or if you have a large project,
you might want to increase this.

A setting of nil means to block indefinitely."
  :type '(choice (const :tag "Block indefinitely" nil)
                 integer)
  :safe (lambda (val)
          (or (integerp val)
              (null val)))
  :group 'elpy)

(defcustom elpy-rpc-error-timeout 30
  "Minimum number of seconds between error popups.

When Elpy encounters an error in the backend, it will display a
lengthy description of the problem for a bug report. This hangs
Emacs for a moment, and can be rather annoying if it happens
repeatedly while editing a source file.

If this variabl is non-nil, Elpy will not display the error
message again within this amount of seconds."
  :type 'integer
  :group 'elpy)

(defvar elpy-rpc--call-id 0
  "Call id of the last call to `elpy-rpc`.

Used to associate responses to callbacks.")
(make-variable-buffer-local 'elpy-rpc--call-id)

(defvar elpy-rpc--buffer-p nil
  "Non-nil if the current buffer is an elpy-rpc buffer.")
(make-variable-buffer-local 'elpy-rpc--buffer-p)

(defvar elpy-rpc--buffer nil
  "The elpy-rpc buffer associated with this buffer.")
(make-variable-buffer-local 'elpy-rpc--buffer)

(defvar elpy-rpc--backend-library-root nil
  "The project root used by this backend.")
(make-variable-buffer-local 'elpy-rpc--backend-library-root)

(defvar elpy-rpc--backend-python-command nil
  "The Python interpreter used by this backend.")
(make-variable-buffer-local 'elpy-rpc--backend-python-command)

(defvar elpy-rpc--backend-callbacks nil
  "The callbacks registered for calls to the current backend.

This maps call IDs to functions.")
(make-variable-buffer-local 'elpy-rpc--backend-callbacks)

(defvar elpy-rpc--last-call nil
  "The time of the last RPC call issued for this backend.")
(make-variable-buffer-local 'elpy-rpc--last-call)

(defvar elpy-rpc--last-error-popup nil
  "The last time an error popup happened.")

(defvar elpy-rpc--jedi-available nil
  "Whether jedi is available or not.")

;;;;;;;;;;;;;;;;;;;
;;; RPC virualenv

(defcustom elpy-rpc-virtualenv-path
  (lambda ()
    (concat (file-name-as-directory (pyvenv-workon-home))
            "elpy-rpc-venv"))
  "Path to the virtualenv used by the RPC.

It can also be a function returning the path to the virtualenv.

If it does not exist, the virtualenv will be created using
`elpy-rpc-python-command' and populated with the needed
packages from `elpy-rpc--get-package-list'."
  :type '(choice (string :tag "Virtualenv path")
                (function :tag "Function returning the virtualenv path"))
  :group 'elpy)

(defun elpy-rpc-get-virtualenv-path ()
  "Return the RPC virutalenv path to use."
  (expand-file-name
   (if (stringp elpy-rpc-virtualenv-path)
       elpy-rpc-virtualenv-path
       (funcall elpy-rpc-virtualenv-path))))

(defun elpy-rpc--get-package-list ()
  "Return the list of packages to be installed in the RPC virtualenv."
  (let ((rpc-python-version (elpy-rpc--get-python-version)))
    (if (version< rpc-python-version "3.6.0")
        '("jedi" "flake8" "autopep8" "yapf" "rope")
      '("jedi" "flake8" "autopep8" "yapf" "black" "rope"))))

(defun elpy-rpc--get-python-version ()
  "Return the RPC python version."
  (with-temp-buffer
    (call-process elpy-rpc-python-command nil t nil "--version")
    (goto-char (point-min))
    (re-search-forward "Python \\([0-9.]+\\)")
    (match-string 1)))

(defmacro with-elpy-rpc-virtualenv-activated (&rest body)
  "Run BODY with Elpy's RPC virtualenv activated.

The current virtualenv name is bounded to the
`deactivated-environment' variable during the execution of
BODY."
  `(if (not (executable-find elpy-rpc-python-command))
       (error "Cannot find executable '%s', please set 'elpy-rpc-python-command' to an existing executable." elpy-rpc-python-command)
     (let ((venv-was-activated pyvenv-virtual-env)
           (pyvenv-post-activate-hooks (remq 'elpy-rpc--disconnect
                                             pyvenv-post-activate-hooks))
           (pyvenv-post-deactivate-hooks (remq 'elpy-rpc--disconnect
                                               pyvenv-post-deactivate-hooks))
           (deactivated-environment
            (or pyvenv-virtual-env
                ;; global env
                (directory-file-name
                 (file-name-directory
                  (directory-file-name
                   (file-name-directory
                    (executable-find elpy-rpc-python-command))))))))
       (pyvenv-activate (elpy-rpc-get-or-create-virtualenv))
       (let (venv-err result)
         (condition-case err
             (setq result (progn ,@body))
           (error (setq venv-err (car (cdr err)))))
         (if venv-was-activated
             (pyvenv-activate (directory-file-name
                               deactivated-environment))
           (pyvenv-deactivate))
         (when venv-err
           (error venv-err))
         result))))

(defun elpy-rpc-get-or-create-virtualenv ()
  "Return Elpy's RPC virtualenv.

Create the virtualenv if it does not exist yet.
Update the virtualenv if the variable `elpy-rpc-python-command' has
changed since the virtualenv creation.

An additional file `elpy-rpc-python-path-command' is added in the
virtualenv directory in order to keep track of the python
binaries used to create the virtualenv."
  (let* ((rpc-venv-path (elpy-rpc-get-virtualenv-path))
         (is-default-rpc-venv (and rpc-venv-path
                                   (string-match "elpy-rpc-venv$"
                                                 rpc-venv-path)))
         (is-venv-exist (file-exists-p rpc-venv-path))
         (venv-python-path-command-file (concat
                                         (file-name-as-directory
                                          rpc-venv-path)
                                         "elpy-rpc-python-path-command"))
         (venv-python-path-command
          (when (file-exists-p venv-python-path-command-file)
            (with-temp-buffer
              (insert-file-contents venv-python-path-command-file)
              (buffer-string))))
         (venv-need-update (and is-venv-exist
                                is-default-rpc-venv
                                (not (string= venv-python-path-command
                                              elpy-rpc-python-command))))
         (venv-creation-allowed (and
                                 (not is-venv-exist)
                                 (y-or-n-p
                                  (if is-default-rpc-venv
                                      (format "Elpy needs a virtualenv to provide static code analysis, create one in '%s' ?" rpc-venv-path)
                                    (format
                                     "`elpy-rpc-virtualenv-path' was set to '%s', but this virtualenv does not exist, create it ?"
                                     rpc-venv-path))))))
    ;; Delete the rpc virtualenv if obsolete
    (when venv-need-update
      (delete-directory rpc-venv-path t)
      (setq is-venv-exist nil))
    ;; Create a new rpc venv if necessary
    (unless is-venv-exist
      (if (not venv-creation-allowed)
          (message "Please indicate the virtualenv you wish to use with `elpy-rpc-virtualenv-path'.")
        (let ((deact-venv pyvenv-virtual-env))
          ;; Create the venv
          (message "Elpy is creating the RPC virtualenv in '%s'" rpc-venv-path)
          ;; temporary workaround (waiting for  https://github.com/jorgenschaefer/pyvenv/pull/90 to be merged)
          ;; (save-window-excursion
          ;;   (pyvenv-create elpy-rpc-venv-name elpy-rpc-python-command))
          (cond
           ((= 0 (call-process elpy-rpc-python-command nil nil nil
                               "-m" "venv" "-h"))
            (with-current-buffer (generate-new-buffer "*venv*")
              (call-process elpy-rpc-python-command nil t t
                            "-m" "venv" rpc-venv-path)))
           ((executable-find "virtualenv")
            (with-current-buffer (generate-new-buffer "*virtualenv*")
              (call-process "virtualenv" nil t t
                            "-p" elpy-rpc-python-command rpc-venv-path)))
           (t
            (error "Elpy necessitates the 'virtualenv' python package, please install it with `pip install virtualenv`")))
          (pyvenv-activate rpc-venv-path)
          ;; Add a file to keep track of the `elpy-rpc-python-command` used
          (with-temp-file venv-python-path-command-file
            (insert elpy-rpc-python-command))
          ;; Install the dependencies
          ;;   safeguard to be sure we don't install stuff in the wrong venv
          (when (file-equal-p pyvenv-virtual-env rpc-venv-path)
            (if (y-or-n-p "Automatically install the RPC dependencies from PyPI (needed for completion, autoformatting and documentation) ? ")
                (with-temp-buffer
                  (message "Elpy is installing the RPC dependencies...")
                  (when (/= (apply 'call-process elpy-rpc-python-command
                                   nil t nil
                                   "-m" "pip" "install" "--upgrade"
                                   (elpy-rpc--get-package-list))
                            0)
                    (message "Elpy failed to install some of the RPC dependencies, please use `elpy-config' to install them.")))
              (message "Some of Elpy's functionnalities will not work, please use `elpy-config' to install the needed python dependencies.")))
          ;; Deactivate the rpc venv
          (if deact-venv
              (pyvenv-activate (directory-file-name deact-venv))
            (pyvenv-deactivate)))))
    rpc-venv-path))


;;;;;;;;;;;;;;;;;;;
;;; Promise objects

(defvar elpy-promise-marker (make-symbol "*elpy-promise*")
  "An uninterned symbol marking an Elpy promise object.")

(defun elpy-promise (success &optional error)
  "Return a new promise.

A promise is an object with a success and error callback. If the
promise is resolved using `elpy-promise-resolve', the SUCCESS
callback is called with the given value. The current buffer is
restored, too.

If the promise is rejected using `elpy-promise-reject', the ERROR
callback is called. For this function, the current buffer is not
necessarily restored, as it is also called when the buffer does
not exist anymore."
  (vector elpy-promise-marker ; 0 id
          success             ; 1 success-callback
          error               ; 2 error-callback
          (current-buffer)    ; 3 current-buffer
          nil                 ; 4 run
          ))

(defun elpy-promise-p (obj)
  "Return non-nil if OBJ is a promise object."
  (and (vectorp obj)
       (= (length obj) 5)
       (eq (aref obj 0) elpy-promise-marker)))

(defsubst elpy-promise-success-callback (promise)
  "Return the success callback for PROMISE."
  (aref promise 1))

(defsubst elpy-promise-error-callback (promise)
  "Return the error callback for PROMISE."
  (aref promise 2))

(defsubst elpy-promise-buffer (promise)
  "Return the buffer for PROMISE."
  (aref promise 3))

(defsubst elpy-promise-resolved-p (promise)
  "Return non-nil if the PROMISE has been resolved or rejected."
  (aref promise 4))

(defsubst elpy-promise-set-resolved (promise)
  "Mark PROMISE as having been resolved."
  (aset promise 4 t))

(defun elpy-promise-resolve (promise value)
  "Resolve PROMISE with VALUE."
  (when (not (elpy-promise-resolved-p promise))
    (unwind-protect
        (let ((success-callback (elpy-promise-success-callback promise)))
          (when success-callback
            (condition-case err
                (with-current-buffer (elpy-promise-buffer promise)
                  (funcall success-callback value))
              (error
               (elpy-promise-reject promise err)))))
      (elpy-promise-set-resolved promise))))

(defun elpy-promise-reject (promise reason)
  "Reject PROMISE because of REASON."
  (when (not (elpy-promise-resolved-p promise))
    (unwind-protect
        (let ((error-callback (elpy-promise-error-callback promise)))
          (when error-callback
            (if (buffer-live-p (elpy-promise-buffer promise))
                (with-current-buffer (elpy-promise-buffer promise)
                  (funcall error-callback reason))
              (with-temp-buffer
                (funcall error-callback reason)))))
      (elpy-promise-set-resolved promise))))

(defun elpy-promise-wait (promise &optional timeout)
  "Wait for PROMISE to be resolved, for up to TIMEOUT seconds.

This will accept process output while waiting.

This will wait for the current Elpy RPC process specifically, as
Emacs currently has a bug where it can wait for the entire time
of the timeout, even if output arrives.

See http://debbugs.gnu.org/cgi/bugreport.cgi?bug=17647"
  (let ((end-time (when timeout
                    (time-add (current-time)
                              (seconds-to-time timeout))))
        (process (get-buffer-process (elpy-rpc--get-rpc-buffer))))
    (while (and (not (elpy-promise-resolved-p promise))
                (or (not end-time)
                    (time-less-p (current-time)
                                 end-time)))
      (accept-process-output process timeout))))

;;;;;;;;;;;;;;;;;
;;; Elpy RPC

(defun elpy-rpc (method params &optional success error)
  "Call METHOD with PARAMS in the backend.

If SUCCESS and optionally ERROR is given, return immediately and
call those when a result is available. Otherwise, wait for a
result and return that."
  (when (not error)
    (setq error #'elpy-rpc--default-error-callback))
  (if success
      (elpy-rpc--call method params success error)
    (elpy-rpc--call-blocking method params)))

(defun elpy-rpc--call-blocking (method-name params)
  "Call METHOD-NAME with PARAMS in the current RPC backend.

Returns the result, blocking until this arrived."
  (let* ((result-arrived nil)
         (error-occured nil)
         (result-value nil)
         (error-object nil)
         (promise (elpy-rpc--call method-name params
                                  (lambda (result)
                                    (setq result-value result
                                          result-arrived t))
                                  (lambda (err)
                                    (setq error-object err
                                          error-occured t)))))
    (elpy-promise-wait promise elpy-rpc-timeout)
    (cond
     (error-occured
      (elpy-rpc--default-error-callback error-object))
     (result-arrived
      result-value)
     (t
      (error "Timeout during RPC call %s from backend"
             method-name)))))

(defun elpy-rpc--call (method-name params success error)
  "Call METHOD-NAME with PARAMS in the current RPC backend.

When a result is available, SUCCESS will be called with that
value as its sole argument. If an error occurs, ERROR will be
called with the error list.

Returns a PROMISE object."
  (let ((promise (elpy-promise success error)))
    (with-current-buffer (elpy-rpc--get-rpc-buffer)
      (setq elpy-rpc--call-id (1+ elpy-rpc--call-id)
            elpy-rpc--last-call (float-time))
      (elpy-rpc--register-callback elpy-rpc--call-id promise)
      (process-send-string
       (get-buffer-process (current-buffer))
       (let ((json-encoding-pretty-print nil))  ;; Link to bug https://github.com/jorgenschaefer/elpy/issues/1521
         (concat (json-encode `((id . ,elpy-rpc--call-id)
                                (method . ,method-name)
                                (params . ,params)))
                 "\n"))))
    promise))

(defun elpy-rpc--register-callback (call-id promise)
  "Register for PROMISE to be called when CALL-ID returns.

Must be called in an elpy-rpc buffer."
  (when (not elpy-rpc--buffer-p)
    (error "Must be called in RPC buffer"))
  (when (not elpy-rpc--backend-callbacks)
    (setq elpy-rpc--backend-callbacks (make-hash-table :test #'equal)))
  (puthash call-id promise elpy-rpc--backend-callbacks))

(defun elpy-rpc--get-rpc-buffer ()
  "Return the RPC buffer associated with the current buffer,
creating one if necessary."
  (when (not (elpy-rpc--process-buffer-p elpy-rpc--buffer))
    (setq elpy-rpc--buffer
          (or (elpy-rpc--find-buffer (elpy-library-root)
                                     elpy-rpc-python-command)
              (elpy-rpc--open (elpy-library-root)
                              elpy-rpc-python-command))))
  elpy-rpc--buffer)

(defun elpy-rpc--process-buffer-p (buffer)
  "Return non-nil when BUFFER is a live elpy-rpc process buffer.

If BUFFER is a buffer for an elpy-rpc process, but the process
died, this will kill the process and buffer."
  (cond
   ((or (not buffer)
        (not (buffer-live-p buffer)))
    nil)
   ((not (buffer-local-value 'elpy-rpc--buffer-p buffer))
    nil)
   ((and (get-buffer-process buffer)
         (process-live-p (get-buffer-process buffer)))
    t)
   (t
    (ignore-errors
      (kill-process (get-buffer-process buffer)))
    (ignore-errors
      (kill-buffer buffer))
    nil)))

(defun elpy-rpc--find-buffer (library-root python-command)
  "Return an existing RPC buffer for this project root and command."
  (catch 'return
    (let ((full-python-command (executable-find python-command)))
      (dolist (buf (buffer-list))
        (when (and (elpy-rpc--process-buffer-p buf)
                   (equal (buffer-local-value 'elpy-rpc--backend-library-root
                                              buf)
                          library-root)
                   (equal (buffer-local-value 'elpy-rpc--backend-python-command
                                              buf)
                          full-python-command))
          (throw 'return buf))))
    nil))

(defun elpy-rpc--open (library-root python-command)
  "Start a new RPC process and return the associated buffer."
  (elpy-rpc--cleanup-buffers)
  (with-elpy-rpc-virtualenv-activated
   (let* ((full-python-command (executable-find python-command))
          (name (format " *elpy-rpc [project:%s environment:%s]*"
                        library-root
                        (or deactivated-environment
                            "global")))
          (new-elpy-rpc-buffer (generate-new-buffer name))
          (proc nil))
     (when (not full-python-command)
       (error "Can't find Python command, configure `elpy-rpc-python-command'"))
     (with-current-buffer new-elpy-rpc-buffer
       (setq elpy-rpc--buffer-p t
             elpy-rpc--buffer (current-buffer)
             elpy-rpc--backend-library-root library-root
             elpy-rpc--backend-python-command full-python-command
             default-directory "/"
             proc (condition-case err
                      (let ((process-connection-type nil)
                            (process-environment (elpy-rpc--environment)))
                        (start-process name
                                       (current-buffer)
                                       full-python-command
                                       "-W" "ignore"
                                       "-m" "elpy.__main__"))
                    (error
                     (elpy-config-error
                      "Elpy can't start Python (%s: %s)"
                      (car err) (cadr err)))))
       (set-process-query-on-exit-flag proc nil)
       (set-process-sentinel proc #'elpy-rpc--sentinel)
       (set-process-filter proc #'elpy-rpc--filter)
       (elpy-rpc-init library-root deactivated-environment
                      (lambda (result)
                        (setq elpy-rpc--jedi-available
                              (cdr (assq 'jedi_available result))))))
     new-elpy-rpc-buffer)))

(defun elpy-rpc--cleanup-buffers ()
  "Close RPC buffers that have not been used in five minutes."
  (when elpy-rpc-maximum-buffer-age
    (let ((old (- (float-time)
                  elpy-rpc-maximum-buffer-age)))
      (dolist (buffer (buffer-list))
        (when (and (elpy-rpc--process-buffer-p buffer)
                   (< (or (buffer-local-value 'elpy-rpc--last-call buffer)
                          old)
                      old))
          (ignore-errors
            (kill-process (get-buffer-process buffer)))
          (ignore-errors
            (kill-buffer buffer)))))))

(defun elpy-rpc--sentinel (process event)
  "The sentinel for the RPC process.

As process sentinels are only ever called when the process
terminates, this will call the error handler of all registered
RPC calls with the event."
  (let ((buffer (process-buffer process))
        (err (list 'process-sentinel (substring event 0 -1))))
    (when (and buffer
               (buffer-live-p buffer))
      (with-current-buffer buffer
        (when elpy-rpc--backend-callbacks
          (maphash (lambda (_call-id promise)
                     (ignore-errors
                       (elpy-promise-reject promise err)))
                   elpy-rpc--backend-callbacks)
          (setq elpy-rpc--backend-callbacks nil))))))

(defun elpy-rpc--filter (process output)
  "The filter for the RPC process."
  (let ((buffer (process-buffer process)))
    (when (and buffer
               (buffer-live-p buffer))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert output)
        (while (progn
                 (goto-char (point-min))
                 (search-forward "\n" nil t))
          (let ((line-end (point))
                (json nil)
                (did-read-json nil))
            (goto-char (point-min))
            (condition-case _err
                (progn
                  (setq json (let ((json-array-type 'list)
                                   (json-false nil)
                                   (json-encoding-pretty-print nil))  ;; Link to bug https://github.com/jorgenschaefer/elpy/issues/1521
                               (json-read)))
                  (if (listp json)
                      (setq  line-end (1+ (point))
                             did-read-json t)
                    (goto-char (point-min))))
              (error
               (goto-char (point-min))))
            (cond
             (did-read-json
              (delete-region (point-min) line-end)
              (elpy-rpc--handle-json json))
             ((looking-at "elpy-rpc ready\n")
              (replace-match "")
              (elpy-rpc--check-backend-version "1.1"))
             ((looking-at "elpy-rpc ready (\\([^ ]*\\))\n")
              (let ((rpc-version (match-string 1)))
                (replace-match "")
                (elpy-rpc--check-backend-version rpc-version)))
             ((looking-at ".*No module named elpy\n")
              (replace-match "")
              (elpy-config-error "Elpy module not found"))
             (t
              (let ((line (buffer-substring (point-min)
                                            line-end)))
                (delete-region (point-min) line-end)
                (elpy-rpc--handle-unexpected-line line))))))))))

(defun elpy-rpc--check-backend-version (rpc-version)
  "Check that we are using the right version."
  (when (not (equal rpc-version elpy-version))
    (elpy-insert--popup "*Elpy Version Mismatch*"
      (elpy-insert--header "Elpy Version Mismatch")
      (elpy-insert--para
       "You are not using the same version of Elpy in Emacs Lisp "
       "compared to Python. This can cause random problems. Please "
       "do make sure to use compatible versions.\n\n"
       "This often happens because you have an obsolete elpy python "
       "package installed on your system/virtualenv. This package "
       "shadows the elpy python package shipped with elpy, leading "
       "to this mismatch. If it is the case, uninstalling the elpy "
       "python package (with pip for example) should resolve the issue.\n")
      (insert
       "\n"
       "Elpy Emacs Lisp version: " elpy-version "\n"
       "Elpy Python version....: " rpc-version "\n"))))

(defun elpy-rpc--handle-unexpected-line (line)
  "Handle an unexpected line from the backend.

This is usually an error or backtrace."
  (let ((buf (get-buffer "*Elpy Output*")))
    (when (not buf)
      (elpy-insert--popup "*Elpy Output*"
        (elpy-insert--header "Output from Backend")
        (elpy-insert--para
         "There was some unexpected output from the Elpy backend. "
         "This is usually not a problem and should usually not be "
         "reported as a bug with Elpy. You can safely hide this "
         "buffer and ignore it. You can also see the output below "
         "in case there is an actual problem.\n\n")
        (elpy-insert--header "Output")
        (setq buf (current-buffer))))
    (with-current-buffer buf
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert line)))))

(defun elpy-rpc--handle-json (json)
  "Handle a single JSON object from the RPC backend."
  (let ((call-id (cdr (assq 'id json)))
        (error-object (cdr (assq 'error json)))
        (result (cdr (assq 'result json))))
    (let ((promise (gethash call-id elpy-rpc--backend-callbacks)))
      (when (not promise)
        (error "Received a response for unknown call-id %s" call-id))
      (remhash call-id elpy-rpc--backend-callbacks)
      (if error-object
          (elpy-promise-reject promise error-object)
        (elpy-promise-resolve promise result)))))

(defun elpy-rpc--default-error-callback (error-object)
  "Display an error from the RPC backend."
  ;; We actually might get an (error "foo") thing here.
  (if (and (consp error-object)
           (not (consp (car error-object))))
      (signal (car error-object) (cdr error-object))
    (let ((message (cdr (assq 'message error-object)))
          (code (cdr (assq 'code error-object)))
          (data (cdr (assq 'data error-object))))
      (cond
       ((not (numberp code))
        (error "Bad response from RPC: %S" error-object))
       ((< code 300)
        (message "Elpy warning: %s" message))
       ((< code 500)
        (error "Elpy error: %s" message))
       ((and elpy-rpc-error-timeout
             elpy-rpc--last-error-popup
             (<= (float-time)
                 (+ elpy-rpc--last-error-popup
                    elpy-rpc-error-timeout)))
        (message "Elpy error popup ignored, see `elpy-rpc-error-timeout': %s"
                 message))
       ((not elpy-disable-backend-error-display)
        (let ((config (elpy-config--get-config)))
          (elpy-insert--popup "*Elpy Error*"
            (elpy-insert--header "Elpy Error")
            (elpy-insert--para
             "The backend encountered an unexpected error. This indicates "
             "a bug in Elpy. Please open a bug report with the data below "
             "in the Elpy bug tracker:")
            (insert "\n"
                    "\n")
            (insert-button
             "https://github.com/jorgenschaefer/elpy/issues/new"
             'action (lambda (button)
                       (browse-url (button-get button 'url)))
             'url "https://github.com/jorgenschaefer/elpy/issues/new")
            (insert "\n"
                    "\n"
                    "```\n")
            (elpy-insert--header "Error Message")
            (insert message "\n\n")
            (elpy-insert--header "Configuration")
            (elpy-config--insert-configuration-table config)
            (let ((traceback (cdr (assq 'traceback data))))
              (when traceback
                (insert "\n")
                (elpy-insert--header "Traceback")
                (insert traceback)))
            (let ((jedi-info (cdr (assq 'jedi_debug_info data))))
              (when jedi-info
                (insert "\n")
                (elpy-insert--header "Jedi Debug Information")
                (pcase (cdr (assq 'debug_info jedi-info))
                  (`nil (insert "Jedi did not emit any debug info.\n"))
                  (infos
                   (dolist (outstr infos)
                     (insert outstr "\n"))))
                (insert "\n"
                        "```\n"
                        "\n"
                        "Reproduction:\n"
                        "\n")
                (let ((method (cdr (assq 'method jedi-info)))
                      (source (cdr (assq 'source jedi-info)))
                      (script-args (cdr (assq 'script_args jedi-info))))
                  (insert "```Python\n")
                  (insert "import jedi\n"
                          "\n"
                          "source = '''\\\n"
                          source
                          "'''\n"
                          "\n"
                          "script = jedi.Script(" script-args ")\n"
                          "script." method "()\n"))))
            (let ((rope-info (cdr (assq 'rope_debug_info data))))
              (when rope-info
                (insert "\n")
                (elpy-insert--header "Rope Debug Information")
                (insert "```\n"
                        "\n"
                        "Reproduction:\n"
                        "\n")
                (let ((project-root (cdr (assq 'project_root rope-info)))
                      (filename (cdr (assq 'filename rope-info)))
                      (source (cdr (assq 'source rope-info)))
                      (function-name (cdr (assq 'function_name rope-info)))
                      (function-args (cdr (assq 'function_args rope-info))))
                  (insert "```Python\n")
                  (insert "\n"
                          "source = '''\n"
                          source
                          "'''\n"
                          "\n")
                  (insert "project = rope.base.project.Project(\n"
                          (format "    %S,\n" project-root)
                          "    ropefolder=None\n"
                          ")\n")
                  (insert "resource = rope.base.libutils.path_to_resource(\n"
                          "    project,\n"
                          (format "    %S,\n" filename)
                          "    'file'\n"
                          ")\n")
                  (insert (format "%s(\n    %s\n)\n"
                                  function-name function-args)))))
            (when (not (= 0 (current-column)))
              (insert "\n"))
            (insert "```"))
          (setq elpy-rpc--last-error-popup (float-time))))))))

(defun elpy-rpc--environment ()
  "Return a `process-environment' for the RPC process.

This includes `elpy-rpc-pythonpath' in the PYTHONPATH, if set."
  (if (or (not elpy-rpc-pythonpath)
          (not (file-exists-p (expand-file-name "elpy/__init__.py"
                                                elpy-rpc-pythonpath))))
      process-environment
    (let* ((old-pythonpath (getenv "PYTHONPATH"))
           (new-pythonpath (if old-pythonpath
                               (concat elpy-rpc-pythonpath
                                       path-separator
                                       old-pythonpath)
                             elpy-rpc-pythonpath)))
      (cons (concat "PYTHONPATH=" new-pythonpath)
            process-environment))))

(defun elpy-rpc--buffer-contents ()
  "Return the contents of the current buffer.

This returns either a string, or a file object for the RPC
protocol if the buffer is larger than
`elpy-rpc-large-buffer-size'."
  (if (< (buffer-size) elpy-rpc-large-buffer-size)
      (buffer-string)
    (let ((file-name (make-temp-file "elpy-rpc-"))
          (coding-system-for-write 'utf-8))
      (write-region nil nil file-name nil :nomessage)
      `((filename . ,file-name)
        (delete_after_use . t)))))

(defun elpy-rpc--region-contents ()
  "Return the selected region as a string."
  (if (use-region-p)
      (buffer-substring (region-beginning) (region-end))))

(defun elpy-rpc--disconnect ()
  "Disconnect rpc process from elpy buffers."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when elpy-mode
        (setq elpy-rpc--buffer nil)))))

;; RPC API functions

(defun elpy-rpc-restart ()
  "Restart all RPC processes."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (elpy-rpc--process-buffer-p buffer)
      (ignore-errors
        (kill-process (get-buffer-process buffer)))
      (ignore-errors
        (kill-buffer buffer)))))

(defun elpy-rpc-init (library-root environment &optional success error)
  "Initialize the backend.

This has to be called as the first method, else Elpy won't be
able to respond to other calls."
  (elpy-rpc "init"
            ;; This uses a vector because otherwise, json-encode in
            ;; older Emacsen gets seriously confused, especially when
            ;; backend is nil.
            (vector `((project_root . ,(expand-file-name library-root))
                      (environment . ,(when environment (expand-file-name environment)))))
            success error))


;;;;;;;;;;;;;;
;;; RPC API

(defun elpy-rpc-get-calltip (&optional success error)
  "Call the get_calltip API function.

Returns a calltip string for the function call at point."
  (when (< (buffer-size) elpy-rpc-ignored-buffer-size)
    (elpy-rpc "get_calltip"
              (list buffer-file-name
                    (elpy-rpc--buffer-contents)
                    (- (point)
                       (point-min)))
              success error)))


(defun elpy-rpc-get-oneline-docstring (&optional success error)
  "Call the get_oneline_docstring API function.

Returns a oneline docstring string for the symbol at point."
  (when (< (buffer-size) elpy-rpc-ignored-buffer-size)
    (elpy-rpc "get_oneline_docstring"
              (list buffer-file-name
                    (elpy-rpc--buffer-contents)
                    (- (point)
                       (point-min)))
              success error)))

(defun elpy-rpc-get-completions (&optional success error)
  "Call the get_completions API function.

Returns a list of possible completions for the Python symbol at
point."
  (when (and (< (buffer-size) elpy-rpc-ignored-buffer-size)
             (not (string-match "^[0-9]+$" (symbol-name (symbol-at-point)))))
    (elpy-rpc "get_completions"
              (list buffer-file-name
                    (elpy-rpc--buffer-contents)
                    (- (point)
                       (point-min)))
              success error)))

(defun elpy-rpc-get-completion-docstring (completion &optional success error)
  "Call the get_completion_docstring API function.

Returns a doc string or nil"
  (elpy-rpc "get_completion_docstring" (list completion) success error))

(defun elpy-rpc-get-completion-location (completion &optional success error)
  "Call the get_completion_location API function.

Returns a list of file name and line number, or nil"
  (elpy-rpc "get_completion_location" (list completion) success error))

(defun elpy-rpc-get-definition (&optional success error)
  "Call the find_definition API function.

Returns nil or a list of (filename, point)."
  (elpy-rpc "get_definition"
            (list buffer-file-name
                  (elpy-rpc--buffer-contents)
                  (- (point)
                     (point-min)))
            success error))

(defun elpy-rpc-get-assignment (&optional success error)
  "Call the find_assignment API function.

Returns nil or a list of (filename, point)."
  (elpy-rpc "get_assignment"
            (list buffer-file-name
                  (elpy-rpc--buffer-contents)
                  (- (point)
                     (point-min)))
            success error))

(defun elpy-rpc-get-docstring (&optional success error)
  "Call the get_docstring API function.

Returns a possible multi-line docstring for the symbol at point."
  (elpy-rpc "get_docstring"
            (list buffer-file-name
                  (elpy-rpc--buffer-contents)
                  (- (point)
                     (point-min)))
            success error))

(defun elpy-rpc-get-pydoc-completions (prefix &optional success error)
  "Return a list of modules available in pydoc starting with PREFIX."
  (elpy-rpc "get_pydoc_completions" (list prefix)
            success error))

(defun elpy-rpc-get-pydoc-documentation (symbol &optional success error)
  "Get the Pydoc documentation for SYMBOL.

Returns a possible multi-line docstring."
  (elpy-rpc "get_pydoc_documentation" (list symbol)
            success error))

(defun elpy-rpc-get-usages (&optional success error)
  "Return the symbol under point usages as a list."
  (elpy-rpc "get_usages"
            (list buffer-file-name
                  (elpy-rpc--buffer-contents)
                  (- (point)
                     (point-min)))
            success error))

(defun elpy-rpc-get-names (&optional success error)
  "Return all names (possible candidates for jumping to definition)."
  (elpy-rpc "get_names"
            (list buffer-file-name
                  (elpy-rpc--buffer-contents)
                  (- (point)
                     (point-min)))
            success error))


(provide 'elpy-rpc)
;;; elpy-rpc.el ends here
