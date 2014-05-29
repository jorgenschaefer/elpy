;;; elpy.el --- Emacs Lisp Python Environment -*- lexical-binding: t -*-

;; Copyright (C) 2012-2014  Jorgen Schaefer

;; Author: Jorgen Schaefer <contact@jorgenschaefer.de>
;; URL: https://github.com/jorgenschaefer/elpy
;; Version: 1.4.50

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

;; The Emacs Lisp Python Environment in Emacs

;; Elpy is an Emacs package to bring powerful Python editing to Emacs.
;; It combines a number of existing Emacs packages, and uses one of a
;; selection of Python packages for code introspection.

;; To use, you need to install not only this package, but a few Python
;; packages as well. See the installation instructions on the wiki.

;; Documentation is available there as well.

;; https://github.com/jorgenschaefer/elpy/wiki

;;; Writing Elpy Modules:

;; A module is a function which is called with one or more arguments.
;; This first argument is the command specifier symbol, which can be
;; one of the following:

;; global-init:
;; - Called once, when Elpy is enabled using `elpy-enable'.

;; global-stop:
;; - Called once, when Elpy is disabled using `elpy-disable'.

;; buffer-init:
;; - Called in a buffer when elpy-mode is enabled.

;; buffer-stop:
;; - Called in a buffer when elpy-mode is disabled.

;;; Writing test runners:

;; A test runner is a function that receives four arguments, described
;; in the docstring of `elpy-test-at-point'. If only the first
;; argument is given, the test runner should find tests under this
;; directory and run them. If the others are given, the test runner
;; should run the specified test only, or as few as it can.

;; Test runners should use an interactive spec of (interactive
;; (elpy-test-at-point)) so they can be called directly by the user.
;; For their main work, they can simply call `elpy-test-run'. See the
;; `elpy-test-discover-runner' for an example.

;;; Code:

(require 'cus-edit)
(require 'elpy-refactor)
(require 'etags)
(require 'idomenu)
(require 'json)
(require 'python)
(require 'grep)
(require 'thingatpt)
(require 'pyvenv)

;;;;;;;;;;;;;;;
;;; Elpy itself

(defgroup elpy nil
  "The Emacs Lisp Python Environment."
  :prefix "elpy-"
  :group 'languages)

(defcustom elpy-modules '(elpy-module-sane-defaults
                          elpy-module-company
                          elpy-module-eldoc
                          elpy-module-find-file-in-project
                          elpy-module-flymake
                          elpy-module-highlight-indentation
                          elpy-module-pyvenv
                          elpy-module-yasnippet)
  "Which Elpy modules to use.

Elpy can use a number of modules for additional features, which
can be inidividually enabled or disabled."
  :type '(set (const :tag "Inline code completion (company-mode)"
                     elpy-module-company)
              (const :tag "Show function signatures (ElDoc)"
                     elpy-module-eldoc)
              (const :tag "Highlight syntax errors (Flymake)"
                     elpy-module-flymake)
              (const :tag "Show the virtualenv in the mode line (pyvenv)"
                     elpy-module-pyvenv)
              (const :tag "Display indentation markers (highlight-indentation)"
                     elpy-module-highlight-indentation)
              (const :tag "Expand code snippets (YASnippet)"
                     elpy-module-yasnippet)
              (const :tag "Find files in a project (ffip)"
                     elpy-module-find-file-in-project)
              (const :tag "Configure some sane defaults for Emacs"
                     elpy-module-sane-defaults))
  :group 'elpy)

(defcustom elpy-project-ignored-directories '(".tox" "build" "dist" ".git"
                                              ".svn" "CVS" ".bzr" ".hg")
  "Directories ignored by functions working on the whole project."
  :type '(repeat string)
  :group 'elpy)

(defcustom elpy-rpc-backend nil
  "Your preferred backend.

Elpy can use different backends for code introspection. These
need to be installed separately using pip or other mechanisms to
make them available to Python. If you prefer not to do this, you
can use the native backend, which is very limited but does not
have any external requirements."
  :type '(choice (const :tag "Rope" "rope")
                 (const :tag "Jedi" "jedi")
                 (const :tag "Native" "native")
                 (const :tag "Automatic" nil))
  :group 'elpy)

(defcustom elpy-rpc-large-buffer-size 4096
  "Size for a source buffer up to which it will be sent directly.

The Elpy RPC protocol uses JSON as the serialization format.
Large buffers take a long time to encode, so Elpy can transmit
them via temporary files. If a buffer is larger than this value,
it is sent via a temporary file."
  :type 'integer
  :group 'elpy)

(defcustom elpy-rpc-python-command (if (eq window-system 'w32)
                                       "pythonw"
                                     "python")
  "The Python interpreter for the RPC backend.

This should be the same interpreter the project will be run with,
and not an interactive shell like ipython."
  :type '(choice (const :tag "python" "python")
                 (const :tag "python2" "python2")
                 (const :tag "python3" "python3")
                 (const :tag "pythonw (Python on Windows)" "pythonw")
                 (string :tag "Other"))
  :group 'elpy)

(defcustom elpy-rpc-pythonpath (file-name-directory (locate-library "elpy"))
  "A directory to add to the PYTHONPATH for the RPC process.

This should be a directory where the elpy module can be found. If
this is nil, it's assumed elpy can be found in the standard path.
Usually, there is no need to change this."
  :type 'directory
  :group 'elpy)

(defcustom elpy-test-runner 'elpy-test-discover-runner
  "The test runner to use to run tests."
  :type '(choice (const :tag "Unittest Discover" elpy-test-discover-runner)
                 (const :tag "Django Discover" elpy-test-django-runner)
                 (const :tag "Nose" elpy-test-nose-runner)
                 (const :tag "py.test" elpy-test-pytest-runner))
  :group 'elpy)

(defconst elpy-version "1.4.50"
  "The version of the Elpy lisp code.")

(defvar elpy-mode-hook nil
  "Hook run when `elpy-mode' is enabled.")

(defvar elpy-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Alphabetical order to make it easier to find free C-c C-X
    ;; bindings in the future. Heh.

    ;; (define-key map (kbd "<backspace>") 'python-indent-dedent-line-backspace)
    ;; (define-key map (kbd "<backtab>")   'python-indent-dedent-line)

    ;; (define-key map (kbd "C-M-x")   'python-shell-send-defun)
    ;; (define-key map (kbd "C-c <")   'python-indent-shift-left)
    ;; (define-key map (kbd "C-c >")   'python-indent-shift-right)
    (define-key map (kbd "C-c C-c") 'elpy-shell-send-region-or-buffer)
    (define-key map (kbd "C-c C-z") 'elpy-shell-switch-to-shell)
    (define-key map (kbd "C-c C-d") 'elpy-doc)
    (define-key map (kbd "C-c C-f") 'find-file-in-project)
    (define-key map (kbd "C-c C-j") 'idomenu)
    (define-key map (kbd "C-c C-n") 'elpy-flymake-forward-error)
    (define-key map (kbd "C-c C-o") 'elpy-occur-definitions)
    (define-key map (kbd "C-c C-p") 'elpy-flymake-backward-error)
    (define-key map (kbd "C-c C-r") 'elpy-refactor)
    (define-key map (kbd "C-c C-s") 'elpy-rgrep-symbol)
    (define-key map (kbd "C-c C-t") 'elpy-test)
    (define-key map (kbd "C-c C-v") 'elpy-check)
    ;; (define-key map (kbd "C-c C-z") 'python-shell-switch-to-shell)

    (define-key map (kbd "<S-return>") 'elpy-open-and-indent-line-below)
    (define-key map (kbd "<C-S-return>") 'elpy-open-and-indent-line-above)

    (define-key map (kbd "<C-down>") 'elpy-nav-forward-definition)
    (define-key map (kbd "<C-up>")  'elpy-nav-backward-definition)

    ;; (define-key map (kbd "M-,")     'iedit-mode
    (define-key map (kbd "M-.")     'elpy-goto-definition)
    (define-key map (kbd "M-n")     'elpy-nav-forward-definition)
    (define-key map (kbd "M-p")     'elpy-nav-backward-definition)
    (define-key map (kbd "M-TAB")   'elpy-company-backend)

    map)
  "Key map for the Emacs Lisp Python Environment.")

(defvar elpy-modules-initialized-p nil
  "Boolean, set to true if modules were run with `global-init'.")

;;;###autoload
(defun elpy-enable (&optional ignored)
  "Enable Elpy in all future Python buffers."
  (interactive)
  (when (< emacs-major-version 24)
    (error "Elpy requires Emacs 24 or newer"))
  (when ignored
    (warn "The argument to `elpy-enable' is deprecated, customize `elpy-modules' instead"))
  (let ((filename (find-lisp-object-file-name 'python-mode
                                              'symbol-function)))
    (when (and filename
               (string-match "/python-mode\\.el\\'"
                             filename))
      (error (concat "You are using python-mode.el. "
                     "Elpy only works with python.el from "
                     "Emacs 24 and above"))))
  (when (not elpy-modules-initialized-p)
    (elpy-modules-run 'global-init)
    (setq elpy-modules-initialized-p t))
  (add-hook 'python-mode-hook 'elpy-mode))

(defun elpy-disable ()
  "Disable Elpy in all future Python buffers."
  (interactive)
  (remove-hook 'python-mode-hook 'elpy-mode)
  (when elpy-modules-initialized-p
    (elpy-modules-run 'global-stop)
    (setq elpy-modules-initialized-p nil)))

;;;###autoload
(define-minor-mode elpy-mode
  "Minor mode in Python buffers for the Emacs Lisp Python Environment.

This mode fully supports virtualenvs. Once you switch a
virtualenv using \\[pyvenv-workon], you can use
\\[elpy-rpc-restart] to make the elpy Python process use your
virtualenv.

See https://github.com/jorgenschaefer/elpy/wiki/Keybindings for a
more structured list.

\\{elpy-mode-map}"
  :lighter " Elpy"
  (when (not (eq major-mode 'python-mode))
    (error "Elpy only works with `python-mode'"))
  (cond
   (elpy-mode
    (when (not elpy-modules-initialized-p)
      (elpy-modules-run 'global-init)
      (setq elpy-modules-initialized-p t))
    (elpy-modules-run 'buffer-init))
   ((not elpy-mode)
    (elpy-modules-run 'buffer-stop))))

;;;;;;;;;;;;;;;;;;;;
;;; Helper Functions

(defun elpy-symbol-at-point ()
  "Return the Python symbol at point, including dotted paths."
  (with-syntax-table python-dotty-syntax-table
    (let ((symbol (symbol-at-point)))
      (if symbol
          (symbol-name symbol)
        nil))))

;;;;;;;;;;;;;;;;;;;;;;
;;; Elpy Config Buffer

(defun elpy-config-error (&optional fmt &rest args)
  "Note a configuration problem.

This will show a message in the minibuffer that tells the user to
use \\[elpy-config]."
  (let ((msg (if fmt
                 (apply #'format fmt args)
               "Elpy is not properly configured")))
    (error "%s; use M-x elpy-config to configure it" msg)))

;;;###autoload
(defun elpy-config ()
  "Configure Elpy.

This function will pop up a configuration buffer, which is mostly
a customize buffer, but has some more options."
  (interactive)
  (let ((buf (custom-get-fresh-buffer "*Elpy Config*"))
        (config (elpy-config--get-config))
        (custom-search-field nil))
    (with-current-buffer buf
      (elpy-insert--header "Elpy Configuration")

      (elpy-config--insert-configuration-problems config)

      (elpy-insert--header "Options")

      (let ((custom-buffer-style 'tree))
        (Custom-mode)
        (elpy-config--insert-help)
        (widget-create 'custom-group
                       :custom-last t
                       :custom-state 'hidden
                       :tag "Elpy"
                       :value 'elpy)
        (widget-create 'custom-group
                       :custom-last t
                       :custom-state 'hidden
                       :tag "Python"
                       :value 'python)
        (widget-create 'custom-group
                       :custom-last t
                       :custom-state 'hidden
                       :tag "Completion (Company)"
                       :value 'company)
        (widget-create 'custom-group
                       :custom-last t
                       :custom-state 'hidden
                       :tag "Call Signatures (ElDoc)"
                       :value 'eldoc)
        (widget-create 'custom-group
                       :custom-last t
                       :custom-state 'hidden
                       :tag "Inline Errors (Flymake)"
                       :value 'flymake)
        (widget-create 'custom-group
                       :custom-last t
                       :custom-state 'hidden
                       :tag "Snippets (YASnippet)"
                       :value 'yasnippet)
        (widget-setup)
        (goto-char (point-min))))
    (pop-to-buffer-same-window buf)))

(defun elpy-config--insert-help ()
  (let ((start (point)))
    ;; Help display from `customize-browse'
    (widget-insert (format "\
%s buttons; type RET or click mouse-1
on a button to invoke its action.
Invoke [+] to expand a group, and [-] to collapse an expanded group.\n"
                           (if custom-raised-buttons
                               "`Raised' text indicates"
                             "Square brackets indicate")))
    (if custom-browse-only-groups
        (widget-insert "\
Invoke the [Group] button below to edit that item in another window.\n\n")
      (widget-insert "Invoke the ")
      (widget-create 'item
                     :format "%t"
                     :tag "[Group]"
                     :tag-glyph "folder")
      (widget-insert ", ")
      (widget-create 'item
                     :format "%t"
                     :tag "[Face]"
                     :tag-glyph "face")
      (widget-insert ", and ")
      (widget-create 'item
                     :format "%t"
                     :tag "[Option]"
                     :tag-glyph "option")
      (widget-insert " buttons below to edit that
item in another window.\n\n")

      (fill-region start (point)))))

(defun elpy-config--insert-configuration-problems (&optional config)
  "Insert help text and widgets for configuration problems."
  (when (not config)
    (setq config (elpy-config--get-config)))
  (elpy-config--insert-configuration-table config)
  (insert "\n")

  ;; Python not found
  (when (not (gethash "python_rpc_executable" config))
    (elpy-insert--para
     "Elpy can not find the configured Python interpreter. Please make "
     "sure that the variable `elpy-rpc-python-command' points to a "
     "command in your PATH. You can change the variable below.\n"))

  ;; Python found, but can't find the elpy module
  (when (and (gethash "python_rpc_executable" config)
             (not (gethash "elpy_version" config)))
    (elpy-insert--para
     "The Python interpreter could not find the elpy module. "
     "Make sure the module is installed"
     (if (getenv "virtual_env" config)
         " in the current virtualenv.\n"
       ".\n"))
    (insert "\n")
    (widget-create 'elpy-insert--pip-button "elpy")
    (insert "\n\n"))

  ;; Otherwise unparseable output.
  (when (gethash "error_output" config)
    (elpy-insert--para
     "There was an unexpected problem starting the RPC process. Please "
     "check the following output to see if this makes sense to you. "
     "To me, it doesn't.\n")
    (insert "\n"
            (gethash "error_output" config) "\n"
            "\n"))

  ;; Requested backend unavailable
  (when (and (gethash "python_rpc_executable" config)
             (or (and (equal elpy-rpc-backend "rope")
                      (not (gethash "rope_version" config)))
                 (and (equal elpy-rpc-backend "jedi")
                      (not (gethash "jedi_version" config)))))
    (elpy-insert--para
     "You requested Elpy to use the backend " elpy-rpc-backend ", "
     "but the Python interpreter could not load that module. Make "
     "sure the module is installed, or change the value of "
     "`elpy-rpc-backend' below to one of the available backends.\n")
    (insert "\n")
    (widget-create 'elpy-insert--pip-button elpy-rpc-backend)
    (insert "\n\n"))

  ;; Only native backend available, but requested automatic choice
  (when (and (gethash "python_rpc_executable" config)
             (and (not elpy-rpc-backend)
                  (not (gethash "rope_version" config))
                  (not (gethash "jedi_version" config))))
    (elpy-insert--para
     "You did not specify a preference for an RPC backend, but there "
     "is only the native backend available. The native backend has "
     "seriously limited capabilities. If you really want to use the "
     "native backend, please change the value of `elpy-rpc-backend' "
     "below to make this explicit. Alternatively, you can install the "
     "module for one of the supported backends.\n")
    (insert "\n")
    (widget-create 'elpy-insert--pip-button "rope")
    (insert "\n")
    (widget-create 'elpy-insert--pip-button "jedi")
    (insert "\n\n"))

  ;; Bad backend version
  (when (and (gethash "elpy_version" config)
             (not (equal (gethash "elpy_version" config)
                         elpy-version)))
    (let ((elpy-python-version (gethash "elpy_version" config)))
      (elpy-insert--para
       "The Elpy backend is version " elpy-python-version " while "
       "the Emacs package is " elpy-version ". This is incompatible. "
       (if (version< elpy-python-version elpy-version)
           "Please upgrade the Python module."
         "Please upgrade the Emacs Lisp package.")
       "\n")))

  (when (not (executable-find "flake8"))
    (elpy-insert--para
     "The configured syntax checker could not be found. Elpy uses this "
     "program to provide syntax checks of your programs, so you might "
     "want to install one. Elpy by default uses flake8.\n")
    (insert "\n")
    (widget-create 'elpy-insert--pip-button "flake8")
    (insert "\n\n"))

  )

(defvar elpy-config--get-config "import json
import sys
config = {}
config['python_version'] = ('{major}.{minor}.{micro}'
                            .format(major=sys.version_info[0],
                                    minor=sys.version_info[1],
                                    micro=sys.version_info[2]))

try:
    import elpy
    config['elpy_version'] = elpy.__version__
except:
    config['elpy_version'] = None

try:
    import jedi
    if isinstance(jedi.__version__, tuple):
        config['jedi_version'] = '.'.join(str(x) for x in jedi.__version__)
    else:
        config['jedi_version'] = jedi.__version__
except:
    config['jedi_version'] = None

try:
    import rope
    config['rope_version'] = rope.VERSION
except:
    config['rope_version'] = None


json.dump(config, sys.stdout)
")

(defun elpy-config--get-config ()
  "Return the configuration from `elpy-rpc-python-command'.

This returns a hash table with the following keys (all strings):

emacs_version
python_rpc
python_rpc_executable
python_interactive
python_interactive_executable
python_version (RPC)
elpy_version
jedi_version
rope_version
virtual_env
virtual_env_short"
  (with-temp-buffer
    (let ((config (make-hash-table :test #'equal)))
      (puthash "emacs_version" emacs-version config)
      (puthash "python_rpc" elpy-rpc-python-command config)
      (puthash "python_rpc_executable"
               (executable-find elpy-rpc-python-command)
               config)
      (let ((interactive-python (if (boundp 'python-python-command)
                                    python-python-command
                                  python-shell-interpreter)))
        (puthash "python_interactive"
                 interactive-python
                 config)
        (puthash "python_interactive_executable"
                 (executable-find interactive-python)
                 config))
      (let ((venv (getenv "VIRTUAL_ENV")))
        (when venv
          (puthash "virtual_env" venv config)
          (puthash "virtual_env_short" (file-name-nondirectory venv) config)))
      (let ((return-value (ignore-errors
                            (let ((process-environment
                                   (elpy-rpc--environment)))
                              (call-process elpy-rpc-python-command
                                            nil
                                            (current-buffer)
                                            nil
                                            "-c"
                                            elpy-config--get-config)))))
        (when return-value
          (let ((data (ignore-errors
                        (let ((json-array-type 'list))
                          (goto-char (point-min))
                          (json-read)))))
            (if (not data)
                (puthash "error_output" (buffer-string) config)
              (dolist (pair data)
                (puthash (symbol-name (car pair)) (cdr pair) config))))))
      config)))

(defun elpy-config--insert-configuration-table (&optional config)
  "Insert a table describing the current Elpy config."
  (when (not config)
    (setq config (elpy-config--get-config)))
  (let ((emacs-version (gethash "emacs_version" config))
        (python-version (gethash "python_version" config))
        (python-rpc (gethash "python_rpc" config))
        (python-rpc-executable (gethash "python_rpc_executable" config))
        (python-interactive (gethash "python_interactive" config))
        (python-interactive-executable (gethash "python_interactive_executable"
                                                config))
        (elpy-python-version (gethash "elpy_version" config))
        (jedi-version (gethash "jedi_version" config))
        (rope-version (gethash "rope_version" config))
        (virtual-env (gethash "virtual_env" config))
        (virtual-env-short (gethash "virtual_env_short" config))
        table maxwidth)
    (setq table
          `(("Virtualenv" . ,(if (gethash "virtual_env" config)
                                 (format "%s (%s)"
                                         virtual-env-short
                                         virtual-env)
                               "None"))
            ("RPC Python" . ,(cond
                              (python-version
                               (format "%s (%s)"
                                       python-version
                                       python-rpc-executable))
                              (python-rpc-executable
                               python-rpc-executable)
                              (python-rpc
                               (format "%s (not found)" python-rpc))
                              (t
                               (format "Not configured"))))
            ("Interactive Python" . ,(cond
                                      (python-interactive-executable
                                       (format "%s (%s)"
                                               python-interactive
                                               python-interactive-executable))
                                      (python-interactive
                                       (format "%s (not found)"
                                               python-interactive))
                                      (t
                                       "Not configured")))
            ("Emacs" . ,emacs-version)
            ("Elpy" . ,(cond
                        ((and elpy-python-version elpy-version
                              (equal elpy-python-version elpy-version))
                         elpy-version)
                        (elpy-python-version
                         (format "%s (Python), %s (Emacs Lisp)"
                                 elpy-python-version
                                 elpy-version))
                        (t
                         (format "Not found (Python), %s (Emacs Lisp)"
                                 elpy-version))))
            ("Jedi" . ,(if jedi-version
                           jedi-version
                         "Not found"))
            ("Rope" . ,(if rope-version
                           rope-version
                         "Not found"))
            ("Syntax checker" . ,(let ((syntax-checker
                                        (executable-find
                                         python-check-command)))
                                   (if  syntax-checker
                                       (format "%s (%s)"
                                               (file-name-nondirectory
                                                syntax-checker)
                                               syntax-checker)
                                     (format "Not found (%s)"
                                             python-check-command))))))
    (setq maxwidth 0)
    (dolist (row table)
      (when (> (length (car row))
               maxwidth)
        (setq maxwidth (length (car row)))))
    (dolist (row table)
      (insert (car row)
              (make-string (- maxwidth (length (car row)))
                           ?.)
              ": "
              (cdr row)
              "\n"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Elpy Formatted Insertion

(defmacro elpy-insert--popup (buffer-name &rest body)
  "Pop up a help buffer named BUFFER-NAME and execute BODY in it."
  (declare (indent 1))
  `(with-help-window ,buffer-name
     (with-current-buffer standard-output
       ,@body)))

(defun elpy-insert--para (&rest messages)
  "Insert a bunch of text and then fill it."
  (let ((start (point)))
    (mapc (lambda (obj)
            (if (stringp obj)
                (insert obj)
              (insert (format "%s" obj))))
          messages)
    (fill-region start (point))))

(defun elpy-insert--header (&rest text)
  "Insert TEXT has a header for a buffer."
  (insert (propertize (mapconcat #'(lambda (x) x)
                                 text
                                 "")
                      'face 'header-line)
          "\n"
          "\n"))

(define-widget 'elpy-insert--pip-button 'item
  "A button that runs pip (or an alternative)."
  :button-prefix "["
  :button-suffix "]"
  :format "%[run%] %v"
  :value-create 'elpy-config--pip-button-value-create
  :action 'elpy-config--pip-button-action)

(defun elpy-config--pip-button-value-create (widget)
  "The :value-create option for the pip button widget."
  (let* ((python-module (widget-get widget :value))
         (do-user-install (not (or (getenv "VIRTUAL_ENV")
                                   pyvenv-virtual-env)))
         (user-option (if do-user-install
                          "--user "
                        ""))
         (command (cond
                   ((executable-find "pip")
                    (format "pip install %s%s" user-option python-module))
                   ((executable-find "easy_install")
                    (format "easy_install %s%s" user-option python-module))
                   (t
                    (error "Neither easy_install nor pip found")))))
    (widget-put widget :command command)
    (insert command)))

(defun elpy-config--pip-button-action (widget &optional event)
  "The :action option for the pip button widget."
  (async-shell-command (widget-get widget :command)))

;;;;;;;;;;;;;;;;
;;; Elpy modules

(defun elpy-modules-run (command &rest args)
  "Run COMMAND with ARGS for all modules in `elpy-modules'."
  (dolist (module elpy-modules)
    (apply module command args)))

(defun elpy-remove-modeline-lighter (mode-name)
  "Remove the lighter for MODE-NAME.

It's not necessary to see (Python Elpy yas company ElDoc) all the
time. Honestly."
  (interactive)
  (cond
   ((eq mode-name 'eldoc-minor-mode)
    (setq eldoc-minor-mode-string nil))
   (t
    (setcdr (assq mode-name minor-mode-alist)
            (list "")))))

;;;;;;;;;;;;;;;;
;;; Project root

(defvar elpy-project-root nil
  "The root of the project the current buffer is in.")
(make-variable-buffer-local 'elpy-project-root)
(put 'elpy-project-root 'safe-local-variable 'file-directory-p)

(defun elpy-project-root ()
  "Return the root of the current buffer's project.

You can set the variable `elpy-project-root' in, for example,
.dir-locals.el to configure this."
  (when (not elpy-project-root)
    (setq elpy-project-root (elpy-project--find-root)))
  elpy-project-root)

(defun elpy-project--find-root ()
  "Find the first directory in the tree not containing an __init__.py

If there is no __init__.py in the current directory, return the
current directory."
  (cond
   ((and (functionp 'projectile-project-root)
         (projectile-project-root))
    (projectile-project-root))
   ((file-exists-p (format "%s/__init__.py" default-directory))
    (locate-dominating-file default-directory
                            (lambda (dir)
                              (not (file-exists-p
                                    (format "%s/__init__.py" dir))))))
   (t
    default-directory)))

(defun elpy-set-project-root (new-root)
  "Set the Elpy project root to NEW-ROOT."
  (interactive "DNew project root: ")
  (setq elpy-project-root new-root))

;;;;;;;;;;;;;;;;;;;;;
;;; Interactive Shell

(defun elpy-use-ipython (&optional ipython)
  "Set defaults to use IPython instead of the standard interpreter.

With prefix arg, prompt for the command to use."
  (interactive (list (when current-prefix-arg
                       (read-file-name "IPython command: "))))
  (when (not ipython)
    (setq ipython "ipython"))
  (if (boundp 'python-python-command)
      ;; Emacs 24 until 24.3
      (setq python-python-command ipython)
    ;; Emacs 24.3 and onwards.

    ;; This is from the python.el commentary.
    ;; Settings for IPython 0.11:
    (setq python-shell-interpreter ipython
          python-shell-interpreter-args ""
          python-shell-prompt-regexp "In \\[[0-9]+\\]: "
          python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
          python-shell-completion-setup-code
          "from IPython.core.completerlib import module_completion"
          python-shell-completion-module-string-code
          "';'.join(module_completion('''%s'''))\n"
          python-shell-completion-string-code
          "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")))

(defun elpy-use-cpython (&optional cpython)
  "Set defaults to use the standard interpreter instead of IPython.

With prefix arg, prompt for the command to use."
  (interactive (list (when current-prefix-arg
                       (read-file-name "Python command: "))))
  (when (not cpython)
    (setq cpython "python"))
  (if (boundp 'python-python-command)
      ;; Emacs 24 until 24.3
      (setq python-python-command cpython)
    ;; Emacs 24.3 and onwards.
    (setq python-shell-interpreter cpython
          python-shell-interpreter-args "-i"
          python-shell-prompt-regexp ">>> "
          python-shell-prompt-output-regexp ""
          python-shell-completion-setup-code
"try:
    import readline
except ImportError:
    def __COMPLETER_all_completions(text): []
else:
    import rlcompleter
    readline.set_completer(rlcompleter.Completer().complete)
    def __COMPLETER_all_completions(text):
        import sys
        completions = []
        try:
            i = 0
            while True:
                res = readline.get_completer()(text, i)
                if not res: break
                i += 1
                completions.append(res)
        except NameError:
            pass
        return completions"
          python-shell-completion-module-string-code ""
          python-shell-completion-string-code
          "';'.join(__COMPLETER_all_completions('''%s'''))\n")))

(defun elpy-shell-send-region-or-buffer (&optional arg)
  "Send the active region or the buffer to the Python shell.

If there is an active region, send that. Otherwise, send the
whole buffer.

In Emacs 24.3 and later, without prefix argument, this will
escape the Python idiom of if __name__ == '__main__' to be false
to avoid accidental execution of code. With prefix argument, this
code is executed."
  (interactive "P")
  ;; Ensure process exists
  (elpy-shell-get-or-create-process)
  (let ((if-main-regex "^if +__name__ +== +[\"']__main__[\"'] *:")
        (has-if-main nil))
    (if (region-active-p)
        (let ((region (elpy--region-without-indentation
                       (region-beginning) (region-end))))
          (setq has-if-main (string-match if-main-regex region))
          (python-shell-send-string region))
      (save-excursion
        (goto-char (point-min))
        (setq has-if-main (re-search-forward if-main-regex nil t)))
      (python-shell-send-buffer arg))
    (display-buffer (process-buffer (elpy-shell-get-or-create-process))
                    nil
                    'visible)
    (when has-if-main
      (message (concat "Removed if __main__ == '__main__' construct, "
                       "use a prefix argument to evaluate.")))))

(defun elpy--region-without-indentation (beg end)
  "Return the current region as a string, but without indentation."
  (if (= beg end)
      ""
    (let ((region (buffer-substring beg end))
          (indent-level nil))
      (with-temp-buffer
        (insert region)
        (goto-char (point-min))
        (while (< (point) (point-max))
          (cond
           ((and (not indent-level)
                 (not (looking-at "[ \t]*$")))
            (setq indent-level (current-indentation)))
           ((and indent-level
                 (not (looking-at "[ \t]*$"))
                 (< (current-indentation)
                    indent-level))
            (error "Can't adjust indentation, consecutive lines indented less than starting line")))
          (forward-line))
        (indent-rigidly (point-min)
                        (point-max)
                        (- indent-level))
        (buffer-string)))))

(defun elpy-shell-switch-to-shell ()
  "Switch to inferior Python process buffer."
  (interactive)
  (pop-to-buffer (process-buffer (elpy-shell-get-or-create-process)) t))

(defun elpy-shell-get-or-create-process ()
  "Get or create an inferior Python process for current buffer and return it."
  (let* ((bufname (format "*%s*" (python-shell-get-process-name nil)))
         (proc (get-buffer-process bufname)))
    (if proc
        proc
      (run-python (python-shell-parse-command))
      (get-buffer-process bufname))))

;;;;;;;;;;;;;;;;;
;;; Syntax Checks

(defun elpy-check (&optional whole-project-p)
  "Run `python-check-command' on the current buffer's file,

or the project root if WHOLE-PROJECT-P is non-nil (interactively,
with a prefix argument)."
  (interactive "P")
  (when (not (buffer-file-name))
    (error "Can't check a buffer without a file."))
  (save-some-buffers (not compilation-ask-about-save) nil)
  (let ((process-environment (python-shell-calculate-process-environment))
        (exec-path (python-shell-calculate-exec-path))
        (file-name-or-directory (expand-file-name
                                 (if whole-project-p
                                     (elpy-project-root)
                                   (buffer-file-name))))
        (extra-args (if whole-project-p
                        (concat " --exclude="
                                (mapconcat #'identity
                                           elpy-project-ignored-directories
                                           ","))
                      "")))
    (compilation-start (concat python-check-command
                               " "
                               (shell-quote-argument file-name-or-directory)
                               extra-args)
                       nil
                       (lambda (mode-name)
                         "*Python Check*"))))

;;;;;;;;;;;;;;
;;; Navigation

(defun elpy-goto-definition ()
  "Go to the definition of the symbol at point, if found."
  (interactive)
  (let ((location (elpy-rpc-get-definition)))
    (if location
        (elpy-goto-location (car location) (cadr location))
      (error "No definition found"))))

(defun elpy-goto-location (filename offset)
  "Show FILENAME at OFFSET to the user."
  (ring-insert find-tag-marker-ring (point-marker))
  (let ((buffer (find-file filename)))
    (with-current-buffer buffer
      (with-selected-window (get-buffer-window buffer)
        (goto-char (1+ offset))))))

(defun elpy-nav-forward-definition ()
  "Move forward to the next definition (class or function)."
  (interactive)
  (if (save-excursion
        (forward-char 1)
        (re-search-forward "^ *\\(def\\|class\\) " nil t))
      (goto-char (match-beginning 1))
    (goto-char (point-max))))

(defun elpy-nav-backward-definition ()
  "Move backward to the previous definition (class or function)."
  (interactive)
  (if (save-excursion
        (forward-char -1)
        (re-search-backward "^ *\\(def\\|class\\) " nil t))
      (goto-char (match-beginning 1))
    (goto-char (point-min))))

(defun elpy-open-and-indent-line-below ()
  "Open a line below the current one, move there, and indent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(defun elpy-open-and-indent-line-above ()
  "Open a line above the current one, move there, and indent."
  (interactive)
  (move-beginning-of-line 1)
  (save-excursion
    (insert "\n"))
  (indent-according-to-mode))

;;;;;;;;;;;;;;;;
;;; Test running

(defun elpy-test (&optional test-whole-project)
  "Run tests on the current test, or the whole project.

If there is a test at point, run that test. If not, or if a
prefix is given, run all tests in the current project."
  (interactive "P")
  (let ((current-test (elpy-test-at-point)))
    (if test-whole-project
        ;; With prefix arg, test the whole project.
        (funcall elpy-test-runner
                 (car current-test)
                 nil nil nil)
      ;; Else, run only this test
      (apply elpy-test-runner current-test))))

(defvar elpy-set-test-runner-history nil
  "History variable for `elpy-set-test-runner'.")

(defun elpy-set-test-runner (test-runner)
  "Tell Elpy to use TEST-RUNNER to run tests.

See `elpy-test' for how to invoke it."
  (interactive
   (list
    (let* ((runners (mapcar (lambda (value)
                              (cons (nth 2 value)
                                    (nth 3 value)))
                            (cdr (get 'elpy-test-runner 'custom-type))))
           (current (cdr (assq elpy-test-runner
                               (mapcar (lambda (cell)
                                         (cons (cdr cell) (car cell)))
                                       runners))))
           (choice (completing-read (if current
                                        (format "Test runner (currently %s): "
                                                current)
                                      "Test runner: ")
                                    runners
                                    nil t nil 'elpy-set-test-runner-history)))
      (if (equal choice "")
          elpy-test-runner
        (cdr (assoc choice runners))))))
  (setq elpy-test-runner test-runner))

(defun elpy-test-at-point ()
  "Return a list specifying the test at point, if any.

This is used as the interactive

This list has four elements.

- Top level directory:
  All test files should be importable from here.
- Test file:
  The current file name.
- Test module:
  The module name, relative to the top level directory.
- Test name:
  The full name of the current test within the module, for
  example TestClass.test_method

If there is no test at point, test name is nil.
If the current buffer is not visiting a file, only the top level
directory is not nil."
  (if (not buffer-file-name)
      (progn
        (save-some-buffers)
        (list (elpy-project-root) nil nil nil))
    (let* ((top (elpy-project-root))
           (file buffer-file-name)
           (module (elpy-test--module-name top file))
           (test (python-info-current-defun)))
      (if (and test (string-match "test" test))
          (progn
            (save-buffer)
            (list top file module test))
        (save-some-buffers)
        (list top nil nil nil)))))

(defun elpy-test--module-name (top-level module-file)
  "Return the module name relative to TOP-LEVEL for MODULE-FILE.

For example, for a top level of /project/root/ and a module file
of /project/root/package/module.py, this would return
\"package.module\"."
  (let* ((relative-name (file-relative-name module-file top-level))
         (no-extension (replace-regexp-in-string "\\.py\\'" "" relative-name))
         (no-init (replace-regexp-in-string "/__init__\\'" "" no-extension))
         (dotted (replace-regexp-in-string "/" "." no-init)))
    (if (string-match "^\\." dotted)
        (concat "." (replace-regexp-in-string (regexp-quote "...") "." dotted))
      dotted)))

(defun elpy-test-run (working-directory command &rest args)
  "Run COMMAND with ARGS in WORKING-DIRECTORY as a test command."
  (let ((default-directory working-directory))
    (compile (mapconcat #'shell-quote-argument
                        (cons command args)
                        " "))))

(defun elpy-test-discover-runner (top file module test)
  "Test the project using the python unittest discover runner.

This requires Python 2.7 or later."
  (interactive (elpy-test-at-point))
  (if test
      (elpy-test-run top
                     "python" "-m" "unittest" (format "%s.%s" module test))
    (elpy-test-run top
                   "python" "-m" "unittest" "discover")))

(defun elpy-test-django-runner (top file module test)
  "Test the project using the Django discover runner.

This requires Django 1.6 or the django-discover-runner package."
  (interactive (elpy-test-at-point))
  (if test
      (elpy-test-run top
                     "django-admin.py" "test" (format "%s.%s" module test))
    (elpy-test-run top
                   "django-admin.py" "test")))

(defun elpy-test-nose-runner (top file module test)
  "Test the project using the nose test runner.

This requires the nose package to be installed."
  (interactive (elpy-test-at-point))
  (if test
      (elpy-test-run top
                     "nosetests" (format "%s:%s" module test))
    (elpy-test-run top
                   "nosetests")))

(defun elpy-test-pytest-runner (top file module test)
  "Test the project using the py.test test runner.

This requires the pytest package to be installed."
  (interactive (elpy-test-at-point))
  (if test
      ;; Apparently, py.test can't easily run just one method? Let's
      ;; just run the class.
      (let ((test-list (split-string test "\\.")))
        (elpy-test-run top
                       "py.test" (format "%s::%s" module (car test-list))))
    (elpy-test-run top
                   "py.test")))

;;;;;;;;;;;;;;;;;
;;; Documentation

(defvar elpy-doc-history nil
  "History for the `elpy-doc' command.")

(defun elpy-doc ()
  "Show documentation for the symbol at point.

If there is no documentation for the symbol at point, or if a
prefix argument is given, prompt for a symbol from the user."
  (interactive)
  (let ((symbol-at-point nil)
        (doc nil))
    (when (not current-prefix-arg)
      (setq doc (elpy-rpc-get-docstring))
      (when (not doc)
        (setq doc (elpy-rpc-get-pydoc-documentation (elpy-symbol-at-point)))))
    (when (not doc)
      (setq doc (elpy-rpc-get-pydoc-documentation
                 (elpy-doc--read-identifier-from-minibuffer
                  (elpy-symbol-at-point)))))
    (if doc
        (elpy-doc--show doc)
      (error "No documentation found."))))

(defun elpy-doc--read-identifier-from-minibuffer (initial)
  "Read a pydoc-able identifier from the minibuffer."
  (completing-read "Pydoc for: "
                   (completion-table-dynamic #'elpy-rpc-get-pydoc-completions)
                   nil nil initial 'elpy-doc-history))

(defun elpy-doc--show (documentation)
  "Show DOCUMENTATION to the user, replacing ^H with bold."
  (with-help-window "*Python Doc*"
    (with-current-buffer "*Python Doc*"
      (erase-buffer)
      (insert documentation)
      (goto-char (point-min))
      (while (re-search-forward "\\(.\\)\\1" nil t)
        (replace-match (propertize (match-string 1)
                                   'face 'bold)
                       t t)))))

;;;;;;;;;;;;;;;;;
;;; Misc features

(defun elpy-occur-definitions ()
  "Display an occur buffer of all definitions in the current buffer.

Also, switch to that buffer."
  (interactive)
  (let ((list-matching-lines-face nil))
    (occur "^ *\\(def\\|class\\) "))
  (let ((window (get-buffer-window "*Occur*")))
    (if window
        (select-window window)
      (switch-to-buffer "*Occur*"))))

(defun elpy-rgrep-symbol (symbol)
  "Search for SYMBOL in the current project.

SYMBOL defaults to the symbol at point, or the current region if
active.

With a prefix argument, prompt for a string to search for."
  (interactive
   (list
    (cond
     (current-prefix-arg
      (read-from-minibuffer "Search for symbol: "))
     ((use-region-p)
      (buffer-substring-no-properties (region-beginning)
                                      (region-end)))
     (t
      (or (thing-at-point 'symbol)
          (read-from-minibuffer "Search for symbol: "))))))
  (grep-compute-defaults)
  (let ((grep-find-ignored-directories (append elpy-project-ignored-directories
                                               grep-find-ignored-directories)))
    (rgrep (format "\\b%s\\b" symbol)
           "*.py"
           (elpy-project-root)))
  (with-current-buffer next-error-last-buffer
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^find .*" nil t)
          (replace-match (format "\\1\nSearching for symbol %s\n"
                                 symbol)))))))

;;;;;;;;;;;;;;;;;;;
;;; Promise objects

(defvar elpy-promise-marker (make-symbol "*elpy-promise*")
  "An uninterned symbol marking an Elpy promise object.")

(defun elpy-promise (success &optional error)
  "Return a new promise.

A promise is an object with a success and error callback. If the
promise is resolved using `elpy-promise-resolve', its success
callback is called with the given value. The current buffer is
restored, too.

If the promise is rejected using `elpy-promise-reject', its error
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
  "Return non-nil if the argument is a promise object."
  (and (vectorp obj)
       (= (length obj) 5)
       (eq (aref obj 0) elpy-promise-marker)))

(defun elpy-promise-resolved-p (promise)
  "Return non-nil if the PROMISE has been resolved or rejected."
  (aref promise 4))

(defun elpy-promise-resolve (promise value)
  "Resolve PROMISE with VALUE."
  (when (not (aref promise 4))
    (unwind-protect
        (let ((success-callback (aref promise 1)))
          (when success-callback
            (condition-case err
                (with-current-buffer (aref promise 3)
                  (funcall success-callback value))
              (error
               (elpy-promise-reject promise err)))))
      (aset promise 4 t))))

(defun elpy-promise-reject (promise reason)
  "Reject PROMISE because of REASON."
  (when (not (aref promise 4))
    (unwind-protect
        (let ((error-callback (aref promise 2)))
          (when error-callback
            (if (buffer-live-p (aref promise 3))
                (with-current-buffer (aref promise 3)
                  (funcall error-callback reason))
              (with-temp-buffer
                (funcall error-callback reason)))))
      (aset promise 4 t))))

(defun elpy-promise-wait (promise &optional timeout)
  "Wait for PROMISE to be resolved, for up to TIMEOUT seconds.

This will accept process output while waiting."
  (let ((end-time (when timeout
                    (time-add (current-time)
                              (seconds-to-time timeout)))))
    (while (and (not (elpy-promise-resolved-p promise))
                (or (not end-time)
                    (time-less-p (current-time)
                                 end-time)))
      (accept-process-output nil timeout))))

;;;;;;;;;;;;;;;;;;;;;
;;; elpy-rpc backends

;; elpy-rpc is a simple JSON-based RPC protocol. It's mostly JSON-RPC
;; 1.0, except we do not implement the full protocol as we do not need
;; all the features. Emacs starts a Python subprocess which runs a
;; special module. The module reads JSON-RPC requests and responds
;; with JSON-RPC responses.

(defvar elpy-rpc--call-id 0
  "Call id of the last elpy-rpc call.

Used to associate responses to callbacks.")
(make-variable-buffer-local 'elpy-rpc--call-id)

(defvar elpy-rpc--buffer-p nil
  "True iff the current buffer is an elpy-rpc buffer.")
(make-variable-buffer-local 'elpy-rpc--buffer-p)

(defvar elpy-rpc--buffer nil
  "The elpy-rpc buffer associated with this buffer.")
(make-variable-buffer-local 'elpy-rpc--buffer)

(defvar elpy-rpc--backend-project-root nil
  "The project root used by this backend.")
(make-variable-buffer-local 'elpy-rpc--backend-project-root)

(defvar elpy-rpc--backend-python-command nil
  "The Python interpreter used by this backend.")
(make-variable-buffer-local 'elpy-rpc--backend-python-command)

(defvar elpy-rpc--backend-callbacks nil
  "The callbacks registered for calls to the current backend.

This maps call IDs to functions.")
(make-variable-buffer-local 'elpy-rpc--backend-callbacks)

(defvar elpy-rpc--timeout 1
  "Number of seconds to wait for a response.

You can dynamically bind this to a higher value if you want to
wait longer.")

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
    (elpy-promise-wait promise elpy-rpc--timeout)
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
      (setq elpy-rpc--call-id (1+ elpy-rpc--call-id))
      (elpy-rpc--register-callback elpy-rpc--call-id promise)
      (process-send-string
       (get-buffer-process (current-buffer))
       (concat (json-encode `((id . ,elpy-rpc--call-id)
                              (method . ,method-name)
                              (params . ,params)))
               "\n")))
    promise))

(defun elpy-rpc--register-callback (call-id promise)
  "Register for PROMISE to be called when CALL-ID returns.

Must be called in an elpy-rpc buffer."
  (assert elpy-rpc--buffer-p)
  (when (not elpy-rpc--backend-callbacks)
    (setq elpy-rpc--backend-callbacks (make-hash-table :test #'equal)))
  (puthash call-id promise elpy-rpc--backend-callbacks))

(defun elpy-rpc--get-rpc-buffer ()
  "Return the RPC buffer associated with the current buffer,
creating one if necessary."
  (when (not (elpy-rpc--process-buffer-p elpy-rpc--buffer))
    (setq elpy-rpc--buffer
          (or (elpy-rpc--find-buffer (elpy-project-root)
                                     elpy-rpc-python-command)
              (elpy-rpc--open (elpy-project-root)
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

(defun elpy-rpc--find-buffer (project-root python-command)
  "Return an existing RPC buffer for this project root and command."
  (catch 'return
    (dolist (buf (buffer-list))
      (when (and (elpy-rpc--process-buffer-p buf)
                 (equal (buffer-local-value 'elpy-rpc--backend-project-root
                                            buf)
                        project-root)
                 (equal (buffer-local-value 'elpy-rpc--backend-python-command
                                            buf)
                        python-command))
        (throw 'return buf)))
    nil))

(defun elpy-rpc--open (project-root python-command)
  "Start a new RPC process and return the associated buffer."
  ;; Prevent configuration errors
  (when (and elpy-rpc-backend
             (not (stringp elpy-rpc-backend)))
    (error "`elpy-rpc-backend' should be nil or a string."))
  (let* ((name (format "*elpy-rpc [project:%s python:%s]*"
                                      project-root
                                      python-command))
         (new-elpy-rpc-buffer (generate-new-buffer name))
         (proc nil))
    (with-current-buffer new-elpy-rpc-buffer
      (setq elpy-rpc--buffer-p t
            elpy-rpc--buffer (current-buffer)
            elpy-rpc--backend-project-root project-root
            elpy-rpc--backend-python-command python-command
            default-directory "/"
            proc (condition-case err
                     (let ((process-connection-type nil)
                           (process-environment (elpy-rpc--environment)))
                       (start-process name
                                      (current-buffer)
                                      python-command
                                      "-W" "ignore"
                                      "-m" "elpy.__main__"))
                   (error
                    (elpy-config-error
                     "Elpy can't start Python (%s: %s)"
                     (car err) (cadr err)))))
      (set-process-query-on-exit-flag proc nil)
      (set-process-sentinel proc #'elpy-rpc--sentinel)
      (set-process-filter proc #'elpy-rpc--filter))
    (cond
     ;; User requested a specific backend
     (elpy-rpc-backend
      (elpy-rpc-set-backend
       elpy-rpc-backend
       (lambda (result)
         ;; Requested backend successfully set
         t)
       (lambda (err)
         (elpy-config-error "Requested backend %s not available"
                            elpy-rpc-backend))))
     ;; User did not specify a backend, make sure we are not using the
     ;; native one.
     (t
      (elpy-rpc-get-backend
       (lambda (current-backend)
         (when (equal current-backend "native")
           (elpy-config-error "Only native backend found"))))))
    new-elpy-rpc-buffer))

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
          (maphash (lambda (call-id promise)
                     (ignore-errors
                       (elpy-promise-reject promise err)))
                   elpy-rpc--backend-callbacks)
          (setq elpy-rpc--backend-callbacks nil))))))

(defun elpy-rpc--filter (process output)
  "The filter for the RPC process."
  (let ((buffer (process-buffer process)))
    (when (and buffer
               (buffer-live-p buffer))
      (with-current-buffer (process-buffer process)
        (goto-char (point-max))
        (insert output)
        (catch 'return
          (while (progn
                   (goto-char (point-min))
                   (search-forward "\n" nil t))
            (goto-char (point-min))
            (let (json did-read-json)
              (condition-case err
                  (setq json (let ((json-array-type 'list))
                               (json-read))
                        did-read-json t)
                (error
                 (goto-char (point-min))
                 (cond
                  ((looking-at "elpy-rpc ready\n")
                   (replace-match "")
                   (elpy-rpc--check-backend-version "1.1"))
                  ((looking-at "elpy-rpc ready (\\([^ ]*\\))\n")
                   (let ((rpc-version (match-string 1)))
                     (replace-match "")
                     (elpy-rpc--check-backend-version rpc-version)))
                  (t
                   (elpy-rpc--handle-unexpected-line)
                   (throw 'return nil)))))
              (when did-read-json
                (delete-region (point-min) (1+ (point)))
                (elpy-rpc--handle-json json)))))))))

(defun elpy-rpc--check-backend-version (rpc-version)
  "Check that we are using the right version."
  (when (not (equal rpc-version elpy-version))
    (elpy-insert--popup "*Elpy Version Mismatch*"
      (elpy-insert--header "Elpy Version Mismatch")
      (elpy-insert--para
       "You are not using the same version of Elpy in Emacs Lisp"
       "compared to Python. This can cause random problems. Please"
       "do make sure to use compatible versions.\n")
      (insert
       "\n"
       "Elpy Emacs Lisp version: " elpy-version "\n"
       "Elpy Python version....: " rpc-version "\n"))))

(defun elpy-rpc--handle-unexpected-line ()
  "Handle an unexpected line from the backend.

This is usually an error or backtrace."
  (goto-char (point-min))
  (let ((missing-module (when (re-search-forward "No module named \\(.*\\)"
                                                 nil t)
                          (match-string 1))))
    (if (equal missing-module "elpy")
        (elpy-config-error "Elpy module not found")
      (let ((data (buffer-string)))
        (elpy-insert--popup "*Elpy Error*"
          (elpy-insert--header "Error initializing Elpy")
          (elpy-insert--para
           "There was an error when trying to start the backend. "
           "Elpy can not work until this problem is solved. "
           "The following lines were received from Python, and might "
           "help identifying the problem.\n")
          (insert "\n"
                  data))))))

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
  (let ((cls-name (cdr (assq 'name error-object)))
        (text (cdr (assq 'message error-object)))
        (traceback (cdr (assq 'traceback error-object)))
        (config (elpy-config--get-config)))
    (if (not traceback)
        (message "Elpy warning: %s" text)
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
                "\n")
        (elpy-insert--header "Configuration")
        (elpy-config--insert-configuration-table config)
        (insert "\n")
        (elpy-insert--header "Traceback")
        (insert traceback)))))

(defun elpy-rpc--environment ()
  "Return a `process-environment' for the RPC process.

This includes `elpy-rpc-pythonpath' in the PYTHONPATH, if set."
  (if (or (not elpy-rpc-pythonpath)
          (not (file-exists-p (format "%s/elpy/__init__.py"
                                      elpy-rpc-pythonpath))))
      process-environment
    (let* ((old-pythonpath (getenv "PYTHONPATH"))
           (new-pythonpath (if old-pythonpath
                               (concat elpy-rpc-pythonpath ":" old-pythonpath)
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

(defun elpy-rpc-get-available-backends (&optional success  error)
  "Call the get_available_backends API function.

Returns a list of names of available backends, depending on which
Python libraries are installed."
  (elpy-rpc "get_available_backends" nil success error))

(defun elpy-rpc-get-backend (&optional success error)
  "Call the get_backend API function.

Returns the name of the backend currently in use."
  (elpy-rpc "get_backend" nil success error))

(defun elpy-rpc-get-calltip (&optional success error)
  "Call the get_calltip API function.

Returns a calltip string for the function call at point."
  (elpy-rpc "get_calltip"
            (list (expand-file-name (elpy-project-root))
                  buffer-file-name
                  (elpy-rpc--buffer-contents)
                  (- (point)
                     (point-min)))
            success error))

(defun elpy-rpc-get-completions (&optional success error)
  "Call the find_completions API function.

Returns a list of possible completions for the Python symbol at
point."
  (elpy-rpc "get_completions"
            (list (expand-file-name (elpy-project-root))
                  buffer-file-name
                  (elpy-rpc--buffer-contents)
                  (- (point)
                     (point-min)))
            success error))

(defun elpy-rpc-get-definition (&optional success error)
  "Call the find_definition API function.

Returns nil or a list of (filename, point)."
  (elpy-rpc "get_definition"
            (list (expand-file-name (elpy-project-root))
                  buffer-file-name
                  (elpy-rpc--buffer-contents)
                  (- (point)
                     (point-min)))
            success error))

(defun elpy-rpc-get-docstring (&optional success error)
  "Call the get_docstring API function.

Returns a possible multi-line docstring for the symbol at point."
  (elpy-rpc "get_docstring"
            (list (expand-file-name (elpy-project-root))
                  buffer-file-name
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

(defun elpy-rpc-set-backend (backend &optional success error)
  "Call the set_backend API function.

This changes the current backend to the named backend. Raises an
error if the backend is not supported."
  (interactive
   (list (completing-read
          (format "Switch elpy backend (currently %s): "
                  (elpy-rpc-get-backend))
          (elpy-rpc-get-available-backends)
          nil t)))
  (elpy-rpc "set_backend" (list backend) success error))

(defalias 'elpy-set-backend 'elpy-rpc-set-backend)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Module: Sane Defaults

(defun elpy-module-sane-defaults (command &rest args)
  (pcase command
    (`buffer-init
     ;; Set `forward-sexp-function' to nil in python-mode. See
     ;; http://debbugs.gnu.org/db/13/13642.html
     (set (make-local-variable 'forward-sexp-function) nil)
     ;; PEP8 recommends two spaces in front of inline comments.
     (set (make-local-variable 'comment-inline-offset) 2))
    (`buffer-stop
     (kill-local-variable 'forward-sexp-function)
     (kill-local-variable 'comment-inline-offset))))

;;;;;;;;;;;;;;;;;;;
;;; Module: Company

(defun elpy-module-company (command &rest args)
  "Module to support company-mode completions."
  (pcase command
    (`global-init
     (require 'company)
     (elpy-remove-modeline-lighter 'company-mode)
     (define-key company-active-map (kbd "C-d")
       'company-show-doc-buffer))
    (`buffer-init
     ;; We want immediate completions from company.
     (set (make-local-variable 'company-idle-delay)
          t)
     ;; And annotations should be right-aligned
     (set (make-local-variable 'company-tooltip-align-annotations)
          t)
     ;; Add our own backend
     (set (make-local-variable 'company-backends)
          (cons 'elpy-company-backend company-backends))
     (company-mode 1))
    (`buffer-stop
     (company-mode -1)
     (kill-local-variable 'company-idle-delay)
     (kill-local-variable 'company-tooltip-align-annotations)
     (kill-local-variable 'company-backends))
    ;; See `company-require-match'.
    (`require-match
     'never)))

(defvar elpy-company-candidate-cache nil
  "Buffer-local cache for candidate information.")
(make-variable-buffer-local 'elpy-company-candidate-cache)

(defun elpy-company--cache-completions (prefix result)
  "Store RESULT in the candidate cache and return candidates."
  (if elpy-company-candidate-cache
      (clrhash elpy-company-candidate-cache)
    (setq elpy-company-candidate-cache
          (make-hash-table :test #'equal)))
  (mapcar (lambda (completion)
            (let ((name (concat prefix
                                (car completion)))
                  (doc (cadr completion)))
              (puthash name doc
                       elpy-company-candidate-cache)
              name))
          result))

(defun elpy-company-backend (command &optional arg &rest ignored)
  "A company-mode backend for Elpy."
  (interactive (list 'interactive))
  (pcase command
    (`interactive
     (company-begin-backend 'elpy-company-backend))
    ;; init => Called once per buffer
    ;; prefix => return the prefix at point
    (`prefix
     (when (and elpy-mode
                (not (company-in-string-or-comment)))
       (company-grab-symbol-cons "\\." 1)))
    ;; candidates <prefix> => return candidates for this prefix
    (`candidates
     (cons :async
           (lambda (callback)
             (elpy-rpc-get-completions
              (lambda (result)
                (funcall
                 callback
                 (if result
                     (elpy-company--cache-completions arg result)
                   ;; Nothing from elpy, try dabbrev-code
                   (let* ((company-backend 'company-dabbrev-code))
                     (company--process-candidates
                      (company-dabbrev-code
                       'candidates arg))))))))))
    ;; sorted => t if the list is already sorted
    ;; - We could sort it ourselves according to "how likely it is".
    ;;   Does a backend do that?
    ;; duplicates => t if there could be duplicates
    ;; no-cache <prefix> => t if company shouldn't cache results
    ;; meta <candidate> => short docstring for minibuffer
    (`meta
     (let ((doc (when elpy-company-candidate-cache
                  (gethash arg elpy-company-candidate-cache))))
       (when (and doc
                  (string-match "^\\(?:\\s-\\|\n\\)*\\(.*\\)$" doc))
         (match-string 1 doc))))
    ;; annotation <candidate> => short docstring for completion buffer
    (`annotation
     "p")
    ;; doc-buffer <candidate> => put doc buffer in `company-doc-buffer'
    (`doc-buffer
     (let ((doc (when elpy-company-candidate-cache
                  (gethash arg elpy-company-candidate-cache))))
       (when doc
         (company-doc-buffer doc))))
    ;; location <candidate> => (buffer . point) or (file . line-number)
    ;; match <candidate> => for non-prefix based backends
    ;; require-match => user may not enter non-match ... meh?
    ;; post-completion <candidate> => after insertion, for snippets
    ))

;;;;;;;;;;;;;;;;;
;;; Module: ElDoc

(defun elpy-module-eldoc (command &rest args)
  "Module to support ElDoc for Python files."
  (pcase command
    (`global-init
     (setq eldoc-minor-mode-string nil))
    (`buffer-init
     (set (make-local-variable 'eldoc-documentation-function)
          'elpy-eldoc-documentation)
     (eldoc-mode 1))
    (`buffer-stop
     (eldoc-mode -1)
     (kill-local-variable 'eldoc-documentation-function))))

(defun elpy-eldoc-documentation ()
  "Return a call tip for the python call at point."
  (elpy-rpc-get-calltip
   (lambda (calltip)
     (eldoc-message
      (if (not calltip)
          (let ((current-defun (python-info-current-defun)))
            (when current-defun
              (format "In: %s()" current-defun)))
        (with-temp-buffer
          ;; multiprocessing.queues.Queue.cancel_join_thread(self)
          (insert calltip)
          (goto-char (point-min))
          ;; First, remove the whole path up to the second-to-last dot. We
          ;; retain the class just to make it nicer.
          (while (search-forward "." nil t)
            nil)
          (when (search-backward "." nil t 2)
            (delete-region (point-min) (1+ (point))))
          ;; Then remove the occurrence of "self", that's not passed by
          ;; the user.
          (when (re-search-forward "(self\\(, \\)?" nil t)
            (replace-match "("))
          (goto-char (point-min))
          ;; Lastly, we'd like to highlight the argument are on.

          ;; This is tricky with keyword vs. positions arguments, and
          ;; possibly quite complex argument values.

          ;; Hence, we don't do anything for now.
          (buffer-string))))))
  ;; Return the last message until we're done
  eldoc-last-message)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Module: Find File in Project

(defun elpy-module-find-file-in-project (command &rest args)
  "Module to enable finding files in the current project."
  (pcase command
    (`global-init
     (require 'find-file-in-project))
    (`buffer-init
     (when buffer-file-name
       (set (make-local-variable 'ffip-project-root)
            (elpy-project-root))))
    (`buffer-stop
     (kill-local-variable 'ffip-project-root))))

;;;;;;;;;;;;;;;;;;;
;;; Module: Flymake

(defun elpy-module-flymake (command &rest args)
  "Enable Flymake support for Python."
  (pcase command
    (`global-init
     (require 'flymake)
     (elpy-remove-modeline-lighter 'flymake-mode)
     ;; Flymake support using flake8, including warning faces.
     (when (and (executable-find "flake8")
                (not (executable-find python-check-command)))
       (setq python-check-command "flake8"))

     ;; Add our initializer function
     (add-to-list 'flymake-allowed-file-name-masks
                  '("\\.py\\'" elpy-flymake-python-init)))
    (`buffer-init
     ;; `flymake-no-changes-timeout': The original value of 0.5 is too
     ;; short for Python code, as that will result in the current line
     ;; to be highlighted most of the time, and that's annoying. This
     ;; value might be on the long side, but at least it does not, in
     ;; general, interfere with normal interaction.
     (set (make-local-variable 'flymake-no-changes-timeout)
          60)

     ;; `flymake-start-syntax-check-on-newline': This should be nil for
     ;; Python, as;; most lines with a colon at the end will mean the
     ;; next line is always highlighted as error, which is not helpful
     ;; and mostly annoying.
     (set (make-local-variable 'flymake-start-syntax-check-on-newline)
          nil)

     ;; Enable warning faces for flake8 output.
     ;; COMPAT: Obsolete variable as of 24.4
     (if (boundp 'flymake-warning-predicate)
         (set (make-local-variable 'flymake-warning-predicate) "^W[0-9]")
       (set (make-local-variable 'flymake-warning-re) "^W[0-9]"))

     (flymake-mode 1))
    (`buffer-stop
     (flymake-mode -1)
     (kill-local-variable 'flymake-no-changes-timeout)
     (kill-local-variable 'flymake-start-syntax-check-on-newline)
     ;; COMPAT: Obsolete variable as of 24.4
     (if (boundp 'flymake-warning-predicate)
         (kill-local-variable 'flymake-warning-predicate)
       (kill-local-variable 'flymake-warning-re)))))

(defun elpy-flymake-python-init ()
  ;; Make sure it's not a remote buffer as flymake would not work
  (when (not (file-remote-p buffer-file-name))
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace)))
      (list python-check-command
            (list temp-file)
            ;; Run flake8 from / to avoid import problems (#169)
            "/"))))

(defun elpy-flymake-forward-error ()
  "Move forward to the next Flymake error and show a
description."
  (interactive)
  (flymake-goto-next-error)
  (elpy-flymake-show-error))

(defun elpy-flymake-backward-error ()
  "Move backward to the previous Flymake error and show a
description."
  (interactive)
  (flymake-goto-prev-error)
  (elpy-flymake-show-error))

(defun elpy-flymake-show-error ()
  "Show the flymake error message at point."
  (let* ((lineno (line-number-at-pos))
         (err-info (car (flymake-find-err-info flymake-err-info
                                               lineno)))
         (text (mapconcat #'flymake-ler-text
                          err-info
                          ", ")))
    (message "%s" text)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Module: Highlight Indentation

(defun elpy-module-highlight-indentation (command &rest args)
  "Module to highlight indentation in Python files."
  (pcase command
    (`global-init
     (require 'highlight-indentation))
    (`buffer-init
     (highlight-indentation-mode 1))
    (`buffer-stop
     (highlight-indentation-mode -1))))

(defun elpy-module-pyvenv (command &rest args)
  "Module to display the current virtualenv in the mode line."
  (pcase command
    (`global-init
     (pyvenv-mode 1))
    (`global-stop
     (pyvenv-mode -1))))

;;;;;;;;;;;;;;;;;;;;;
;;; Module: Yasnippet

(defun elpy-module-yasnippet (command &rest args)
  "Module to enable YASnippet snippets."
  (pcase command
    (`global-init
     (require 'yasnippet)
     (elpy-remove-modeline-lighter 'yas-minor-mode)

     ;; We provide some YASnippet snippets. Add them.

     ;; yas-snippet-dirs can be a string for a single directory. Make
     ;; sure it's a list in that case so we can add our own entry.
     (when (not (listp yas-snippet-dirs))
       (setq yas-snippet-dirs (list yas-snippet-dirs)))
     (add-to-list 'yas-snippet-dirs
                  (concat (file-name-directory (locate-library "elpy"))
                          "snippets/")
                  t)

     ;; Now load yasnippets.
     (yas-reload-all))
    (`global-stop
     (setq yas-snippet-dirs
           (delete (concat (file-name-directory (locate-library "elpy"))
                           "snippets/")
                   yas-snippet-dirs)))
    (`buffer-init
     (yas-minor-mode 1))
    (`buffer-stop
     (yas-minor-mode -1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Backwards compatibility

;; Functions for Emacs 24 before 24.3
(when (not (fboundp 'python-shell-send-string))
  (defalias 'python-shell-send-string 'python-send-string))
(when (not (fboundp 'python-shell-send-buffer))
  (defun python-shell-send-buffer (&optional arg)
    (python-send-buffer)))
(when (not (fboundp 'python-info-current-defun))
  (defalias 'python-info-current-defun 'python-current-defun))
(when (not (fboundp 'python-nav-backward-statement))
  (defalias 'python-nav-backward-statement 'backward-sexp))

(when (not (fboundp 'python-shell-get-process-name))
  (defun python-shell-get-process-name (dedicated)
    "Compatibility function for older Emacsen."
    "Python"))
(when (not (fboundp 'python-shell-parse-command))
  (defun python-shell-parse-command ()
    "Compatibility function for older Emacsen."
    python-python-command))
(when (not (fboundp 'python-shell-calculate-process-environment))
  (defun python-shell-calculate-process-environment ()
    "Compatibility function for older Emacsen."
    process-environment))
(when (not (fboundp 'python-shell-calculate-exec-path))
  (defun python-shell-calculate-exec-path ()
    "Compatibility function for older Emacsen."
    exec-path))

;; Emacs 24.2 made `locate-dominating-file' accept a predicate instead
;; of a string. Simply overwrite the current one, it's
;; backwards-compatible. The code below is taken from Emacs 24.3.
(when (or (< emacs-major-version 24)
          (and (= emacs-major-version 24)
               (<= emacs-minor-version 2)))
  (defun locate-dominating-file (file name)
    "Look up the directory hierarchy from FILE for a directory containing NAME.
Stop at the first parent directory containing a file NAME,
and return the directory.  Return nil if not found.
Instead of a string, NAME can also be a predicate taking one argument
\(a directory) and returning a non-nil value if that directory is the one for
which we're looking."
    ;; We used to use the above locate-dominating-files code, but the
    ;; directory-files call is very costly, so we're much better off doing
    ;; multiple calls using the code in here.
    ;;
    ;; Represent /home/luser/foo as ~/foo so that we don't try to look for
    ;; `name' in /home or in /.
    (setq file (abbreviate-file-name file))
    (let ((root nil)
          ;; `user' is not initialized outside the loop because
          ;; `file' may not exist, so we may have to walk up part of the
          ;; hierarchy before we find the "initial UID".  Note: currently unused
          ;; (user nil)
          try)
      (while (not (or root
                      (null file)
                      ;; FIXME: Disabled this heuristic because it is sometimes
                      ;; inappropriate.
                      ;; As a heuristic, we stop looking up the hierarchy of
                      ;; directories as soon as we find a directory belonging
                      ;; to another user.  This should save us from looking in
                      ;; things like /net and /afs.  This assumes that all the
                      ;; files inside a project belong to the same user.
                      ;; (let ((prev-user user))
                      ;;   (setq user (nth 2 (file-attributes file)))
                      ;;   (and prev-user (not (equal user prev-user))))
                      (string-match locate-dominating-stop-dir-regexp file)))
        (setq try (if (stringp name)
                      (file-exists-p (expand-file-name name file))
                    (funcall name file)))
        (cond (try (setq root file))
              ((equal file (setq file (file-name-directory
                                       (directory-file-name file))))
               (setq file nil))))
      (if root (file-name-as-directory root))))
  )

;; highlight-indentation 0.5 does not use modes yet
(when (not (fboundp 'highlight-indentation-mode))
  (defun highlight-indentation-mode (on-or-off)
    (cond
     ((and (> on-or-off 0)
           (not highlight-indent-active))
      (highlight-indentation))
     ((and (<= on-or-off 0)
           highlight-indent-active)
      (highlight-indentation)))))

(provide 'elpy)
;;; elpy.el ends here
