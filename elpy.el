;;; elpy.el --- Emacs Python Development Environment -*- lexical-binding: t -*-

;; Copyright (C) 2012-2016  Jorgen Schaefer

;; Author: Jorgen Schaefer <contact@jorgenschaefer.de>
;; URL: https://github.com/jorgenschaefer/elpy
;; Version: 1.17.0
;; Keywords: Python, IDE, Languages, Tools
;; Package-Requires: ((company "0.9.2") (emacs "24.4") (find-file-in-project "3.3")  (highlight-indentation "0.5.0") (pyvenv "1.3") (yasnippet "0.8.0") (s "1.11.0"))

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
;; It combines a number of existing Emacs packages, both written in
;; Emacs Lisp as well as Python.

;; For more information, read the Elpy manual:

;; https://elpy.readthedocs.io/en/latest/index.html

;;; Code:

(require 'cus-edit)
(require 'etags)
(require 'files-x)
(require 'grep)
(require 'ido)
(require 'json)
(require 'python)
(require 'subr-x)
(require 'xref nil t)
(require 'cl-lib)   ; for `cl-every', `cl-copy-list', `cl-delete-if-not'

(require 'elpy-refactor)
(require 'elpy-django)
(require 'elpy-profile)
(require 'elpy-shell)
(require 'pyvenv)
(require 'find-file-in-project)

(defconst elpy-version "1.17.0"
  "The version of the Elpy lisp code.")

;;;;;;;;;;;;;;;;;;;;;;
;;; User customization

(defgroup elpy nil
  "The Emacs Lisp Python Environment."
  :prefix "elpy-"
  :group 'languages)

(defcustom elpy-mode-hook nil
  "Hook run when `elpy-mode' is enabled.

This can be used to enable minor modes for Python development."
  :type 'hook
  :options '(subword-mode hl-line-mode)
  :group 'elpy)

(defcustom elpy-modules '(elpy-module-sane-defaults
                          elpy-module-company
                          elpy-module-eldoc
                          elpy-module-flymake
                          elpy-module-highlight-indentation
                          elpy-module-pyvenv
                          elpy-module-yasnippet
                          elpy-module-django)
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
              (const :tag "Django configurations (Elpy-Django)"
                     elpy-module-django)
              (const :tag "Configure some sane defaults for Emacs"
                     elpy-module-sane-defaults))
  :group 'elpy)

(defcustom elpy-project-ignored-directories
  '(".tox" "build" "dist" ".cask" ".ipynb_checkpoints")
  "Directories ignored by functions working on the whole project.
This is in addition to `vc-directory-exclusion-list'
and `grep-find-ignored-directories', as appropriate."
  :type '(repeat string)
  :safe (lambda (val)
          (cl-every #'stringp val))
  :group 'elpy)

(defun elpy-project-ignored-directories ()
  "Compute the list of ignored directories for project.
Combines
  `elpy-project-ignored-directories'
  `vc-directory-exclusion-list'
  `grep-find-ignored-directories'"
  (delete-dups
   (append elpy-project-ignored-directories
           vc-directory-exclusion-list
           (if (fboundp 'rgrep-find-ignored-directories)
               (rgrep-find-ignored-directories (elpy-project-root))
             (cl-delete-if-not #'stringp (cl-copy-list
                                          grep-find-ignored-directories))))))

(defcustom elpy-project-root nil
  "The root of the project the current buffer is in.

There is normally no use in setting this variable directly, as
Elpy tries to detect the project root automatically. See
`elpy-project-root-finder-functions' for a way of influencing
this.

Setting this variable globally will override Elpy's automatic
project detection facilities entirely.

Alternatively, you can set this in file- or directory-local
variables using \\[add-file-local-variable] or
\\[add-dir-local-variable].

Do not use this variable in Emacs Lisp programs. Instead, call
the `elpy-project-root' function. It will do the right thing."
  :type 'directory
  :safe 'file-directory-p
  :group 'elpy)
(make-variable-buffer-local 'elpy-project-root)

(defcustom elpy-project-root-finder-functions
  '(elpy-project-find-projectile-root
    elpy-project-find-python-root
    elpy-project-find-git-root
    elpy-project-find-hg-root
    elpy-project-find-svn-root)
  "List of functions to ask for the current project root.

These will be checked in turn. The first directory found is used."
  :type '(set (const :tag "Projectile project root"
                     elpy-project-find-projectile-root)
              (const :tag "Python project (setup.py, setup.cfg)"
                     elpy-project-find-python-root)
              (const :tag "Git repository root (.git)"
                     elpy-project-find-git-root)
              (const :tag "Mercurial project root (.hg)"
                     elpy-project-find-hg-root)
              (const :tag "Subversion project root (.svn)"
                     elpy-project-find-svn-root))
  :group 'elpy)

(make-obsolete-variable 'elpy-company-hide-modeline 'elpy-remove-modeline-lighter)
(defcustom elpy-remove-modeline-lighter t
  "Non-nil if Elpy should remove most mode line display.

Modeline shows many minor modes currently active. For Elpy, this is mostly
uninteresting information, but if you rely on your modeline in other modes,
you might want to keep it."
  :type 'boolean
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
                 (const :tag "Automatic" nil))
  :safe (lambda (val)
          (member val '("rope" "jedi" "native" nil)))
  :group 'elpy)

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

(defcustom elpy-company-post-completion-function 'ignore
  "Your preferred Company post completion function.

Elpy can automatically insert parentheses after completing
callable objects.

The heuristic on when to insert these parentheses can easily be
wrong, though, so this is disabled by default. Set this variable
to the function `elpy-company-post-complete-parens' to enable
this feature."
  :type '(choice (const :tag "Ignore post complete" ignore)
                 (const :tag "Complete callables with parens"
                        elpy-company-post-complete-parens)
                 (function :tag "Other function"))
  :group 'elpy)

(defcustom elpy-eldoc-show-current-function t
  "If true, show the current function if no calltip is available.

When Elpy can not find the calltip of the function call at point,
it can show the name of the function or class and method being
edited instead. Setting this variable to nil disables this feature."
  :type 'boolean
  :group 'elpy)

(defcustom elpy-test-runner 'elpy-test-discover-runner
  "The test runner to use to run tests."
  :type '(choice (const :tag "Unittest Discover" elpy-test-discover-runner)
                 (const :tag "Django Discover" elpy-test-django-runner)
                 (const :tag "Nose" elpy-test-nose-runner)
                 (const :tag "py.test" elpy-test-pytest-runner)
                 (const :tag "Twisted Trial" elpy-test-trial-runner))
  :safe 'elpy-test-runner-p
  :group 'elpy)

(defcustom elpy-test-discover-runner-command '("python" "-m" "unittest")
  "The command to use for `elpy-test-discover-runner'."
  :type '(repeat string)
  :group 'elpy)

(defcustom elpy-test-django-runner-command '("django-admin.py" "test"
                                             "--noinput")
  "The command to use for `elpy-test-django-runner'."
  :type '(repeat string)
  :group 'elpy)

(defcustom elpy-test-django-runner-manage-command '("manage.py" "test"
                                                    "--noinput")
  "The command to use for `elpy-test-django-runner' in case we want to use manage.py."
  :type '(repeat string)
  :group 'elpy)

(defcustom elpy-test-django-with-manage nil
  "Set to nil, elpy will use `elpy-test-django-runner-command',
set to t elpy will use `elpy-test-django-runner-manage-command' and set the project root accordingly."
  :type 'boolean
  :group 'elpy)

(defcustom elpy-test-nose-runner-command '("nosetests")
  "The command to use for `elpy-test-nose-runner'."
  :type '(repeat string)
  :group 'elpy)

(defcustom elpy-test-trial-runner-command '("trial")
  "The command to use for `elpy-test-trial-runner'."
  :type '(repeat string)
  :group 'elpy)

(defcustom elpy-test-pytest-runner-command '("py.test")
  "The command to use for `elpy-test-pytest-runner'."
  :type '(repeat string)
  :group 'elpy)

(defcustom elpy-test-compilation-function 'compile
  "Function used by `elpy-test-run' to run a test command.

The function should behave similarly to `compile'. Another good
option is `pdb'."
  :type 'string
  :group 'elpy)

(defcustom elpy-rgrep-file-pattern "*.py"
  "FILES to use for `elpy-rgrep-symbol'."
  :type 'string
  :group 'elpy)

(defcustom elpy-disable-backend-error-display t
  "Non-nil if Elpy should disable backed error display."
  :type 'boolean
  :group 'elpy)

(defcustom elpy-syntax-check-command "flake8"
  "The command to use for `elpy-check'."
  :type 'string
  :group 'elpy)

;;;;;;;;;;;;;
;;; Elpy Mode
(defvar elpy-refactor-map
  (let ((map (make-sparse-keymap "Refactor")))
    (define-key map (kbd "i") (cons (format "%smport fixup"
                                            (propertize "i" 'face 'bold))
                                    'elpy-importmagic-fixup))
    (define-key map (kbd "f") (cons (format "%sormat code"
                                            (propertize "f" 'face 'bold))
                                    'elpy-format-code))
    (define-key map (kbd "r") (cons (format "%sefactor"
                                            (propertize "r" 'face 'bold))
                                    'elpy-refactor))
    map)
  "Key map for the refactor command")

(defvar elpy-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Alphabetical order to make it easier to find free C-c C-X
    ;; bindings in the future. Heh.

    ;; (define-key map (kbd "<backspace>") 'python-indent-dedent-line-backspace)
    ;; (define-key map (kbd "<backtab>")   'python-indent-dedent-line)

    ;; (define-key map (kbd "C-M-x")   'python-shell-send-defun)
    ;; (define-key map (kbd "C-c <")   'python-indent-shift-left)
    ;; (define-key map (kbd "C-c >")   'python-indent-shift-right)
    (define-key map (kbd "C-c RET") 'elpy-importmagic-add-import)
    (define-key map (kbd "C-c C-b") 'elpy-nav-expand-to-indentation)
    (define-key map (kbd "C-c C-c") 'elpy-shell-send-region-or-buffer)
    (define-key map (kbd "C-c C-d") 'elpy-doc)
    (define-key map (kbd "C-c C-e") 'elpy-multiedit-python-symbol-at-point)
    (define-key map (kbd "C-c C-f") 'elpy-find-file)
    (define-key map (kbd "C-c C-n") 'elpy-flymake-next-error)
    (define-key map (kbd "C-c C-o") 'elpy-occur-definitions)
    (define-key map (kbd "C-c C-p") 'elpy-flymake-previous-error)
    (define-key map (kbd "C-c C-s") 'elpy-rgrep-symbol)
    (define-key map (kbd "C-c C-t") 'elpy-test)
    (define-key map (kbd "C-c C-v") 'elpy-check)
    (define-key map (kbd "C-c C-z") 'elpy-shell-switch-to-shell)
    (define-key map (kbd "C-c C-k") 'elpy-shell-kill)
    (define-key map (kbd "C-c C-K") 'elpy-shell-kill-all)
    (define-key map (kbd "C-c C-r") elpy-refactor-map)
    (define-key map (kbd "C-c C-x") elpy-django-mode-map)

    (define-key map (kbd "<S-return>") 'elpy-open-and-indent-line-below)
    (define-key map (kbd "<C-S-return>") 'elpy-open-and-indent-line-above)

    (define-key map (kbd "<C-return>") 'elpy-shell-send-statement-and-step)

    (define-key map (kbd "<C-down>") 'elpy-nav-forward-block)
    (define-key map (kbd "<C-up>") 'elpy-nav-backward-block)
    (define-key map (kbd "<C-left>") 'elpy-nav-backward-indent)
    (define-key map (kbd "<C-right>") 'elpy-nav-forward-indent)

    (define-key map (kbd "<M-down>") 'elpy-nav-move-line-or-region-down)
    (define-key map (kbd "<M-up>") 'elpy-nav-move-line-or-region-up)
    (define-key map (kbd "<M-left>") 'elpy-nav-indent-shift-left)
    (define-key map (kbd "<M-right>") 'elpy-nav-indent-shift-right)

    (if (not (boundp 'xref-find-definitions))
        (define-key map (kbd "M-.") 'elpy-goto-definition))
    (if (not (boundp 'xref-find-definitions-other-window))
        (define-key map (kbd "C-x 4 M-.") 'elpy-goto-definition-other-window)
      (define-key map (kbd "C-x 4 M-.") 'xref-find-definitions-other-window))
    (if (boundp 'xref-pop-marker-stack)
      (define-key map (kbd "M-*") 'xref-pop-marker-stack))

    (define-key map (kbd "M-TAB") 'elpy-company-backend)

    ;; key bindings for elpy-shell
    (define-key map (kbd "C-c C-y e") 'elpy-shell-send-statement)
    (define-key map (kbd "C-c C-y E") 'elpy-shell-send-statement-and-go)
    (define-key map (kbd "C-c C-y s") 'elpy-shell-send-top-statement)
    (define-key map (kbd "C-c C-y S") 'elpy-shell-send-top-statement-and-go)
    (define-key map (kbd "C-c C-y f") 'elpy-shell-send-defun)
    (define-key map (kbd "C-c C-y F") 'elpy-shell-send-defun-and-go)
    (define-key map (kbd "C-c C-y c") 'elpy-shell-send-defclass)
    (define-key map (kbd "C-c C-y C") 'elpy-shell-send-defclass-and-go)
    (define-key map (kbd "C-c C-y g") 'elpy-shell-send-group)
    (define-key map (kbd "C-c C-y G") 'elpy-shell-send-group-and-go)
    (define-key map (kbd "C-c C-y w") 'elpy-shell-send-codecell)
    (define-key map (kbd "C-c C-y W") 'elpy-shell-send-codecell-and-go)
    (define-key map (kbd "C-c C-y r") 'elpy-shell-send-region-or-buffer)
    (define-key map (kbd "C-c C-y R") 'elpy-shell-send-region-or-buffer-and-go)
    (define-key map (kbd "C-c C-y b") 'elpy-shell-send-buffer)
    (define-key map (kbd "C-c C-y B") 'elpy-shell-send-buffer-and-go)
    (define-key map (kbd "C-c C-y C-e") 'elpy-shell-send-statement-and-step)
    (define-key map (kbd "C-c C-y C-S-E") 'elpy-shell-send-statement-and-step-and-go)
    (define-key map (kbd "C-c C-y C-s") 'elpy-shell-send-top-statement-and-step)
    (define-key map (kbd "C-c C-y C-S-S") 'elpy-shell-send-top-statement-and-step-and-go)
    (define-key map (kbd "C-c C-y C-f") 'elpy-shell-send-defun-and-step)
    (define-key map (kbd "C-c C-y C-S-F") 'elpy-shell-send-defun-and-step-and-go)
    (define-key map (kbd "C-c C-y C-c") 'elpy-shell-send-defclass-and-step)
    (define-key map (kbd "C-c C-y C-S-C") 'elpy-shell-send-defclass-and-step-and-go)
    (define-key map (kbd "C-c C-y C-g") 'elpy-shell-send-group-and-step)
    (define-key map (kbd "C-c C-y C-S-G") 'elpy-shell-send-group-and-step-and-go)
    (define-key map (kbd "C-c C-y C-w") 'elpy-shell-send-codecell-and-step)
    (define-key map (kbd "C-c C-y C-S-W") 'elpy-shell-send-codecell-and-step-and-go)
    (define-key map (kbd "C-c C-y C-r") 'elpy-shell-send-region-or-buffer-and-step)
    (define-key map (kbd "C-c C-y C-S-R") 'elpy-shell-send-region-or-buffer-and-step-and-go)
    (define-key map (kbd "C-c C-y C-b") 'elpy-shell-send-buffer-and-step)
    (define-key map (kbd "C-c C-y C-S-B") 'elpy-shell-send-buffer-and-step-and-go)

    map)
  "Key map for the Emacs Lisp Python Environment.")

(easy-menu-define elpy-menu elpy-mode-map
  "Elpy Mode Menu"
  '("Elpy"
    ["Documentation" elpy-doc
     :help "Get documentation for symbol at point"]
    ["Run Tests" elpy-test
     :help "Run test at point, or all tests in the project"]
    ["Go to Definition" elpy-goto-definition
     :help "Go to the definition of the symbol at point"]
    ["Go to previous definition" pop-tag-mark
     :active (not (ring-empty-p find-tag-marker-ring))
     :help "Return to the position"]
    ["Complete" elpy-company-backend
     :keys "M-TAB"
     :help "Complete at point"]
    ["Refactor" elpy-refactor
     :help "Refactor options"]
    "---"
    ("Interactive Python"
     ["Switch to Python Shell" elpy-shell-switch-to-shell
      :help "Start and switch to the interactive Python"]
     ["Send Region or Buffer" elpy-shell-send-region-or-buffer
      :label (if (use-region-p)
                 "Send Region to Python"
               "Send Buffer to Python")
      :help "Send the current region or the whole buffer to Python"]
     ["Send Definition" python-shell-send-defun
      :help "Send current definition to Python"]
     ["Kill Python shell" elpy-shell-kill
      :help "Kill the current Python shell"]
     ["Kill all Python shells" elpy-shell-kill-all
      :help "Kill all Python shells"])
    ("Project"
     ["Find File" elpy-find-file
      :help "Interactively find a file in the current project"]
     ["Find Symbol" elpy-rgrep-symbol
      :help "Find occurrences of a symbol in the current project"]
     ["Set Project Root" elpy-set-project-root
      :help "Change the current project root"]
     ["Set Project Variable" elpy-set-project-variable
      :help "Configure a project-specific option"])
    ("Syntax Check"
     ["Check Syntax" elpy-check
      :help "Check the syntax of the current file"]
     ["Next Error" elpy-flymake-next-error
      :help "Go to the next inline error, if any"]
     ["Previous Error" elpy-flymake-previous-error
      :help "Go to the previous inline error, if any"])
    ("Indentation Blocks"
     ["Dedent" python-indent-shift-left
      :help "Dedent current block or region"
      :suffix (if (use-region-p) "Region" "Block")]
     ["Indent" python-indent-shift-right
      :help "Indent current block or region"
      :suffix (if (use-region-p) "Region" "Block")]
     ["Up" elpy-nav-move-line-or-region-up
      :help "Move current block or region up"
      :suffix (if (use-region-p) "Region" "Block")]
     ["Down" elpy-nav-move-line-or-region-down
      :help "Move current block or region down"
      :suffix (if (use-region-p) "Region" "Block")])
    "---"
    ["News" elpy-news t]
    ["Configure" elpy-config t]))

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
  (elpy-modules-global-init)
  (define-key inferior-python-mode-map (kbd "C-c C-z") 'elpy-shell-switch-to-buffer)
  (add-hook 'python-mode-hook 'elpy-mode)
  (add-hook 'pyvenv-post-activate-hooks 'elpy-rpc--disconnect)
  (add-hook 'inferior-python-mode-hook 'elpy-shell--enable-output-filter))

(defun elpy-disable ()
  "Disable Elpy in all future Python buffers."
  (interactive)
  (remove-hook 'python-mode-hook 'elpy-mode)
  (remove-hook 'inferior-python-mode-hook 'elpy-shell--enable-output-filter)
  (elpy-modules-global-stop))

;;;###autoload
(define-minor-mode elpy-mode
  "Minor mode in Python buffers for the Emacs Lisp Python Environment.

This mode fully supports virtualenvs. Once you switch a
virtualenv using \\[pyvenv-workon], you can use
\\[elpy-rpc-restart] to make the elpy Python process use your
virtualenv.

\\{elpy-mode-map}"
  :lighter " Elpy"
  (when (not (derived-mode-p 'python-mode))
    (error "Elpy only works with `python-mode'"))
  (when (boundp 'xref-backend-functions)
    (add-hook 'xref-backend-functions #'elpy--xref-backend nil t))
  (cond
   (elpy-mode
    (elpy-modules-buffer-init))
   ((not elpy-mode)
    (elpy-modules-buffer-stop))))

;;;;;;;;;;;;;
;;; Elpy News

(defun elpy-news ()
  "Display Elpy's release notes."
  (interactive)
  (with-current-buffer (get-buffer-create "*Elpy News*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert-file-contents (concat (file-name-directory (locate-library "elpy"))
                                    "NEWS.rst"))
      (help-mode))
    (pop-to-buffer (current-buffer))))

;;;;;;;;;;;;;;;
;;; Elpy Config

(defvar elpy-config--related-custom-groups
  '(("Elpy" elpy "elpy-")
    ("Python" python "python-")
    ("Virtual Environments (Pyvenv)" pyvenv "pyvenv-")
    ("Completion (Company)" company "company-")
    ("Call Signatures (ElDoc)" eldoc "eldoc-")
    ("Inline Errors (Flymake)" flymake "flymake-")
    ("Snippets (YASnippet)" yasnippet "yas-")
    ("Directory Grep (rgrep)" grep "grep-")
    ("Search as You Type (ido)" ido "ido-")
    ("Django Extension" elpy-django "elpy-django-")
    ;; ffip does not use defcustom
    ;; highlight-indent does not use defcustom, either. Its sole face
    ;; is defined in basic-faces.
    ))

(defvar elpy-config--get-config "import json
import sys

try:
    import xmlrpclib
except ImportError:
    import xmlrpc.client as xmlrpclib

from distutils.version import LooseVersion


def latest(package, version=None):
    try:
        pypi = xmlrpclib.ServerProxy('https://pypi.python.org/pypi')
        latest = pypi.package_releases(package)[0]
        if version is None or LooseVersion(version) < LooseVersion(latest):
            return latest
        else:
            return None
    except:
        return None


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
    config['jedi_latest'] = latest('jedi', config['jedi_version'])
except:
    config['jedi_version'] = None
    config['jedi_latest'] = latest('jedi')

try:
    import rope
    config['rope_version'] = rope.VERSION
    if sys.version_info[0] <= 2:
        config['rope_latest'] = latest('rope', config['rope_version'])
    else:
        config['rope_latest'] = latest('rope_py3k', config['rope_version'])
except:
    config['rope_version'] = None
    config['rope_latest'] = latest('rope')

try:
    import importmagic
    config['importmagic_version'] = importmagic.__version__
    config['importmagic_latest'] = latest('importmagic', config['importmagic_version'])
except:
    config['importmagic_version'] = None
    config['importmagic_latest'] = latest('importmagic')

try:
    import autopep8
    config['autopep8_version'] = autopep8.__version__
    config['autopep8_latest'] = latest('autopep8', config['autopep8_version'])
except:
    config['autopep8_version'] = None
    config['autopep8_latest'] = latest('autopep8')

try:
    import yapf
    config['yapf_version'] = yapf.__version__
    config['yapf_latest'] = latest('yapf', config['yapf_version'])
except:
    config['yapf_version'] = None
    config['yapf_latest'] = latest('yapf')

json.dump(config, sys.stdout)
")

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
        (dolist (cust elpy-config--related-custom-groups)
          (widget-create 'custom-group
                         :custom-last t
                         :custom-state 'hidden
                         :tag (car cust)
                         :value (cadr cust)))
        (widget-setup)
        (goto-char (point-min))))
    (pop-to-buffer-same-window buf)))

;;;###autoload
(defun elpy-version ()
  "Display the version of Elpy."
  (interactive)
  (message "Elpy %s (use M-x elpy-config for details)" elpy-version))

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
  (let* ((python-version (gethash "python_version" config))
         (rope-pypi-package  (if (and python-version
                                      (string-match "^3\\." python-version))
                                 "rope_py3k"
                               "rope")))

    (elpy-config--insert-configuration-table config)
    (insert "\n")

    ;; Python not found
    (when (not (gethash "python_rpc_executable" config))
      (elpy-insert--para
       "Elpy can not find the configured Python interpreter. Please make "
       "sure that the variable `elpy-rpc-python-command' points to a "
       "command in your PATH. You can change the variable below.\n\n"))

    ;; No virtual env
    (when (and (gethash "python_rpc_executable" config)
               (not (gethash "virtual_env" config)))
      (elpy-insert--para
       "You have not activated a virtual env. While Elpy supports this, "
       "it is often a good idea to work inside a virtual env. You can use "
       "M-x pyvenv-activate or M-x pyvenv-workon to activate a virtual "
       "env.\n\n"))

    ;; No virtual env, but ~/.local/bin not in PATH
    (when (and (not (memq system-type '(ms-dos windows-nt)))
               (gethash "python_rpc_executable" config)
               (not pyvenv-virtual-env)
               (not (or (member (expand-file-name "~/.local/bin")
                                exec-path)
                        (member (expand-file-name "~/.local/bin/")
                                exec-path))))
      (elpy-insert--para
       "The directory ~/.local/bin/ is not in your PATH. As there is "
       "no active virtualenv, installing Python packages locally will "
       "place executables in that directory, so Emacs won't find them. "
       "If you are missing some commands, do add this directory to your "
       "PATH.\n\n"))

    ;; Python found, but can't find the elpy module
    (when (and (gethash "python_rpc_executable" config)
               (not (gethash "elpy_version" config)))
      (elpy-insert--para
       "The Python interpreter could not find the elpy module. "
       "Make sure the module is installed"
       (if (gethash "virtual_env" config)
           " in the current virtualenv.\n"
         ".\n"))
      (insert "\n")
      (widget-create 'elpy-insert--pip-button :package "elpy")
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
      (widget-create 'elpy-insert--pip-button
                     :package (if (equal elpy-rpc-backend "rope")
                                  rope-pypi-package
                                "jedi"))
      (insert "\n\n"))

    ;; No backend available.
    (when (and (gethash "python_rpc_executable" config)
               (and (not elpy-rpc-backend)
                    (not (gethash "rope_version" config))
                    (not (gethash "jedi_version" config))))
      (elpy-insert--para
       "There is no backend available. Please install either Rope or Jedi."
       "See https://github.com/jorgenschaefer/elpy/wiki/FAQ#q-should-i-use-rope-or-jedi for guidance.\n")
      (insert "\n")
      (widget-create 'elpy-insert--pip-button :package rope-pypi-package)
      (insert "\n")
      (widget-create 'elpy-insert--pip-button :package "jedi")
      (insert "\n\n"))

    ;; Newer version of Rope available
    (when (and (gethash "rope_version" config)
               (gethash "rope_latest" config))
      (elpy-insert--para
       "There is a newer version of Rope available.\n")
      (insert "\n")
      (widget-create 'elpy-insert--pip-button
                     :package rope-pypi-package :upgrade t)
      (insert "\n\n"))

    ;; Newer version of Jedi available
    (when (and (gethash "jedi_version" config)
               (gethash "jedi_latest" config))
      (elpy-insert--para
       "There is a newer version of Jedi available.\n")
      (insert "\n")
      (widget-create 'elpy-insert--pip-button
                     :package "jedi" :upgrade t)
      (insert "\n\n"))

    ;; No importmagic available
    (when (not (gethash "importmagic_version" config))
      (elpy-insert--para
       "The importmagic package is not available. Commands using this will "
       "not work.\n")
      (insert "\n")
      (widget-create 'elpy-insert--pip-button
                     :package "importmagic")
      (insert "\n\n"))

    ;; Newer version of importmagic available
    (when (and (gethash "importmagic_version" config)
               (gethash "importmagic_latest" config))
      (elpy-insert--para
       "There is a newer version of the importmagic package available.\n")
      (insert "\n")
      (widget-create 'elpy-insert--pip-button
                     :package "importmagic" :upgrade t)
      (insert "\n\n"))

    ;; No autopep8 available
    (when (not (gethash "autopep8_version" config))
      (elpy-insert--para
       "The autopep8 package is not available. Commands using this will "
       "not work.\n")
      (insert "\n")
      (widget-create 'elpy-insert--pip-button
                     :package "autopep8")
      (insert "\n\n"))

    ;; Newer version of autopep8 available
    (when (and (gethash "autopep8_version" config)
               (gethash "autopep8_latest" config))
      (elpy-insert--para
       "There is a newer version of the autopep8 package available.\n")
      (insert "\n")
      (widget-create 'elpy-insert--pip-button
                     :package "autopep8" :upgrade t)
      (insert "\n\n"))

    ;; No yapf available
    (when (not (gethash "yapf_version" config))
      (elpy-insert--para
       "The yapf package is not available. Commands using this will "
       "not work.\n")
      (insert "\n")
      (widget-create 'elpy-insert--pip-button
                     :package "yapf")
      (insert "\n\n"))

    ;; Newer version of yapf available
    (when (and (gethash "yapf_version" config)
               (gethash "yapf_latest" config))
      (elpy-insert--para
       "There is a newer version of the yapf package available.\n")
      (insert "\n")
      (widget-create 'elpy-insert--pip-button
                     :package "yapf" :upgrade t)
      (insert "\n\n"))

    ;; Syntax checker not available
    (when (not (executable-find elpy-syntax-check-command))
      (elpy-insert--para
       "The configured syntax checker could not be found. Elpy uses this "
       "program to provide syntax checks of your programs, so you might "
       "want to install one. Elpy by default uses flake8.\n")
      (insert "\n")
      (widget-create 'elpy-insert--pip-button :package "flake8")
      (insert "\n\n"))

    ))

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
        (puthash "virtual_env" venv config)
        (if venv
            (puthash "virtual_env_short" (file-name-nondirectory venv) config)
          (puthash "virtual_env_short" nil config)))
      (let ((return-value (ignore-errors
                            (let ((process-environment
                                   (elpy-rpc--environment))
                                  (default-directory "/"))
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
        (jedi-latest (gethash "jedi_latest" config))
        (rope-version (gethash "rope_version" config))
        (rope-latest (gethash "rope_latest" config))
        (importmagic-version (gethash "importmagic_version" config))
        (importmagic-latest (gethash "importmagic_latest" config))
        (autopep8-version (gethash "autopep8_version" config))
        (autopep8-latest (gethash "autopep8_latest" config))
        (yapf-version (gethash "yapf_version" config))
        (yapf-latest (gethash "yapf_latest" config))
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
            ("Jedi" . ,(elpy-config--package-link "jedi"
                                                  jedi-version
                                                  jedi-latest))
            ("Rope" . ,(elpy-config--package-link "rope"
                                                  rope-version
                                                  rope-latest))
            ("Importmagic" . ,(elpy-config--package-link "importmagic"
                                                         importmagic-version
                                                         importmagic-latest))
            ("Autopep8" . ,(elpy-config--package-link "autopep8"
                                                      autopep8-version
                                                      autopep8-latest))
            ("Yapf" . ,(elpy-config--package-link "yapf"
                                                  yapf-version
                                                  yapf-latest))
            ("Syntax checker" . ,(let ((syntax-checker
                                        (executable-find
                                         elpy-syntax-check-command)))
                                   (if  syntax-checker
                                       (format "%s (%s)"
                                               (file-name-nondirectory
                                                syntax-checker)
                                               syntax-checker)
                                     (format "Not found (%s)"
                                             elpy-syntax-check-command))))))
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

(defun elpy-config--package-link (_name version latest)
  "Return a string detailing a Python package.

NAME is the PyPI name of the package. VERSION is the currently
installed version. LATEST is the latest-available version on
PyPI, or nil if that's VERSION."
  (cond
   ((and (not version) (not latest))
    "Not found")
   ((not latest)
    version)
   ((not version)
    (format "Not found (%s available)" latest))
   (t
    (format "%s (%s available)" version latest))))

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
  :value-create 'elpy-insert--pip-button-value-create
  :action 'elpy-insert--pip-button-action)

(defun elpy-insert--pip-button-value-create (widget)
  "The :value-create option for the pip button widget."
  (let* ((python-package (widget-get widget :package))
         (do-upgrade (widget-get widget :upgrade))
         (upgrade-option (if do-upgrade
                             "--upgrade "
                           ""))
         (do-user-install (not (or (getenv "VIRTUAL_ENV")
                                   pyvenv-virtual-env)))
         (user-option (if do-user-install
                          "--user "
                        ""))

         (command (cond
                   ((= (call-process elpy-rpc-python-command
                                     nil nil nil
                                     "-m" "pip" "--help")
                       0)
                    (format "%s -m pip install %s%s%s"
                            elpy-rpc-python-command
                            user-option
                            upgrade-option
                            python-package))
                   ((executable-find "easy_install")
                    (format "easy_install %s%s"
                            user-option python-package))
                   (t
                    (error "Neither easy_install nor pip found")))))
    (widget-put widget :command command)
    (insert command)))

(defun elpy-insert--pip-button-action (widget &optional _event)
  "The :action option for the pip button widget."
  (async-shell-command (widget-get widget :command)))

;;;;;;;;;;;;
;;; Projects

(defvar elpy-project--variable-name-history nil
  "The history for `elpy-project--read-project-variable'")

(defun elpy-project-root ()
  "Return the root of the current buffer's project.

This can very well be nil if the current file is not part of a
project.

See `elpy-project-root-finder-functions' for a way to configure
how the project root is found. You can also set the variable
`elpy-project-root' in, for example, .dir-locals.el to override
this."
  (when (not elpy-project-root)
    (setq elpy-project-root
          (run-hook-with-args-until-success
           'elpy-project-root-finder-functions)))
  elpy-project-root)

(defun elpy-set-project-root (new-root)
  "Set the Elpy project root to NEW-ROOT."
  (interactive "DNew project root: ")
  (setq elpy-project-root new-root))

(defun elpy-project-find-python-root ()
  "Return the current Python project root, if any.

This is marked with setup.py or setup.cfg."
  (or (locate-dominating-file default-directory "setup.py")
      (locate-dominating-file default-directory "setup.cfg")))

(defun elpy-project-find-git-root ()
  "Return the current git repository root, if any."
  (locate-dominating-file default-directory ".git"))

(defun elpy-project-find-hg-root ()
  "Return the current git repository root, if any."
  (locate-dominating-file default-directory ".hg"))

(defun elpy-project-find-svn-root ()
  "Return the current git repository root, if any."
  (locate-dominating-file default-directory
                          (lambda (dir)
                            (and (file-directory-p (format "%s/.svn" dir))
                                 (not (file-directory-p (format "%s/../.svn"
                                                                dir)))))))

(defun elpy-project-find-projectile-root ()
  "Return the current project root according to projectile."
  ;; `ignore-errors' both to avoid an unbound function error as well
  ;; as ignore projectile saying there is no project root here.
  (ignore-errors
    (projectile-project-root)))

(defun elpy-library-root ()
  "Return the root of the Python package chain of the current buffer.

That is, if you have /foo/package/module.py, it will return /foo,
so that import package.module will pick up module.py."
  (locate-dominating-file default-directory
                          (lambda (dir)
                            (not (file-exists-p
                                  (format "%s/__init__.py"
                                          dir))))))

(defun elpy-project--read-project-variable (prompt)
  "Prompt the user for a variable name to set project-wide."
  (let* ((prefixes (mapcar (lambda (cust)
                             (nth 2 cust))
                           elpy-config--related-custom-groups))
         (var-regex (format "^%s" (regexp-opt prefixes))))
    (intern
     (completing-read
      prompt
      obarray
      (lambda (sym)
        (and (get sym 'safe-local-variable)
             (string-match var-regex (symbol-name sym))
             (get sym 'custom-type)))
      :require-match
      nil
      'elpy-project--variable-name-history))))

(defun elpy-project--read-variable-value (prompt variable)
  "Read the value for VARIABLE from the user."
  (let ((custom-type (get variable 'custom-type)))
    (if custom-type
        (widget-prompt-value (if (listp custom-type)
                                 custom-type
                               (list custom-type))
                             prompt
                             (if (boundp variable)
                                 (funcall
                                  (or (get variable 'custom-get)
                                      'symbol-value)
                                  variable))
                             (not (boundp variable)))
      (eval-minibuffer prompt))))

(defun elpy-set-project-variable (variable value)
  "Set or remove a variable in the project-wide .dir-locals.el.

With prefix argument, remove the variable."
  (interactive
   (let* ((variable (elpy-project--read-project-variable
                     (if current-prefix-arg
                         "Remove project variable: "
                       "Set project variable: ")))
          (value (if current-prefix-arg
                     nil
                   (elpy-project--read-variable-value (format "Value for %s: "
                                                              variable)
                                                      variable))))
     (list variable value)))
  (with-current-buffer (find-file-noselect (format "%s/%s"
                                                   (elpy-project-root)
                                                   dir-locals-file))
    (modify-dir-local-variable nil
                               variable
                               value
                               (if current-prefix-arg
                                   'delete
                                 'add-or-replace))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Search Project Files

(defun elpy-rgrep-symbol (regexp)
  "Search for REGEXP in the current project.

REGEXP defaults to the symbol at point, or the current region if
active.

With a prefix argument, always prompt for a string to search for."
  (interactive
   (list
    (let ((symbol
           (if (use-region-p)
               (buffer-substring-no-properties (region-beginning)
                                               (region-end))
             (thing-at-point 'symbol))))
      (if (and symbol (not current-prefix-arg))
          (concat "\\<" symbol "\\>")
        (read-from-minibuffer "Search in project for regexp: " symbol)))))
  (grep-compute-defaults)
  (let ((grep-find-ignored-directories (elpy-project-ignored-directories)))
    (rgrep regexp
           elpy-rgrep-file-pattern
           (or (elpy-project-root)
               default-directory)))
  (with-current-buffer next-error-last-buffer
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^find .*" nil t)
          (replace-match (format "Searching for '%s'\n"
                                 (regexp-quote regexp))))))))

;;;;;;;;;;;;;;;;;;;;;;
;;; Find Project Files

(defcustom elpy-ffip-prune-patterns '()
  "Elpy-specific extension of `ffip-prune-patterns'.
This is in addition to `elpy-project-ignored-directories'
and `completion-ignored-extensions'.
The final value of `ffip-prune-patterns' used is computed
by the eponymous function `elpy-ffip-prune-patterns'."
  :type '(repeat string)
  :safe (lambda (val)
          (cl-every #'stringp val))
  :group 'elpy)

(defun elpy-ffip-prune-patterns ()
  "Compute `ffip-prune-patterns' from other variables.
This combines
  `elpy-ffip-prune-patterns'
  `elpy-project-ignored-directories'
  `completion-ignored-extensions'
  `ffip-prune-patterns'."
  (delete-dups
   (nconc
    (mapcar (lambda (dir) (concat "*/" dir "/*"))
            elpy-project-ignored-directories)
    (mapcar (lambda (ext) (concat "*" ext))
            completion-ignored-extensions)
    (cl-copy-list elpy-ffip-prune-patterns)
    (cl-copy-list ffip-prune-patterns))))

(defun elpy-find-file (&optional dwim)
  "Efficiently find a file in the current project.

With prefix argument, tries to guess what kind of file the user
wants to open.

On an import line, it opens the file of that module.

Otherwise, it opens a test file associated with the current file,
if one exists. A test file is named test_<name>.py if the current
file is <name>.py, and is either in the same directors or a
\"test\" or \"tests\" subdirectory."
  (interactive "P")
  (cond
   ((and dwim
         (buffer-file-name)
         (save-excursion
           (goto-char (line-beginning-position))
           (or (looking-at "^ *import +\\([[:alnum:]._]+\\)")
               (looking-at "^ *from +\\([[:alnum:]._]+\\) +import +\\([[:alnum:]._]+\\)"))))
    (let* ((module (if (match-string 2)
                       (format "%s.%s" (match-string 1) (match-string 2))
                     (match-string 1)))
           (path (elpy-find--resolve-module module)))
      (if path
          (find-file path)
        (elpy-find-file nil))))
   ((and dwim
         (buffer-file-name))
    (let ((test-file (elpy-find--test-file)))
      (if test-file
          (find-file test-file)
        (elpy-find-file nil))))
   (t
    (let ((ffip-prune-patterns (elpy-ffip-prune-patterns))
          (ffip-project-root (or (elpy-project-root)
                                 default-directory))
          ;; Set up ido to use vertical file lists.
          (ido-decorations '("\n" "" "\n" "\n..."
                             "[" "]" " [No match]" " [Matched]"
                             " [Not readable]" " [Too big]"
                             " [Confirm]"))
          (ido-setup-hook (cons (lambda ()
                                  (define-key ido-completion-map (kbd "<down>")
                                    'ido-next-match)
                                  (define-key ido-completion-map (kbd "<up>")
                                    'ido-prev-match))
                                ido-setup-hook)))
      (find-file-in-project)))))

(defun elpy-find--test-file ()
  "Return the test file for the current file, if any.

If this is a test file, return the non-test file.

A test file is named test_<name>.py if the current file is
<name>.py, and is either in the same directors or a \"test\" or
\"tests\" subdirectory."
  (catch 'return
    (let (full-name directory file)
      (setq full-name (buffer-file-name))
      (when (not full-name)
        (throw 'return nil))
      (setq full-name (expand-file-name full-name)
            directory (file-name-directory full-name)
            file (file-name-nondirectory full-name))
      (if (string-match "^test_" file)
          (let ((file (substring file 5)))
            (dolist (implementation (list (format "%s/%s" directory file)
                                          (format "%s/../%s" directory file)))
              (when (file-exists-p implementation)
                (throw 'return implementation))))
        (dolist (test (list (format "%s/test_%s" directory file)
                            (format "%s/test/test_%s" directory file)
                            (format "%s/tests/test_%s" directory file)
                            (format "%s/../test/test_%s" directory file)
                            (format "%s/../tests/test_%s" directory file)))
          (when (file-exists-p test)
            (throw 'return test)))))))

(defun elpy-find--module-path (module)
  "Return a directory path for MODULE.

The resulting path is not guaranteed to exist. This simply
resolves leading periods relative to the current directory and
replaces periods in the middle of the string with slashes.

Only works with absolute imports. Stop using implicit relative
imports. They're a bad idea."
  (let* ((relative-depth (when(string-match "^\\.+" module)
                           (length (match-string 0 module))))
         (base-directory (if relative-depth
                             (format "%s/%s"
                                     (buffer-file-name)
                                     (mapconcat (lambda (_)
                                                  "../")
                                                (make-vector relative-depth
                                                             nil)
                                                ""))
                           (elpy-library-root)))
         (file-name (replace-regexp-in-string
                     "\\."
                     "/"
                     (if relative-depth
                         (substring module relative-depth)
                       module))))
    (expand-file-name (format "%s/%s" base-directory file-name))))

(defun elpy-find--resolve-module (module)
  "Resolve MODULE relative to the current file and project.

Returns a full path name for that module."
  (catch 'return
    (let ((path (elpy-find--module-path module)))
      (while (string-prefix-p (expand-file-name (elpy-library-root))
                              path)
        (dolist (name (list (format "%s.py" path)
                            (format "%s/__init__.py" path)))
          (when (file-exists-p name)
            (throw 'return name)))
        (if (string-match "/$" path)
            (setq path (substring path 0 -1))
          (setq path (file-name-directory path)))))
    nil))

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
                                     (or (elpy-project-root)
                                         (buffer-file-name))
                                   (buffer-file-name))))
        (extra-args (if whole-project-p
                        (concat
                         (if (string-match "pylint$" python-check-command)
                             " --ignore="
                           " --exclude=")
                         (mapconcat #'identity
                                    (elpy-project-ignored-directories)
                                    ","))
                      "")))
    (compilation-start (concat python-check-command
                               " "
                               (shell-quote-argument file-name-or-directory)
                               extra-args)
                       nil
                       (lambda (_mode-name)
                         "*Python Check*"))))

;;;;;;;;;;;;;;
;;; Navigation

(defvar elpy-nav-expand--initial-position nil
  "Initial position before expanding to indentation.")
(make-variable-buffer-local 'elpy-nav-expand--initial-position)

(defun elpy-goto-definition ()
  "Go to the definition of the symbol at point, if found."
  (interactive)
  (let ((location (elpy-rpc-get-definition)))
    (if location
        (elpy-goto-location (car location) (cadr location))
      (error "No definition found"))))

(defun elpy-goto-assignment ()
  "Go to the assignment of the symbol at point, if found."
  (interactive)
  (let ((location (elpy-rpc-get-assignment)))
    (if location
        (elpy-goto-location (car location) (cadr location))
      (error "No assignment found"))))

(defun elpy-goto-definition-other-window ()
  "Go to the definition of the symbol at point in other window, if found."
  (interactive)
  (let ((location (elpy-rpc-get-definition)))
    (if location
        (elpy-goto-location (car location) (cadr location) 'other-window)
      (error "No definition found"))))

(defun elpy-goto-assignment-other-window ()
  "Go to the assignment of the symbol at point in other window, if found."
  (interactive)
  (let ((location (elpy-rpc-get-assignment)))
    (if location
        (elpy-goto-location (car location) (cadr location) 'other-window)
      (error "No assignment found"))))

(defun elpy-goto-location (filename offset &optional other-window-p)
  "Show FILENAME at OFFSET to the user.

If other-window-p is non-nil, show the same in other window."
  (ring-insert find-tag-marker-ring (point-marker))
  (let ((buffer (find-file-noselect filename)))
    (if other-window-p
        (pop-to-buffer buffer t)
      (switch-to-buffer buffer))
    (goto-char (1+ offset))
    (recenter 0)))

(defun elpy-nav-forward-block ()
  "Move to the next line indented like point.

This will skip over lines and statements with different
indentation levels."
  (interactive "^")
  (let ((indent (current-column))
        (start (point))
        (cur nil))
    (when (/= (% indent python-indent-offset)
              0)
      (setq indent (* (1+ (/ indent python-indent-offset))
                      python-indent-offset)))
    (python-nav-forward-statement)
    (while (and (< indent (current-indentation))
                (not (eobp)))
      (when (equal (point) cur)
        (error "Statement does not finish"))
      (setq cur (point))
      (python-nav-forward-statement))
    (when (< (current-indentation)
             indent)
      (goto-char start))))

(defun elpy-nav-backward-block ()
  "Move to the previous line indented like point.

This will skip over lines and statements with different
indentation levels."
  (interactive "^")
  (let ((indent (current-column))
        (start (point))
        (cur nil))
    (when (/= (% indent python-indent-offset)
              0)
      (setq indent (* (1+ (/ indent python-indent-offset))
                      python-indent-offset)))
    (python-nav-backward-statement)
    (while (and (< indent (current-indentation))
                (not (bobp)))
      (when (equal (point) cur)
        (error "Statement does not start"))
      (setq cur (point))
      (python-nav-backward-statement))
    (when (< (current-indentation)
             indent)
      (goto-char start))))

(defun elpy-nav-forward-indent ()
  "Move forward to the next indent level, or over the next word."
  (interactive "^")
  (if (< (current-column) (current-indentation))
      (let* ((current (current-column))
             (next (* (1+ (/ current python-indent-offset))
                      python-indent-offset)))
        (goto-char (+ (point-at-bol)
                      next)))
    (let ((eol (point-at-eol)))
      (forward-word)
      (when (> (point) eol)
        (goto-char (point-at-bol))))))

(defun elpy-nav-backward-indent ()
  "Move backward to the previous indent level, or over the previous word."
  (interactive "^")
  (if (and (<= (current-column) (current-indentation))
           (/= (current-column) 0))
      (let* ((current (current-column))
             (next (* (1- (/ current python-indent-offset))
                      python-indent-offset)))
        (goto-char (+ (point-at-bol)
                      next)))
    (backward-word)))

(defun elpy-nav-move-line-or-region-down (&optional beg end)
  "Move the current line or active region down."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list nil nil)))
  (if beg
      (elpy--nav-move-region-vertically beg end 1)
    (elpy--nav-move-line-vertically 1)))

(defun elpy-nav-move-line-or-region-up (&optional beg end)
  "Move the current line or active region down."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list nil nil)))
  (if beg
      (elpy--nav-move-region-vertically beg end -1)
    (elpy--nav-move-line-vertically -1)))

(defun elpy--nav-move-line-vertically (dir)
  (let* ((beg (point-at-bol))
         (end (point-at-bol 2))
         (col (current-column))
         (region (delete-and-extract-region beg end)))
    (forward-line dir)
    (save-excursion
      (insert region))
    (goto-char (+ (point) col))))

(defun elpy--nav-move-region-vertically (beg end dir)
  (let* ((point-before-mark (< (point) (mark)))
         (beg (save-excursion
                (goto-char beg)
                (point-at-bol)))
         (end (save-excursion
                (goto-char end)
                (if (bolp)
                    (point)
                  (point-at-bol 2))))
         (region (delete-and-extract-region beg end)))
    (goto-char beg)
    (forward-line dir)
    (save-excursion
      (insert region))
    (if point-before-mark
        (set-mark (+ (point)
                     (length region)))
      (set-mark (point))
      (goto-char (+ (point)
                    (length region))))
    (setq deactivate-mark nil)))

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

(defun elpy-nav-expand-to-indentation ()
  "Select surrounding lines with current indentation."
  (interactive)
  (setq elpy-nav-expand--initial-position (point))
  (let ((indentation (current-indentation)))
    (if (= indentation 0)
        (mark-whole-buffer)
      (while (<= indentation (current-indentation))
        (forward-line -1))
      (forward-line 1)
      (push-mark (point) nil t)
      (while (<= indentation (current-indentation))
        (forward-line 1))
      (backward-char))))

(defadvice keyboard-quit (before collapse-region activate)
  (when (eq last-command 'elpy-nav-expand-to-indentation)
    (goto-char elpy-nav-expand--initial-position)))

(defun elpy-nav-normalize-region ()
  "If the first or last line are not fully selected, select them completely."
  (let ((beg (region-beginning))
        (end (region-end)))
    (goto-char beg)
    (beginning-of-line)
    (push-mark (point) nil t)
    (goto-char end)
    (when (not (= (point) (line-beginning-position)))
      (end-of-line))))

(defun elpy-nav-indent-shift-right (&optional _count)
  "Shift current line by COUNT columns to the right.

COUNT defaults to `python-indent-offset'.
If region is active, normalize the region and shift."
  (interactive)
  (if (use-region-p)
      (progn
        (elpy-nav-normalize-region)
        (python-indent-shift-right (region-beginning) (region-end) current-prefix-arg))
    (python-indent-shift-right (line-beginning-position) (line-end-position) current-prefix-arg)))

(defun elpy-nav-indent-shift-left (&optional _count)
  "Shift current line by COUNT columns to the left.

COUNT defaults to `python-indent-offset'.
If region is active, normalize the region and shift."
  (interactive)
  (if (use-region-p)
      (progn
        (elpy-nav-normalize-region)
        (python-indent-shift-left (region-beginning) (region-end) current-prefix-arg))
    (python-indent-shift-left (line-beginning-position) (line-end-position) current-prefix-arg)))


;;;;;;;;;;;;;;;;
;;; Test running

(defvar elpy-set-test-runner-history nil
  "History variable for `elpy-set-test-runner'.")

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
        (list (elpy-library-root) nil nil nil))
    (let* ((top (elpy-library-root))
           (file buffer-file-name)
           (module (elpy-test--module-name-for-file top file))
           (test (elpy-test--current-test-name)))
      (if (and file (string-match "test" (or module test "")))
          (progn
            (save-buffer)
            (list top file module test))
        (save-some-buffers)
        (list top nil nil nil)))))

(defun elpy-test--current-test-name ()
  (let ((name (python-info-current-defun)))
    (if (and name
             (string-match "\\`\\([^.]+\\.[^.]+\\)\\." name))
        (match-string 1 name)
      name)))

(defun elpy-test--module-name-for-file (top-level module-file)
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

(defun elpy-test-runner-p (obj)
  "Return t iff OBJ is a test runner.

This uses the `elpy-test-runner-p' symbol property."
  (get obj 'elpy-test-runner-p))

(defun elpy-test-run (working-directory command &rest args)
  "Run COMMAND with ARGS in WORKING-DIRECTORY as a test command."
  (let ((default-directory working-directory))
    (funcall elpy-test-compilation-function
             (mapconcat #'shell-quote-argument
                        (cons command args)
                        " "))))

(defun elpy-test-discover-runner (top _file module test)
  "Test the project using the python unittest discover runner.

This requires Python 2.7 or later."
  (interactive (elpy-test-at-point))
  (let ((test (cond
               (test (format "%s.%s" module test))
               (module module)
               (t "discover"))))
    (apply #'elpy-test-run
           top
           (append elpy-test-discover-runner-command
                   (list test)))))
(put 'elpy-test-discover-runner 'elpy-test-runner-p t)

(defun elpy-test-django-runner (top _file module test)
  "Test the project using the Django discover runner,
or with manage.py if elpy-test-django-with-manage is true.

This requires Django 1.6 or the django-discover-runner package."
  (interactive (elpy-test-at-point))
  (if module
      (apply #'elpy-test-run
             top
             (append
              ;; if we want to use manage.py, get the root directory where it is.
              (if elpy-test-django-with-manage
                  (append (list (concat (expand-file-name
                                         (locate-dominating-file
                                          (elpy-project-root)
                                          (car elpy-test-django-runner-manage-command)))
                                        (car elpy-test-django-runner-manage-command)))
                          (cdr elpy-test-django-runner-manage-command))
                ;; or the default:
                elpy-test-django-runner-command)
              (list (if test
                        (format "%s.%s" module test)
                      module))))
    (apply #'elpy-test-run
           top
           (append
            (if elpy-test-django-with-manage
                (append (list (concat (expand-file-name
                                       (locate-dominating-file
                                        (if (elpy-project-root)
                                            (elpy-project-root)
                                          ".")
                                        (car elpy-test-django-runner-manage-command)))
                                      (car elpy-test-django-runner-manage-command)))
                        (cdr elpy-test-django-runner-manage-command))
              elpy-test-django-runner-command)))))
(put 'elpy-test-django-runner 'elpy-test-runner-p t)

(defun elpy-test-nose-runner (top _file module test)
  "Test the project using the nose test runner.

This requires the nose package to be installed."
  (interactive (elpy-test-at-point))
  (if module
      (apply #'elpy-test-run
             top
             (append elpy-test-nose-runner-command
                     (list (if test
                               (format "%s:%s" module test)
                             module))))
    (apply #'elpy-test-run
           top
           elpy-test-nose-runner-command)))
(put 'elpy-test-nose-runner 'elpy-test-runner-p t)

(defun elpy-test-trial-runner (top _file module test)
  "Test the project using Twisted's Trial test runner.

This requires the twisted-core package to be installed."
  (interactive (elpy-test-at-point))
  (if module
      (apply #'elpy-test-run
             top
             (append elpy-test-trial-runner-command
                     (list (if test
                               (format "%s.%s" module test)
                             module))))
    (apply #'elpy-test-run top elpy-test-trial-runner-command)))
(put 'elpy-test-trial-runner 'elpy-test-runner-p t)

(defun elpy-test-pytest-runner (top file module test)
  "Test the project using the py.test test runner.

This requires the pytest package to be installed."
  (interactive (elpy-test-at-point))
  (cond
   (test
    (let ((test-list (split-string test "\\.")))
      (apply #'elpy-test-run
             top
             (append elpy-test-pytest-runner-command
                     (list (mapconcat #'identity
                                      (cons file test-list)
                                      "::"))))))
   (module
    (apply #'elpy-test-run top (append elpy-test-pytest-runner-command
                                       (list file))))
   (t
    (apply #'elpy-test-run top elpy-test-pytest-runner-command))))
(put 'elpy-test-pytest-runner 'elpy-test-runner-p t)

;;;;;;;;;;;;;;;;;
;;; Documentation

(defvar elpy-doc-history nil
  "History for the `elpy-doc' command.")

(defun elpy-doc ()
  "Show documentation for the symbol at point.

If there is no documentation for the symbol at point, or if a
prefix argument is given, prompt for a symbol from the user."
  (interactive)
  (let ((doc nil))
    (when (not current-prefix-arg)
      (setq doc (elpy-rpc-get-docstring))
      (when (not doc)
        (save-excursion
          (python-nav-backward-up-list)
          (setq doc (elpy-rpc-get-docstring))))
      (when (not doc)
        (setq doc (elpy-rpc-get-pydoc-documentation
                   (elpy-doc--symbol-at-point))))
      (when (not doc)
        (save-excursion
          (python-nav-backward-up-list)
          (setq doc (elpy-rpc-get-pydoc-documentation
                     (elpy-doc--symbol-at-point))))))
    (when (not doc)
      (setq doc (elpy-rpc-get-pydoc-documentation
                 (elpy-doc--read-identifier-from-minibuffer
                  (elpy-doc--symbol-at-point)))))
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

(defun elpy-doc--symbol-at-point ()
  "Return the Python symbol at point, including dotted paths."
  (with-syntax-table python-dotty-syntax-table
    (let ((symbol (symbol-at-point)))
      (if symbol
          (symbol-name symbol)
        nil))))

;;;;;;;;;;;;;;
;;; Buffer manipulation

(defun elpy-buffer--replace-block (spec)
  "Replace a block.  SPEC is (startline endline newblock)."
  (let ((start-line (nth 0 spec))
        (end-line (nth 1 spec))
        (new-block (nth 2 spec)))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (forward-line start-line)
        (let ((beg (point))
              (end (progn (forward-line (- end-line start-line)) (point))))
          ;; Avoid deleting and re-inserting when the blocks are equal.
          (unless (string-equal (buffer-substring beg end) new-block)
            (delete-region beg end)
            (insert new-block)))))))

(defun elpy-buffer--replace-region (beg end rep)
  "Replace text in BUFFER in region (BEG END) with REP."
  (unless (string-equal (buffer-substring beg end) rep)
    (save-excursion
      (goto-char end)
      (insert rep)
      (delete-region beg end))))


;;;;;;;;;;;;;;;;;;;;;;;
;;; Import manipulation

(defun elpy-importmagic--add-import-read-args (&optional object prompt ask-for-alias)
  (let* ((default-object (save-excursion
                           (let ((bounds (with-syntax-table python-dotty-syntax-table
                                           (bounds-of-thing-at-point 'symbol))))
                             (if bounds (buffer-substring (car bounds) (cdr bounds)) ""))))
         (object-to-import (or object (read-string "Object to import: " default-object)))
         (possible-imports (elpy-rpc "get_import_symbols" (list buffer-file-name
                                                                (elpy-rpc--buffer-contents)
                                                                object-to-import)))
         (statement-prompt (or prompt "New import statement: ")))
    (cond
     ;; An elpy warning (i.e. index not ready) is returned as a string.
     ((stringp possible-imports)
      "")
     ;; If there is no candidate, we exit immediately.
     ((null possible-imports)
      (message "No import candidate found")
      "")
     ;; We have some candidates, let the user choose one.
     (t
      (let* ((first-choice (car possible-imports))
	     (user-choice (completing-read statement-prompt possible-imports))
	     (alias (if ask-for-alias (read-string (format "Import \"%s\" as: " object-to-import)) "")))
	(concat (if (equal user-choice "") first-choice user-choice)
		(if (not (or (equal alias "") (equal alias object-to-import))) (concat " as " alias))))))))

(defun elpy-importmagic-add-import (&optional statement ask-for-alias)
  "Prompt to import thing at point, show possible imports and add selected import.

With prefix arg, also ask for an import alias."
  (interactive "i\nP")
  (let* ((statement (or statement (elpy-importmagic--add-import-read-args nil nil ask-for-alias)))
	 (res (elpy-rpc "add_import" (list buffer-file-name
					   (elpy-rpc--buffer-contents)
					   statement))))
    (unless (equal statement "")
      (elpy-buffer--replace-block res))))

(defun elpy-importmagic-fixup ()
  "Query for new imports of unresolved symbols, and remove unreferenced imports.

Also sort the imports in the import statement blocks."
  (interactive)
  ;; get all unresolved names, and interactively add imports for them
  (let* ((res (elpy-rpc "get_unresolved_symbols" (list buffer-file-name
						       (elpy-rpc--buffer-contents))))
	 (unresolved-aliases (list)))
    (unless (stringp res)
      (if (null res) (message "No imports to add."))
      (dolist (object res)
        (let* ((prompt (format "How to import \"%s\": " object))
               (choice (elpy-importmagic--add-import-read-args object prompt nil)))
	  (when (equal choice "")
	    (push (car (split-string object "\\.")) unresolved-aliases))
	  (elpy-importmagic-add-import choice nil))))
    ;; ask for unresolved aliases real names and add import for them
    (dolist (alias unresolved-aliases)
      (let* ((object (read-string (format "Real name of \"%s\" alias: " alias nil)))
	     (prompt (format "How to import \"%s\": " object))
	     (choice (concat
		      (elpy-importmagic--add-import-read-args object prompt nil)
		      (concat " as " alias))))
	(elpy-importmagic-add-import choice nil))))
  ;; now get a new import statement block (this also sorts)
  (let* ((res (elpy-rpc "remove_unreferenced_imports" (list buffer-file-name
                                                            (elpy-rpc--buffer-contents)))))
    (unless (stringp res)
      (elpy-buffer--replace-block res))))

;;;;;;;;;;;;;;;;;;;;;
;;; Code reformatting

(defun elpy-format-code ()
  "Format code using the available formatter."
  (interactive)
  (cond
   ((executable-find "yapf")
    (elpy-yapf-fix-code))
   ((executable-find "autopep8")
    (elpy-autopep8-fix-code))
   (t
    (message "Install yapf/autopep8 to format code."))))

(defun elpy-yapf-fix-code ()
  "Automatically formats Python code with yapf."
  (interactive)
  (elpy--fix-code-with-formatter "fix_code_with_yapf"))

(defun elpy-autopep8-fix-code ()
  "Automatically formats Python code to conform to the PEP 8 style guide."
  (interactive)
  (elpy--fix-code-with-formatter "fix_code"))

(defun elpy--fix-code-with-formatter (method)
  "Common routine for formatting python code."
  (let ((line (line-number-at-pos))
        (col (current-column)))
    (if (use-region-p)
        (let ((new-block (elpy-rpc method (list (elpy-rpc--region-contents))))
              (beg (region-beginning)) (end (region-end)))
          (elpy-buffer--replace-region
           beg end
           (replace-regexp-in-string "\n$" "" new-block))
          (goto-char end)
          (deactivate-mark))
      ;; Vector instead of list, json.el in Emacs 24.3 and before
      ;; breaks for single-element lists of alists.
      (let ((new-block (elpy-rpc method (vector (elpy-rpc--buffer-contents))))
            (beg (point-min))
            (end (point-max)))
        (elpy-buffer--replace-region beg end new-block)
        (when (bobp)
          (forward-line (1- line))
          (forward-char col))))))

;;;;;;;;;;;;;;
;;; Multi-Edit

(defvar elpy-multiedit-overlays nil
  "List of overlays currently being edited.")

(defun elpy-multiedit-add-overlay (beg end)
  "Add an editable overlay between BEG and END.

A modification in any of these overlays will modify all other
overlays, too."
  (interactive "r")
  (when (elpy-multiedit--overlays-in-p beg end)
    (error "Overlapping multiedit overlays are not allowed"))
  (let ((ov (make-overlay beg end nil nil :rear-advance)))
    (overlay-put ov 'elpy-multiedit t)
    (overlay-put ov 'face 'highlight)
    (overlay-put ov 'modification-hooks '(elpy-multiedit--overlay-changed))
    (overlay-put ov 'insert-in-front-hooks '(elpy-multiedit--overlay-changed))
    (overlay-put ov 'insert-behind-hooks '(elpy-multiedit--overlay-changed))
    (push ov elpy-multiedit-overlays)))

(defun elpy-multiedit--overlays-in-p (beg end)
  "Return t iff there are multiedit overlays between beg and end."
  (catch 'return
    (dolist (ov (overlays-in beg end))
      (when (overlay-get ov 'elpy-multiedit)
        (throw 'return t)))
    nil))

(defun elpy-multiedit-stop ()
  "Stop editing multiple places at once."
  (interactive)
  (dolist (ov elpy-multiedit-overlays)
    (delete-overlay ov))
  (setq elpy-multiedit-overlays nil))

(defun elpy-multiedit--overlay-changed (ov after-change _beg _end
                                           &optional _pre-change-length)
  "Called for each overlay that changes.

This updates all other overlays."
  (when (and after-change
             (not undo-in-progress)
             (overlay-buffer ov))
    (let ((text (buffer-substring (overlay-start ov)
                                  (overlay-end ov)))
          (inhibit-modification-hooks t))
      (dolist (other-ov elpy-multiedit-overlays)
        (when (and (not (equal other-ov ov))
                   (buffer-live-p (overlay-buffer other-ov)))
          (with-current-buffer (overlay-buffer other-ov)
            (save-excursion
              (goto-char (overlay-start other-ov))
              (insert text)
              (delete-region (point) (overlay-end other-ov)))))))))

(defun elpy-multiedit ()
  "Edit all occurences of the symbol at point, or the active region.

If multiedit is active, stop it."
  (interactive)
  (if elpy-multiedit-overlays
      (elpy-multiedit-stop)
    (let ((regex (if (use-region-p)
                     (regexp-quote (buffer-substring (region-beginning)
                                                     (region-end)))
                   (format "\\_<%s\\_>" (regexp-quote
                                         (symbol-name
                                          (symbol-at-point))))))
          (case-fold-search nil))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward regex nil t)
          (elpy-multiedit-add-overlay (match-beginning 0)
                                      (match-end 0)))))))

(defun elpy-multiedit-python-symbol-at-point (&optional use-symbol-p)
  "Edit all usages of the the Python symbol at point.

With prefix arg, edit all syntactic usages of the symbol at
point. This might include unrelated symbols that just share the
name."
  (interactive "P")
  (if (or elpy-multiedit-overlays
          use-symbol-p
          (use-region-p))
      ;; If we are already doing a multiedit, or are explicitly told
      ;; to use the symbol at point, or if we are on an active region,
      ;; call the multiedit function that does just that already.
      (call-interactively 'elpy-multiedit)
    ;; Otherwise, fetch usages from backend.
    (save-some-buffers)
    (let ((usages (condition-case err
                      (elpy-rpc-get-usages)
                    ;; This is quite the stunt, but elisp parses JSON
                    ;; null as nil, which is indistinguishable from
                    ;; the empty list, we stick to the error.
                    (error
                     (if (and (eq (car err) 'error)
                              (stringp (cadr err))
                              (string-match "not implemented" (cadr err)))
                         'not-supported
                       (error (cadr err)))))))
      (cond
       ((eq usages 'not-supported)
        (call-interactively 'elpy-multiedit)
        (message (concat "Using syntactic editing "
                         "as current backend does not support get_usages.")))
       ((null usages)
        (call-interactively 'elpy-multiedit)
        (if elpy-multiedit-overlays
            (message (concat "Using syntactic editing as no usages of the "
                             "symbol at point were found by the backend."))
          (message "No occurrences of the symbol at point found")))
       (t
        (save-restriction
          (widen)
          (elpy-multiedit--usages usages)))))))

(defun elpy-multiedit--usages (usages)
  "Mark the usages in USAGES for editing."
  (let ((name nil)
        (locations (make-hash-table :test #'equal)))
    (dolist (usage usages)
      (let* ((filename (cdr (assq 'filename usage)))
             (this-name (cdr (assq 'name usage)))
             (offset (cdr (assq 'offset usage))))
        (setq name this-name)
        (with-current-buffer (if filename
                                 (find-file-noselect filename)
                               (current-buffer))
          (elpy-multiedit-add-overlay (+ offset 1)
                                      (+ offset 1 (length this-name)))
          (save-excursion
            (goto-char (+ offset 1))
            (puthash filename
                     (cons (list offset
                                 (buffer-substring (line-beginning-position)
                                                   (line-end-position))
                                 (- (point)
                                    (line-beginning-position))
                                 (- (+ (point) (length this-name))
                                    (line-beginning-position)))
                           (gethash filename locations))
                     locations)))))
    (if (<= (hash-table-count locations)
            1)
        (message "Editing %s usages of '%s' in this buffer"
                 (length usages) name)
      (with-current-buffer (get-buffer-create "*Elpy Edit Usages*")
        (let ((inhibit-read-only t)
              (filenames nil))
          (erase-buffer)
          (elpy-insert--para
           "The symbol '" name "' was found in multiple files. Editing "
           "all locations:\n\n")
          (maphash (lambda (key _value)
                     (when (not (member key filenames))
                       (setq filenames (cons key filenames))))
                   locations)
          (dolist (filename (sort filenames #'string<))
            (elpy-insert--header filename)
            (dolist (location (sort (gethash filename locations)
                                    (lambda (loc1 loc2)
                                      (< (car loc1)
                                         (car loc2)))))
              (let ((line (nth 1 location))
                    (start (+ (line-beginning-position)
                              (nth 2 location)))
                    (end (+ (line-end-position)
                            (nth 3 location))))
                ;; Insert the \n first, else we extend the overlay.
                (insert line "\n")
                (elpy-multiedit-add-overlay start end)))
            (insert "\n"))
          (goto-char (point-min))
          (display-buffer (current-buffer)
                          nil
                          'visible))))))

;;;;;;;;;;;;;;;;;;;;;
;;; Occur Definitions

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

;;;;;;;
;;; RPC

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
       (concat (json-encode `((id . ,elpy-rpc--call-id)
                              (method . ,method-name)
                              (params . ,params)))
               "\n")))
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
  (when (and elpy-rpc-backend
             (not (stringp elpy-rpc-backend)))
    (error "`elpy-rpc-backend' should be nil or a string."))
  (elpy-rpc--cleanup-buffers)
  (let* ((full-python-command (executable-find python-command))
         (name (format " *elpy-rpc [project:%s python:%s]*"
                       library-root
                       full-python-command))
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
      (elpy-rpc-init elpy-rpc-backend library-root
                     (lambda (result)
                       (let ((backend (cdr (assq 'backend result))))
                         (when (and elpy-rpc-backend
                                    (not (equal backend elpy-rpc-backend)))
                           (elpy-config-error
                            "Can't set backend %s, using %s instead"
                            elpy-rpc-backend backend))))))
    new-elpy-rpc-buffer))

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
                  (setq json (let ((json-array-type 'list))
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
       "do make sure to use compatible versions.\n")
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
         "This is usually some module that does not use correct logging, "
         "but might indicate a configuration problem.\n\n")
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
      (when (memq 'elpy-mode minor-mode-list)
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

(defun elpy-rpc-init (backend library-root &optional success error)
  "Initialize the backend.

This has to be called as the first method, else Elpy won't be
able to respond to other calls."
  (elpy-rpc "init"
            ;; This uses a vector because otherwise, json-encode in
            ;; older Emacsen gets seriously confused, especially when
            ;; backend is nil.
            (vector `((backend . ,backend)
                      (project_root . ,(expand-file-name library-root))))
            success error))

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

(defun elpy-rpc-get-completions (&optional success error)
  "Call the get_completions API function.

Returns a list of possible completions for the Python symbol at
point."
  (when (< (buffer-size) elpy-rpc-ignored-buffer-size)
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
  "Return the symbol under point usages as a list"
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

;;;;;;;;;;;;;;;;
;;; Xref backend
(defun elpy--xref-backend ()
  "Return the name of the elpy xref backend."
  (if (string= elpy-rpc-backend "jedi")
      'elpy
    nil))

(defvar elpy-xref--format-references
  (let ((str "%s:\t%s"))
    (put-text-property 0 4 'face 'font-lock-comment-face str)
    str)
  "String used to format references in xref buffers.")

;; Elpy location structure
(when (featurep 'xref)
  (cl-defstruct (xref-elpy-location
                 (:constructor xref-make-elpy-location (file pos)))
    "Location of a python symbol definition."
    file pos)

  (defun xref-make-elpy-location (file pos)
    "Return an elpy location structure.
Points to file FILE, at position POS."
    (make-instance 'xref-etags-location
                   :file (expand-file-name file)
                   :pos pos))

  (cl-defmethod xref-location-marker ((l xref-elpy-location))
    (with-current-buffer (find-file-noselect (xref-elpy-location-file l))
      (save-excursion
        (goto-char (xref-elpy-location-pos l))
        (point-marker))))

  (cl-defmethod xref-location-group ((l xref-elpy-location))
    (xref-elpy-location-file l))

  ;; Identifiers
  (cl-defmethod xref-backend-identifier-at-point ((_backend (eql elpy)))
    (elpy-xref--identifier-at-point))

  (defun elpy-xref--identifier-at-point ()
    "Return identifier at point."
    (symbol-at-point))

  (defun elpy-xref--goto-identifier (id)
    "Goto the identifier ID in the current buffer.
This is needed to get information on the identifier with jedi
\(that work only on the symbol at point\)"
    (let ((case-fold-search nil))
      (goto-char (point-min))
      (if (not (search-forward-regexp (format "\\_<%s\\_>" id) nil t))
          (error "Symbol not found on current buffer")
        (goto-char (match-beginning 0)))))

  ;; Find definition
  (cl-defmethod xref-backend-definitions ((_backend (eql elpy)) symbol)
    (elpy-xref--definitions symbol))

  (defun elpy-xref--definitions (symbol)
    "Return SYMBOL definition position as a xref object."
    (save-excursion
      (elpy-xref--goto-identifier symbol)
      (let* ((location (elpy-rpc-get-definition)))
        (if (not location)
            (error "No definition found")
          (let* ((file (expand-file-name (car location)))
                 (pos (+ 1 (cadr location)))
                 (line (with-current-buffer (find-file-noselect file)
                         (goto-char (+ pos 1))
                         (buffer-substring (line-beginning-position) (line-end-position))))
                 (linenumber  (with-current-buffer (find-file-noselect file)
                                (line-number-at-pos pos)))
                 (summary (format elpy-xref--format-references linenumber line))
                 (loc (xref-make-elpy-location file pos)))
            (list (xref-make summary loc)))))))

  ;; Find references
  (cl-defmethod xref-backend-references ((_backend (eql elpy)) symbol)
    (elpy-xref--references symbol))

  (defun elpy-xref--references (symbol)
    "Return SYMBOL references as a list of xref objects."
    (save-excursion
      (elpy-xref--goto-identifier symbol)
      (let* ((references (elpy-rpc-get-usages)))
        (cl-loop
         for ref in references
         for file = (alist-get 'filename ref)
         for pos = (+ (alist-get 'offset ref) 1)
         for line = (with-current-buffer (find-file-noselect file)
                      (save-excursion
                        (goto-char (+ pos 1))
                        (buffer-substring (line-beginning-position) (line-end-position))))
         for linenumber = (with-current-buffer (find-file-noselect file)
                            (line-number-at-pos pos))
         for summary = (format elpy-xref--format-references linenumber line)
         for loc = (xref-make-elpy-location file pos)
         collect (xref-make summary loc)))))

  ;; Completion table (used when calling `xref-find-references`)
  (cl-defmethod xref-backend-identifier-completion-table ((_backend (eql elpy)))
    (elpy-xref--get-completion-table))

  (defun elpy-xref--get-completion-table ()
    "Return the completion table for identifiers."
    (let ((id-at-point (elpy-xref--identifier-at-point))
          (table nil))
      (when id-at-point
        (push id-at-point table))
      (cl-loop
       for ref in (elpy-rpc-get-names)
       for name = (alist-get 'name ref)
       unless (member name table)
       do (push name table))
      table))

  ;; Apropos
  (cl-defmethod xref-backend-apropos ((_backend (eql elpy)) regex)
    (elpy-xref--apropos regex))

  (defun elpy-xref--apropos (regex)
    "Return identifiers matching REGEX."
    (let ((ids (elpy-rpc-get-names))
          (case-fold-search nil))
      (cl-loop
       for id in ids
       for name = (alist-get 'name id)
       for filename = (alist-get 'filename id)
       for pos = (+ (alist-get 'offset id) 1)
       if (string-match regex name)
       collect (with-current-buffer (find-file-noselect filename)
                 (goto-char pos)
                 (save-excursion
                   (let* ((linenumber (line-number-at-pos))
                          (line (buffer-substring
                                 (line-beginning-position)
                                 (line-end-position)))
                          (summary (format elpy-xref--format-references linenumber line))
                          (loc (xref-make-elpy-location filename pos)))
                     (xref-make summary loc)))))))
  )

;;;;;;;;;;;
;;; Modules

(defvar elpy-modules-initialized-p nil
  "Boolean, set to true if modules were run with `global-init'.")

(defun elpy-modules-run (command &rest args)
  "Run COMMAND with ARGS for all modules in `elpy-modules'."
  (dolist (module elpy-modules)
    (apply module command args)))

(defun elpy-modules-global-init ()
  "Run the global-init method of Elpy modules.

Make sure this only happens once."
  (when (not elpy-modules-initialized-p)
    (elpy-modules-run 'global-init)
    (setq elpy-modules-initialized-p t)))

(defun elpy-modules-global-stop ()
  "Run the global-stop method of Elpy modules.

Make sure this only happens once per global-init call."
  (when elpy-modules-initialized-p
    (elpy-modules-run 'global-stop)
    (setq elpy-modules-initialized-p nil)))

(defun elpy-modules-buffer-init ()
  "Run the buffer-init method of Elpy modules.

Make sure global-init is called first."
  (elpy-modules-global-init)
  (elpy-modules-run 'buffer-init))

(defun elpy-modules-buffer-stop ()
  "Run the buffer-stop method of Elpy modules."
  (elpy-modules-run 'buffer-stop))

(defun elpy-modules-remove-modeline-lighter (modename)
  "Remove the lighter for MODENAME.

It should not be necessary to see (Python Elpy yas company ElDoc) all the
time.

If you need your modeline, you can set the variable `elpy-remove-modeline-lighter' to nil
"

  (interactive)
  (when elpy-remove-modeline-lighter
    (cond
     ((eq modename 'eldoc-minor-mode)
      (setq eldoc-minor-mode-string nil))
     (t
      (let ((cell (assq modename minor-mode-alist)))
        (when cell
          (setcdr cell
                  (list ""))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Module: Sane Defaults

(defun elpy-module-sane-defaults (command &rest _args)
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

(defun elpy-module-company (command &rest _args)
  "Module to support company-mode completions."
  (pcase command
    (`global-init
     (require 'company)
     (elpy-modules-remove-modeline-lighter 'company-mode)
     (define-key company-active-map (kbd "C-d")
       'company-show-doc-buffer))
    (`buffer-init
     ;; We want immediate completions from company.
     (set (make-local-variable 'company-idle-delay)
          0.01)
     ;; And annotations should be right-aligned.
     (set (make-local-variable 'company-tooltip-align-annotations)
          t)
     ;; Also, dabbrev in comments and strings is nice.
     (set (make-local-variable 'company-dabbrev-code-everywhere)
          t)
     ;; Add our own backend and remove a bunch of backends that
     ;; interfere in Python mode.
     (set (make-local-variable 'company-backends)
          (cons 'elpy-company-backend
                (delq 'company-semantic
                      (delq 'company-ropemacs
                            (delq 'company-capf
                                  (mapcar #'identity company-backends))))))
     (company-mode 1)
     (when (> (buffer-size) elpy-rpc-ignored-buffer-size)
       (message
	(concat "Buffer %s larger than elpy-rpc-ignored-buffer-size (%d)."
		" Elpy will turn off completion.")
	(buffer-name) elpy-rpc-ignored-buffer-size)))
    (`buffer-stop
     (company-mode -1)
     (kill-local-variable 'company-idle-delay)
     (kill-local-variable 'company-tooltip-align-annotations)
     (kill-local-variable 'company-backends))
    ))

(defvar elpy-company--cache nil
  "Buffer-local cache for candidate information.")
(make-variable-buffer-local 'elpy-company--cache)

(defun elpy-company--cache-clear ()
  "Clear and initialize the cache."
  (if elpy-company--cache
      (clrhash elpy-company--cache)
    (setq elpy-company--cache
          (make-hash-table :test #'equal))))

(defun elpy-company--cache-annotation (name)
  "Return the cached annotation for NAME."
  (when elpy-company--cache
    (cdr (assq 'annotation (gethash name elpy-company--cache)))))

(defun elpy-company--cache-meta (name)
  "Return the cached annotation for NAME."
  (when elpy-company--cache
    (cdr (assq 'meta (gethash name elpy-company--cache)))))

(defun elpy-company--cache-name (name)
  "Return the cached name for NAME.

Confused yet? We pass \"our\" name, that is, prefix + suffix,
here, and return the \"name\" as used by the backend."
  (when elpy-company--cache
    (cdr (assq 'name (gethash name elpy-company--cache)))))

(defun elpy-company--cache-completions (prefix result)
  "Store RESULT in the candidate cache and return candidates."
  (elpy-company--cache-clear)
  (mapcar (lambda (completion)
            (let* ((suffix (cdr (assq 'name completion)))
                   (name (concat (s-chop-suffix (company-grab-symbol) prefix) suffix)))
              (puthash name completion elpy-company--cache)
              name))
          result))

(defun elpy-company--python-exception-p (name)
  "Check whether NAME is a Python exception."
  (member name '("ArithmeticError"
                 "AssertionError"
                 "AttributeError"
                 "BlockingIOError"
                 "BrokenPipeError"
                 "BufferError"
                 "BytesWarning"
                 "ChildProcessError"
                 "ConnectionAbortedError"
                 "ConnectionError"
                 "ConnectionRefusedError"
                 "ConnectionResetError"
                 "DeprecationWarning"
                 "EOFError"
                 "EnvironmentError"
                 "Exception"
                 "FileExistsError"
                 "FileNotFoundError"
                 "FloatingPointError"
                 "FutureWarning"
                 "IOError"
                 "ImportError"
                 "ImportWarning"
                 "IndentationError"
                 "IndexError"
                 "InterruptedError"
                 "IsADirectoryError"
                 "KeyError"
                 "LookupError"
                 "MemoryError"
                 "NameError"
                 "NotADirectoryError"
                 "NotImplementedError"
                 "OSError"
                 "OverflowError"
                 "PendingDeprecationWarning"
                 "PermissionError"
                 "ProcessLookupError"
                 "RecursionError"
                 "ReferenceError"
                 "ResourceWarning"
                 "RuntimeError"
                 "RuntimeWarning"
                 "StandardError"
                 "StopAsyncIteration"
                 "StopIteration"
                 "SyntaxError"
                 "SyntaxWarning"
                 "SystemError"
                 "TabError"
                 "TimeoutError"
                 "TypeError"
                 "UnboundLocalError"
                 "UnicodeDecodeError"
                 "UnicodeEncodeError"
                 "UnicodeError"
                 "UnicodeTranslateError"
                 "UnicodeWarning"
                 "UserWarning"
                 "ValueError"
                 "Warning"
                 "ZeroDivisionError")))

(defun elpy-company-post-complete-parens (annotation name)
  "Complete functions, classes, and callable instances with parentheses.

Add parentheses in case ANNOTATION is \"class\", \"function\", or \"instance\",
unless the completion is already looking at a left parenthesis,
or unless NAME is a Python exception outside a reasonably formed raise statement,
or unless NAME is no callable instance."
  (when (not (looking-at-p "\("))
    (cond ((string= annotation "function")
           (insert "()")
           (backward-char 1))
          ((string= annotation "class")
           (cond ((elpy-company--python-exception-p name)
                  (when (save-excursion
                          (backward-word 2)
                          (looking-at "\\_<raise\\_>"))
                    (insert "()")
                    (backward-char 1)))
                 (t
                  (insert "()")
                  (backward-char 1))))
          ((string= annotation "instance")
           ;; The jedi backend annotates some callables as instances (e.g. numpy
           ;; and scipy) and `elpy-company--cache' does not allow to identify
           ;; callable instances.
           ;; It looks easy to modify `elpy-company--cache' cheaply for the jedi
           ;; backend to eliminate the `elpy-rpc-get-calltip' call below, but
           ;; not for the rope backend.
           (insert "()")
           (backward-char 1)
           (when (not (elpy-rpc-get-calltip))
             (backward-char 1)
             (delete-char 2))))))

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
                (elpy-company--cache-clear)
                (funcall
                 callback
                 (cond
                  ;; The backend returned something
                  (result
                   (elpy-company--cache-completions arg result))
                  ;; Nothing from the backend, try dabbrev-code.
                  ((> (length arg) company-minimum-prefix-length)
                   (elpy--sort-and-strip-duplicates
                    (company-dabbrev-code 'candidates arg)))
                  ;; Well, ok, let's go meh.
                  (t
                   nil))))))))
    ;; sorted => t if the list is already sorted
    (`sorted
     t)
    ;; duplicates => t if there could be duplicates
    (`duplicates
     nil)
    ;; no-cache <prefix> => t if company shouldn't cache results
    ;; meta <candidate> => short docstring for minibuffer
    (`meta
     (let ((meta (elpy-company--cache-meta arg)))
       (when (and meta
                  (string-match "\\`\\(.*\n.*\\)\n.*" meta))
         (setq meta (match-string 1 meta)))
       meta))
    ;; annotation <candidate> => short docstring for completion buffer
    (`annotation
     (elpy-company--cache-annotation arg))
    ;; doc-buffer <candidate> => put doc buffer in `company-doc-buffer'
    (`doc-buffer
     (let* ((name (elpy-company--cache-name arg))
            (doc (when name
                   (elpy-rpc-get-completion-docstring name))))
       (when doc
         (company-doc-buffer doc))))
    ;; require-match => Never require a match, even if the user
    ;; started to interact with company. See `company-require-match'.
    (`require-match
     'never)
    ;; location <candidate> => (buffer . point) or (file .
    ;; line-number)
    (`location
     (let* ((name (elpy-company--cache-name arg))
            (loc (when name
                   (elpy-rpc-get-completion-location name))))
       (when loc
         (cons (car loc)
               (cadr loc)))))
    ;; match <candidate> => for non-prefix based backends
    ;; post-completion <candidate> => after insertion, for snippets
    (`post-completion
     (funcall elpy-company-post-completion-function
              (elpy-company--cache-annotation arg)
              (elpy-company--cache-name arg)))))

(defun elpy--sort-and-strip-duplicates (seq)
  "Sort SEQ and remove any duplicates."
  (sort (delete-dups seq)
        (lambda (a b)
          (string< a b))))

;;;;;;;;;;;;;;;;;
;;; Module: ElDoc

(defun elpy-module-eldoc (command &rest _args)
  "Module to support ElDoc for Python files."
  (pcase command
    (`global-init
     (require 'eldoc)
     (setq eldoc-minor-mode-string nil))
    (`buffer-init
     (set (make-local-variable 'eldoc-documentation-function)
          'elpy-eldoc-documentation)
     (eldoc-mode 1))
    (`buffer-stop
     (eldoc-mode -1)
     (kill-local-variable 'eldoc-documentation-function))))

(defun elpy-eldoc-documentation ()
  "Return some interesting information for the code at point.

This will return flymake errors for the line at point if there
are any. If not, this will do an asynchronous call to the RPC
backend to get a call tip, and display that using
`eldoc-message'. If the backend has no call tip, this will
display the current class and method instead."
  (let ((flymake-error (elpy-flymake-error-at-point)))
    (if flymake-error
        flymake-error
      (elpy-rpc-get-calltip
       (lambda (calltip)
         (eldoc-message
          (cond
           ((not calltip)
            (when elpy-eldoc-show-current-function
              (let ((current-defun (python-info-current-defun)))
                (when current-defun
                  (format "In: %s()" current-defun)))))
           ((stringp calltip)
            calltip)
           (t
            (let ((name (cdr (assq 'name calltip)))
                  (index (cdr (assq 'index calltip)))
                  (params (cdr (assq 'params calltip))))
              (when index
                (setf (nth index params)
                      (propertize (nth index params)
                                  'face
                                  'eldoc-highlight-function-argument)))
              (format "%s(%s)"
                      name
                      (mapconcat #'identity params ", "))
              ))))))
      ;; Return the last message until we're done
      eldoc-last-message)))

;;;;;;;;;;;;;;;;;;;
;;; Module: Flymake

(defun elpy-module-flymake (command &rest _args)
  "Enable Flymake support for Python."
  (pcase command
    (`global-init
     (require 'flymake)
     (elpy-modules-remove-modeline-lighter 'flymake-mode)
     ;; Flymake support using flake8, including warning faces.
     (setq python-check-command elpy-syntax-check-command)

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

     (when (and (buffer-file-name)
                (executable-find python-check-command))
       (flymake-mode 1)))
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

(defun elpy-flymake-next-error ()
  "Move forward to the next Flymake error and show a
description."
  (interactive)
  (flymake-goto-next-error)
  (elpy-flymake-show-error))

(defun elpy-flymake-previous-error ()
  "Move backward to the previous Flymake error and show a
description."
  (interactive)
  (flymake-goto-prev-error)
  (elpy-flymake-show-error))

(defun elpy-flymake-show-error ()
  "Show the flymake error message at point."
  (interactive)
  (let ((error-message (elpy-flymake-error-at-point)))
    (when error-message
      (message "%s" error-message))))

(defun elpy-flymake-error-at-point ()
  "Return the flymake error at point, or nil if there is none."
  (when (boundp 'flymake-err-info)
    (let* ((lineno (line-number-at-pos))
           (err-info (car (flymake-find-err-info flymake-err-info
                                                 lineno))))
      (when err-info
        (mapconcat #'flymake-ler-text
                   err-info
                   ", ")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Module: Highlight Indentation

(defun elpy-module-highlight-indentation (command &rest _args)
  "Module to highlight indentation in Python files."
  (pcase command
    (`global-init
     (require 'highlight-indentation))
    (`buffer-init
     (highlight-indentation-mode 1))
    (`buffer-stop
     (highlight-indentation-mode -1))))

;;;;;;;;;;;;;;;;;;
;;; Module: pyvenv

(defun elpy-module-pyvenv (command &rest _args)
  "Module to display the current virtualenv in the mode line."
  (pcase command
    (`global-init
     (pyvenv-mode 1))
    (`global-stop
     (pyvenv-mode -1))))

;;;;;;;;;;;;;;;;;;;;;
;;; Module: Yasnippet

(defun elpy-module-yasnippet (command &rest _args)
  "Module to enable YASnippet snippets."
  (pcase command
    (`global-init
     (require 'yasnippet)
     (elpy-modules-remove-modeline-lighter 'yas-minor-mode)

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
;;; Module: Elpy-Django

(defun elpy-module-django (command &rest _args)
  "Module to provide Django support."
  (pcase command
    (`buffer-init
     (elpy-django-setup))
    (`buffer-stop
     (elpy-django -1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Backwards compatibility

;; TODO Some/most of this compatibility code can now go, as minimum required
;; Emacs version is 24.4.

;; Functions for Emacs 24 before 24.3
(when (not (fboundp 'python-info-current-defun))
  (defalias 'python-info-current-defun 'python-current-defun))

(when (not (fboundp 'python-nav-forward-statement))
  (defun python-nav-forward-statement (&rest ignored)
    "Function added in Emacs 24.3"
    (error "Enhanced Python navigation only available in Emacs 24.3+")))

(when (not (fboundp 'python-nav-backward-up-list))
  (defun python-nav-backward-up-list ()
    "Compatibility function for older Emacsen"
    (ignore-errors
      (backward-up-list))))

(when (not (fboundp 'python-shell-calculate-exec-path))
  (defun python-shell-calculate-exec-path ()
    "Compatibility function for older Emacsen."
    exec-path))

(when (not (fboundp 'python-shell-calculate-process-environment))
  (defun python-shell-calculate-process-environment ()
    "Compatibility function for older Emacsen."
    process-environment))

(when (not (fboundp 'python-shell-get-process-name))
  (defun python-shell-get-process-name (_dedicated)
    "Compatibility function for older Emacsen."
    "Python"))

(when (not (fboundp 'python-shell-parse-command))
  (defun python-shell-parse-command ()
    "Compatibility function for older Emacsen."
    python-python-command))

(when (not (fboundp 'python-shell-send-buffer))
  (defun python-shell-send-buffer (&optional _arg)
    (python-send-buffer)))

(when (not (fboundp 'python-shell-send-string))
  (defalias 'python-shell-send-string 'python-send-string))

(when (not (fboundp 'python-indent-shift-right))
  (defalias 'python-indent-shift-right 'python-shift-right))

(when (not (fboundp 'python-indent-shift-left))
  (defalias 'python-indent-shift-left 'python-shift-left))

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

;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=25753#44
(when (version< emacs-version "25.2")
  (defun python-shell-completion-native-try ()
    "Return non-nil if can trigger native completion."
    (let ((python-shell-completion-native-enable t)
          (python-shell-completion-native-output-timeout
           python-shell-completion-native-try-output-timeout))
      (python-shell-completion-native-get-completions
       (get-buffer-process (current-buffer))
       nil "_"))))

;; python.el in Emacs 24 does not have functions to determine buffer encoding.
;; In these versions, we fix the encoding to utf-8 (safe choice when no encoding
;; is defined since Python 2 uses ASCII and Python 3 UTF-8).
(unless (fboundp 'python-info-encoding)
  (defun python-info-encoding ()
    'utf-8))

;; Added in Emacs 25
(unless (fboundp 'python-shell-comint-end-of-output-p)
  (defun python-shell-comint-end-of-output-p (output)
    "Return non-nil if OUTPUT is ends with input prompt."
    (string-match
     ;; XXX: It seems on macOS an extra carriage return is attached
     ;; at the end of output, this handles that too.
     (concat
      "\r?\n?"
      ;; Remove initial caret from calculated regexp
      (replace-regexp-in-string
       (rx string-start ?^) ""
       python-shell--prompt-calculated-input-regexp)
      (rx eos))
     output)))

(provide 'elpy)
;;; elpy.el ends here
