;;; elpy.el --- Emacs Python Development Environment -*- lexical-binding: t -*-

;; Copyright (C) 2012-2019  Jorgen Schaefer

;; Author: Jorgen Schaefer <contact@jorgenschaefer.de>, Gaby Launay <gaby.launay@protonmail.com>
;; URL: https://github.com/jorgenschaefer/elpy
;; Version: 1.34.0
;; Keywords: Python, IDE, Languages, Tools
;; Package-Requires: ((company "0.9.10") (emacs "24.4") (highlight-indentation "0.7.0") (pyvenv "1.20") (yasnippet "0.13.0") (s "1.12.0"))

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
(require 'elpy-rpc)
(require 'pyvenv)

(defconst elpy-version "1.34.0"
  "The version of the Elpy Lisp code.")

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
              (const :tag "Code folding"
                     elpy-module-folding)
              (const :tag "Show the virtualenv in the mode line (pyvenv)"
                     elpy-module-pyvenv)
              (const :tag "Display indentation markers (highlight-indentation)"
                     elpy-module-highlight-indentation)
              (const :tag "Expand code snippets (YASnippet)"
                     elpy-module-yasnippet)
              (const :tag "Django configurations (Elpy-Django)"
                     elpy-module-django)
              (const :tag "Automatically update documentation (Autodoc)."
                     elpy-module-autodoc)
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
                     elpy-project-find-svn-root)
              (const :tag "Django project root (manage.py, django-admin.py)"
                     elpy-project-find-django-root))
  :group 'elpy)

(make-obsolete-variable 'elpy-company-hide-modeline
                        'elpy-remove-modeline-lighter
                        "1.10.0")
(defcustom elpy-remove-modeline-lighter t
  "Non-nil if Elpy should remove most mode line display.

Modeline shows many minor modes currently active. For Elpy, this is mostly
uninteresting information, but if you rely on your modeline in other modes,
you might want to keep it."
  :type 'boolean
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

(defcustom elpy-get-info-from-shell nil
  "If t, use the shell to gather docstrings and completions.

Normally elpy provides completion and documentation using static code analysis (from jedi). With this option set to t, elpy will add the completion candidates and the docstrings from the associated python shell; This activates fallback completion candidates for cases when static code analysis fails."
  :type 'boolean
  :group 'elpy)

(defcustom elpy-get-info-from-shell-timeout 1
  "Timeout (in seconds) for gathering information from the shell."
  :type 'number
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
                 (const :tag "Green" elpy-test-green-runner)
                 (const :tag "Django Discover" elpy-test-django-runner)
                 (const :tag "Nose" elpy-test-nose-runner)
                 (const :tag "py.test" elpy-test-pytest-runner)
                 (const :tag "Twisted Trial" elpy-test-trial-runner))
  :safe 'elpy-test-runner-p
  :group 'elpy)

(defcustom elpy-test-discover-runner-command '("python-shell-interpreter" "-m" "unittest")
  "The command to use for `elpy-test-discover-runner'.
If the string \"python-shell-interpreter\" is present, it will be replaced with
the value of `python-shell-interpreter'."
  :type '(repeat string)
  :group 'elpy)

(defcustom elpy-test-green-runner-command '("green")
  "The command to use for `elpy-test-green-runner'."
  :type '(repeat string)
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
  "Non-nil if Elpy should disable backend error display."
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
  "Key map for the refactor command.")

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
    (define-key map (kbd "C-c @ C-c") 'elpy-folding-toggle-at-point)
    (define-key map (kbd "C-c @ C-b") 'elpy-folding-toggle-docstrings)
    (define-key map (kbd "C-c @ C-m") 'elpy-folding-toggle-comments)
    (define-key map (kbd "C-c @ C-f") 'elpy-folding-hide-leafs)
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

    (unless (fboundp 'xref-find-definitions)
        (define-key map (kbd "M-.") 'elpy-goto-definition))
    (if (not (fboundp 'xref-find-definitions-other-window))
        (define-key map (kbd "C-x 4 M-.") 'elpy-goto-definition-other-window)
      (define-key map (kbd "C-x 4 M-.") 'xref-find-definitions-other-window))
    (when (fboundp 'xref-pop-marker-stack)
        (define-key map (kbd "M-*") 'xref-pop-marker-stack))

    (define-key map (kbd "M-TAB") 'elpy-company-backend)

    map)
  "Key map for the Emacs Lisp Python Environment.")

(defvar elpy-shell-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "e") 'elpy-shell-send-statement)
    (define-key map (kbd "E") 'elpy-shell-send-statement-and-go)
    (define-key map (kbd "s") 'elpy-shell-send-top-statement)
    (define-key map (kbd "S") 'elpy-shell-send-top-statement-and-go)
    (define-key map (kbd "f") 'elpy-shell-send-defun)
    (define-key map (kbd "F") 'elpy-shell-send-defun-and-go)
    (define-key map (kbd "c") 'elpy-shell-send-defclass)
    (define-key map (kbd "C") 'elpy-shell-send-defclass-and-go)
    (define-key map (kbd "o") 'elpy-shell-send-group)
    (define-key map (kbd "O") 'elpy-shell-send-group-and-go)
    (define-key map (kbd "w") 'elpy-shell-send-codecell)
    (define-key map (kbd "W") 'elpy-shell-send-codecell-and-go)
    (define-key map (kbd "r") 'elpy-shell-send-region-or-buffer)
    (define-key map (kbd "R") 'elpy-shell-send-region-or-buffer-and-go)
    (define-key map (kbd "b") 'elpy-shell-send-buffer)
    (define-key map (kbd "B") 'elpy-shell-send-buffer-and-go)
    (define-key map (kbd "C-e") 'elpy-shell-send-statement-and-step)
    (define-key map (kbd "C-S-E") 'elpy-shell-send-statement-and-step-and-go)
    (define-key map (kbd "C-s") 'elpy-shell-send-top-statement-and-step)
    (define-key map (kbd "C-S-S") 'elpy-shell-send-top-statement-and-step-and-go)
    (define-key map (kbd "C-f") 'elpy-shell-send-defun-and-step)
    (define-key map (kbd "C-S-F") 'elpy-shell-send-defun-and-step-and-go)
    (define-key map (kbd "C-c") 'elpy-shell-send-defclass-and-step)
    (define-key map (kbd "C-S-C") 'elpy-shell-send-defclass-and-step-and-go)
    (define-key map (kbd "C-o") 'elpy-shell-send-group-and-step)
    (define-key map (kbd "C-S-O") 'elpy-shell-send-group-and-step-and-go)
    (define-key map (kbd "C-w") 'elpy-shell-send-codecell-and-step)
    (define-key map (kbd "C-S-W") 'elpy-shell-send-codecell-and-step-and-go)
    (define-key map (kbd "C-r") 'elpy-shell-send-region-or-buffer-and-step)
    (define-key map (kbd "C-S-R") 'elpy-shell-send-region-or-buffer-and-step-and-go)
    (define-key map (kbd "C-b") 'elpy-shell-send-buffer-and-step)
    (define-key map (kbd "C-S-B") 'elpy-shell-send-buffer-and-step-and-go)
    map)
  "Key map for the shell related commands.")
(fset 'elpy-shell-map elpy-shell-map)

(defcustom elpy-shell-command-prefix-key "C-c C-y"
  "Prefix key used to call elpy shell related commands.

This option need to bet set through `customize' or `customize-set-variable' to be taken into account."
  :type 'string
  :group 'elpy
  :set
  (lambda (var key)
    (when (and (boundp var) (symbol-value var))
      (define-key elpy-mode-map (kbd (symbol-value var)) nil))
    (when key
      (define-key elpy-mode-map (kbd key) 'elpy-shell-map)
      (set var key))))

(defvar elpy-pdb-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d") 'elpy-pdb-debug-buffer)
    (define-key map (kbd "p") 'elpy-pdb-break-at-point)
    (define-key map (kbd "e") 'elpy-pdb-debug-last-exception)
    (define-key map (kbd "b") 'elpy-pdb-toggle-breakpoint-at-point)
    map)
  "Key map for the shell related commands.")
(fset 'elpy-pdb-map elpy-pdb-map)
(define-key elpy-mode-map (kbd "C-c C-u") 'elpy-pdb-map)

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
     ["Send Definition" elpy-shell-send-defun
      :help "Send current definition to Python"]
     ["Kill Python shell" elpy-shell-kill
      :help "Kill the current Python shell"]
     ["Kill all Python shells" elpy-shell-kill-all
      :help "Kill all Python shells"])
    ("Debugging"
     ["Debug buffer" elpy-pdb-debug-buffer
      :help "Debug the current buffer using pdb"]
     ["Debug at point" elpy-pdb-break-at-point
      :help "Debug the current buffer and stop at the current position"]
     ["Debug last exception" elpy-pdb-debug-last-exception
      :help "Run post-mortem pdb on the last exception"]
     ["Add/remove breakpoint" elpy-pdb-toggle-breakpoint-at-point
      :help "Add or remove a breakpoint at the current position"])
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
    ("Code folding"
     ["Hide/show at point" elpy-folding-toggle-at-point
      :help "Hide or show the block or docstring at point"]
     ["Hide/show all docstrings" elpy-folding-toggle-docstrings
      :help "Hide or show all the docstrings"]
     ["Hide/show all comments" elpy-folding-toggle-comments
      :help "Hide or show all the comments"]
     ["Hide leafs" elpy-folding-hide-leafs
      :help "Hide all leaf blocks (blocks not containing other blocks)"])
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
    ["Configure" elpy-config t]))

(defvar elpy-enabled-p nil
  "Is Elpy enabled or not.")

;;;###autoload
(defun elpy-enable (&optional _ignored)
  "Enable Elpy in all future Python buffers."
  (interactive)
  (unless elpy-enabled-p
    (when (< emacs-major-version 24)
      (error "Elpy requires Emacs 24 or newer"))
    (when _ignored
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
    (add-hook 'pyvenv-post-deactivate-hooks 'elpy-rpc--disconnect)
    (add-hook 'inferior-python-mode-hook 'elpy-shell--enable-output-filter)
    (add-hook 'python-shell-first-prompt-hook 'elpy-shell--send-setup-code t)
    ;; Enable Elpy-mode in the opened python buffer
    (setq elpy-enabled-p t)
    (dolist (buffer (buffer-list))
      (and (not (string-match "^ ?\\*" (buffer-name buffer)))
           (with-current-buffer buffer
             (when (string= major-mode 'python-mode)
               (elpy-mode t)))))
    ))

(defun elpy-disable ()
  "Disable Elpy in all future Python buffers."
  (interactive)
  (elpy-modules-global-stop)
  (define-key inferior-python-mode-map (kbd "C-c C-z") nil)
  (remove-hook 'python-mode-hook 'elpy-mode)
  (remove-hook 'pyvenv-post-activate-hooks 'elpy-rpc--disconnect)
  (remove-hook 'pyvenv-post-deactivate-hooks 'elpy-rpc--disconnect)
  (remove-hook 'inferior-python-mode-hook 'elpy-shell--enable-output-filter)
  (remove-hook 'python-shell-first-prompt-hook 'elpy-shell--send-setup-code)
  (setq elpy-enabled-p nil))

;;;###autoload
(define-minor-mode elpy-mode
  "Minor mode in Python buffers for the Emacs Lisp Python Environment.

This mode fully supports virtualenvs. Once you switch a
virtualenv using \\[pyvenv-workon], you can use
\\[elpy-rpc-restart] to make the elpy Python process use your
virtualenv.

\\{elpy-mode-map}"
  :lighter " Elpy"
  (unless (derived-mode-p 'python-mode)
    (error "Elpy only works with `python-mode'"))
  (unless elpy-enabled-p
    (error "Please enable Elpy with `(elpy-enable)` before using it"))
  (when (boundp 'xref-backend-functions)
    (add-hook 'xref-backend-functions #'elpy--xref-backend nil t))
  (cond
   (elpy-mode
    (elpy-modules-buffer-init))
   ((not elpy-mode)
    (elpy-modules-buffer-stop))))

;;;;;;;;;;;;;;;
;;; Elpy Config

(defvar elpy-config--related-custom-groups
  '(("Elpy" elpy "elpy-")
    ("Python" python "python-")
    ("Virtual Environments (Pyvenv)" pyvenv "pyvenv-")
    ("Completion (Company)" company "company-")
    ("Call Signatures (ElDoc)" eldoc "eldoc-")
    ("Inline Errors (Flymake)" flymake "flymake-")
    ("Code folding (hideshow)" hideshow "hs-")
    ("Snippets (YASnippet)" yasnippet "yas-")
    ("Directory Grep (rgrep)" grep "grep-")
    ("Search as You Type (ido)" ido "ido-")
    ("Django extension" elpy-django "elpy-django-")
    ("Autodoc extension" elpy-autodoc "elpy-autodoc-")
    ;; ffip does not use defcustom
    ;; highlight-indent does not use defcustom, either. Its sole face
    ;; is defined in basic-faces.
    ))

(defvar elpy-config--get-config "import json
import sys
from distutils.version import LooseVersion
import warnings
warnings.filterwarnings('ignore', category=FutureWarning)

try:
    import urllib2 as urllib
except ImportError:
    import urllib.request as urllib


# Check if we can connect to pypi quickly enough
try:
    response = urllib.urlopen('https://pypi.org/pypi', timeout=1)
    CAN_CONNECT_TO_PYPI = True
except:
    CAN_CONNECT_TO_PYPI = False


def latest(package, version=None):
    if not CAN_CONNECT_TO_PYPI:
        return None
    try:
        response = urllib.urlopen('https://pypi.org/pypi/{package}/json'.format(package=package),
               timeout=2).read()
        latest = json.loads(response)['info']['version']
        if version is None or LooseVersion(version) < LooseVersion(latest):
            return latest
        else:
            return None
    except:
        return None


config = {}
config['can_connect_to_pypi'] = CAN_CONNECT_TO_PYPI
config['rpc_python_version'] = ('{major}.{minor}.{micro}'
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

try:
    import black
    config['black_version'] = black.__version__
    config['black_latest'] = latest('black', config['black_version'])
except:
    config['black_version'] = None
    config['black_latest'] = latest('black')

json.dump(config, sys.stdout)
")

(defun elpy-config-error (&optional fmt &rest args)
  "Note a configuration problem.

FMT is the formating string.

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

      (elpy-config--insert-configuration-table config)
      (insert "\n")

      (elpy-insert--header "Warnings")

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
  "Insert the customization help."
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
  (unless config
    (setq config (elpy-config--get-config)))
  (let* ((rpc-python-version (gethash "rpc_python_version" config))
         (rope-pypi-package  (if (and rpc-python-version
                                      (string-match "^3\\." rpc-python-version))
                                 "rope_py3k"
                               "rope")))

    ;; Python not found
    (unless (gethash "rpc_python_executable" config)
      (elpy-insert--para
       "Elpy can not find the configured Python interpreter. Please make "
       "sure that the variable `elpy-rpc-python-command' points to a "
       "command in your PATH. You can change the variable below.\n\n"))

    ;; No virtual env
    (when (and (gethash "rpc_python_executable" config)
               (not (gethash "virtual_env" config)))
      (elpy-insert--para
       "You have not activated a virtual env. While Elpy supports this, "
       "it is often a good idea to work inside a virtual env. You can use "
       "M-x pyvenv-activate or M-x pyvenv-workon to activate a virtual "
       "env.\n\n"))

    ;; No virtual env, but ~/.local/bin not in PATH
    (when (and (not (memq system-type '(ms-dos windows-nt)))
               (gethash "rpc_python_executable" config)
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
       "PATH -- and then do `elpy-rpc-restart'.\n\n"))

    ;; Python found, but can't find the elpy module
    (when (and (gethash "rpc_python_executable" config)
               (not (gethash "elpy_version" config)))
      (elpy-insert--para
       "The Python interpreter could not find the elpy module. "
       "Please report to: "
       "https://github.com/jorgenschaefer/elpy/issues/new."
       "\n")
      (insert "\n"))

    ;; Bad backend version
    (when (and (gethash "elpy_version" config)
               (not (equal (gethash "elpy_version" config)
                           elpy-version)))
      (let ((elpy-python-version (gethash "elpy_version" config)))
        (elpy-insert--para
         "The Elpy backend is version " elpy-python-version " while "
         "the Emacs package is " elpy-version ". This is incompatible. "
         "Please report to: https://github.com/jorgenschaefer/elpy/issues/new."
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

    ;; Interactive python interpreter not in the current virtual env
    (when (and pyvenv-virtual-env
               (not (string-prefix-p (expand-file-name pyvenv-virtual-env)
                                     (executable-find
                                      python-shell-interpreter))))
      (elpy-insert--para
       "The python interactive interpreter (" python-shell-interpreter
       ") is not installed on the current virtualenv ("
       pyvenv-virtual-env "). The system binary ("
       (executable-find python-shell-interpreter)
       ") will be used instead."
       "\n")
      (insert "\n")
      (widget-create 'elpy-insert--pip-button
                     :package python-shell-interpreter :norpc t)
      (insert "\n\n"))

    ;; Couldn't connect to pypi to check package versions
    (when (not (gethash "can_connect_to_pypi" config))
      (elpy-insert--para
       "Elpy could not connect to Pypi (or at least not quickly enough) "
       "and check if the python packages were up-to-date. "
       "You can still try to update all of them:"
       "\n")
      (insert "\n")
      (widget-create 'elpy-insert--generic-button
                     :button-name "[Update python packages]"
                     :function (lambda () (with-elpy-rpc-virtualenv-activated
                                        (elpy-rpc--install-dependencies))))
      (insert "\n\n"))

    ;; Pip not available in the rpc virtualenv
    (when (and
           (equal elpy-rpc-virtualenv-path 'default)
           (elpy-rpc--pip-missing))
         (elpy-insert--para
          "Pip doesn't seem to be installed in the dedicated virtualenv "
          "created by Elpy (" (elpy-rpc-get-virtualenv-path) "). "
          "This may prevent some features from working properly"
          " (completion, documentation, reformatting, ...). "
          "You can try reinstalling the virtualenv. "
          "If the problem persists, please report on Elpy's github page."
          "\n\n")
      (widget-create 'elpy-insert--generic-button
                     :button-name "[Reinstall RPC virtualenv]"
                     :function (lambda () (elpy-rpc-reinstall-virtualenv)))
      (insert "\n\n"))

    ;; Requested backend unavailable
    (when (and (gethash "rpc_python_executable" config)
               (not (gethash "jedi_version" config)))
      (elpy-insert--para
       "The jedi package is not available. Completion and code navigation will"
       " not work.\n")
      (insert "\n")
      (widget-create 'elpy-insert--pip-button
                     :package "jedi")
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


    ;; No autopep8 available
    (unless (gethash "autopep8_version" config)
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
    (unless (gethash "yapf_version" config)
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

    ;; No black available
    (unless (gethash "black_version" config)
      (elpy-insert--para
       "The black package is not available. Commands using this will "
       "not work.\n")
      (insert "\n")
      (widget-create 'elpy-insert--pip-button
                     :package "black")
      (insert "\n\n"))

    ;; Newer version of black available
    (when (and (gethash "black_version" config)
               (gethash "black_latest" config))
      (elpy-insert--para
       "There is a newer version of the black package available.\n")
      (insert "\n")
      (widget-create 'elpy-insert--pip-button
                     :package "black" :upgrade t)
      (insert "\n\n"))

    ;; Syntax checker not available
    (unless (executable-find (car (split-string elpy-syntax-check-command)))
      (elpy-insert--para
       "The configured syntax checker could not be found. Elpy uses this "
       "program to provide syntax checks of your programs, so you might "
       "want to install one. Elpy by default uses flake8.\n")
      (insert "\n")
      (widget-create 'elpy-insert--pip-button :package "flake8" :norpc t)
      (insert "\n\n"))

    ))

(defun elpy-config--package-available-p (package)
  "Check if PACKAGE is installed in the rpc."
  (with-elpy-rpc-virtualenv-activated
   (equal 0 (call-process elpy-rpc-python-command nil nil nil "-c"
                          (format "import %s" package)))))

(defun elpy-config--get-config ()
  "Return the configuration from `elpy-rpc-python-command'.

This returns a hash table with the following keys (all strings):

emacs_version
elpy_version
python_interactive
python_interactive_version
python_interactive_executable
rpc_virtualenv
rpc_virtualenv_short
rpc_python
rpc_python_version
rpc_python_executable
jedi_version
rope_version
virtual_env
virtual_env_short"
  (with-temp-buffer
    (let ((config (make-hash-table :test #'equal)))
      (puthash "emacs_version" emacs-version config)
      (let ((rpc-venv (elpy-rpc-get-or-create-virtualenv)))
        (puthash "rpc_virtualenv" rpc-venv config)
        (if rpc-venv
            (puthash "rpc_virtualenv_short"
                     (file-name-nondirectory (directory-file-name rpc-venv))
                       config)
          (puthash "rpc_virtualenv_short" nil config)))
      (with-elpy-rpc-virtualenv-activated
       (puthash "rpc_python" elpy-rpc-python-command config)
       (puthash "rpc_python_executable"
                (executable-find elpy-rpc-python-command)
                config))
      (let ((interactive-python (if (boundp 'python-python-command)
                                    python-python-command
                                  python-shell-interpreter)))
        (puthash "python_interactive"
                 interactive-python
                 config)
        (puthash "python_interactive_version"
                 (let ((pversion (shell-command-to-string
                                  (format "%s --version"
                                          python-shell-interpreter))))
                   (when (string-match "[0-9.]+" pversion)
                     (match-string 0 pversion)))
                 config)
        (puthash "python_interactive_executable"
                 (executable-find interactive-python)
                 config))
      (let ((venv (getenv "VIRTUAL_ENV")))
        (puthash "virtual_env" venv config)
        (if venv
            (puthash "virtual_env_short" (file-name-nondirectory
                                          (directory-file-name venv))
                     config)
          (puthash "virtual_env_short" nil config)))
      (with-elpy-rpc-virtualenv-activated
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
                         (let ((json-array-type 'list)
                               (json-false nil)
                               (json-encoding-pretty-print nil))  ;; Link to bug https://github.com/jorgenschaefer/elpy/issues/1521
                           (goto-char (point-min))
                           (json-read)))))
             (if (not data)
                 (puthash "error_output" (buffer-string) config)
               (dolist (pair data)
                 (puthash (symbol-name (car pair)) (cdr pair) config)))))))
      config)))

(defun elpy-config--insert-configuration-table (&optional config)
  "Insert a table describing the current Elpy config."
  (unless config
    (setq config (elpy-config--get-config)))
  (let ((emacs-version (gethash "emacs_version" config))
        (elpy-python-version (gethash "elpy_version" config))
        (virtual-env (gethash "virtual_env" config))
        (virtual-env-short (gethash "virtual_env_short" config))
        (python-interactive (gethash "python_interactive" config))
        (python-interactive-version (gethash "python_interactive_version" config))
        (python-interactive-executable (gethash "python_interactive_executable"
                                                config))
        (rpc-python (gethash "rpc_python" config))
        (rpc-python-executable (gethash "rpc_python_executable" config))
        (rpc-python-version (gethash "rpc_python_version" config))
        (rpc-virtualenv (gethash "rpc_virtualenv" config))
        (rpc-virtualenv-short (gethash "rpc_virtualenv_short" config))
        (jedi-version (gethash "jedi_version" config))
        (jedi-latest (gethash "jedi_latest" config))
        (rope-version (gethash "rope_version" config))
        (rope-latest (gethash "rope_latest" config))
        (autopep8-version (gethash "autopep8_version" config))
        (autopep8-latest (gethash "autopep8_latest" config))
        (yapf-version (gethash "yapf_version" config))
        (yapf-latest (gethash "yapf_latest" config))
        (black-version (gethash "black_version" config))
        (black-latest (gethash "black_latest" config))
        table maxwidth)
    (setq table
          `(("Emacs" . ,emacs-version)
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
            (("Virtualenv" (lambda ()
                             (call-interactively 'pyvenv-workon)
                             (elpy-config)))
                            . ,(if (gethash "virtual_env" config)
                                 (format "%s (%s)"
                                         virtual-env-short
                                         virtual-env)
                               "None"))
            (("Interactive Python" (lambda ()
                                     (customize-variable
                                      'python-shell-interpreter)))
             . ,(cond
                 (python-interactive-executable
                  (format "%s %s (%s)"
                          python-interactive
                          python-interactive-version
                          python-interactive-executable))
                 (python-interactive
                  (format "%s (not found)"
                          python-interactive))
                 (t
                  "Not configured")))
            ("RPC virtualenv"
             . ,(format "%s (%s)"
                        (if (or (eq elpy-rpc-virtualenv-path 'system)
                                (eq elpy-rpc-virtualenv-path 'global))  ;; for backward compatibility
                            "system"
                          rpc-virtualenv-short)
                        rpc-virtualenv))
            ((" Python" (lambda ()
                             (customize-variable
                              'elpy-rpc-python-command)))
             . ,(cond
                 (rpc-python-executable
                  (format "%s %s (%s)"
                          rpc-python
                          rpc-python-version
                          rpc-python-executable))
                 (rpc-python-executable
                  rpc-python-executable)
                 (rpc-python
                  (format "%s (not found)" rpc-python))
                 (t
                  (format "Not configured"))))
            (" Jedi" . ,(elpy-config--package-link "jedi"
                                                  jedi-version
                                                  jedi-latest))
            (" Rope" . ,(elpy-config--package-link "rope"
                                                  rope-version
                                                  rope-latest))
            (" Autopep8" . ,(elpy-config--package-link "autopep8"
                                                      autopep8-version
                                                      autopep8-latest))
            (" Yapf" . ,(elpy-config--package-link "yapf"
                                                  yapf-version
                                                  yapf-latest))
            (" Black" . ,(elpy-config--package-link "black"
                                                   black-version
                                                   black-latest))
            (("Syntax checker" (lambda ()
                                 (customize-variable 'elpy-syntax-check-command)))

              . ,(let ((syntax-checker
                                        (executable-find
                                         (car (split-string
                                               elpy-syntax-check-command)))))
                                   (if  syntax-checker
                                       (format "%s (%s)"
                                               (file-name-nondirectory
                                                syntax-checker)
                                               syntax-checker)
                                     (format "Not found (%s)"
                                             elpy-syntax-check-command))))))
    (setq maxwidth 0)
    (dolist (row table)
      (let (length)
        (if (stringp (car row))
            (setq length (length (car row)))
          (setq length (length (car (car row)))))
        (when (> length maxwidth)
          (setq maxwidth length ))))
    (dolist (row table)
      (if (stringp (car row))
          (insert (car row)
                  (make-string (- maxwidth (length (car row)))
                               ?.))
        (widget-create 'elpy-insert--generic-button
                       :button-name (car (car row))
                       :function (car (cdr (car row))))
        (insert (make-string (- maxwidth (length (car (car row))))
                     ?.)))
      (insert ": "
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

(defun elpy-insert--para (&rest messages)
  "Insert MESSAGES, a list of strings, and then fill it."
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

(define-widget 'elpy-insert--generic-button 'item
  "A button that run a rgiven function."
  :button-prefix ""
  :button-suffix ""
  :format "%[%v%]"
  :value-create 'elpy-insert--generic-button-value-create
  :action 'elpy-insert--generic-button-action)

(defun elpy-insert--generic-button-value-create (widget)
  "The :value-create option for the customize button widget."
  (insert (widget-get widget :button-name)))

(defun elpy-insert--generic-button-action (widget &optional _event)
  "The :action option for the customize button widget."
  (funcall (widget-get widget :function)))

(define-widget 'elpy-insert--pip-button 'item
  "A button that runs pip (or an alternative)."
  :button-prefix "["
  :button-suffix "]"
  :format "%[%v%]"
  :value-create 'elpy-insert--pip-button-value-create
  :action 'elpy-insert--pip-button-action)

(defun elpy-insert--pip-button-value-create (widget)
  "The :value-create option for the pip button widget."
  (let* ((python-package (widget-get widget :package))
         (do-upgrade (widget-get widget :upgrade))
         (upgrade-option (if do-upgrade
                             "--upgrade "
                           ""))
         (command (cond
                   ((= (call-process elpy-rpc-python-command
                                     nil nil nil
                                     "-m" "pip" "--help")
                       0)
                    (format "%s -m pip install %s%s"
                            elpy-rpc-python-command
                            upgrade-option
                            python-package))
                   ((executable-find "easy_install")
                    (format "easy_install %s" python-package))
                   (t
                    (error "Neither easy_install nor pip found")))))
    (widget-put widget :command command)
    (if do-upgrade
        (insert (format "Update %s" python-package))
      (insert (format "Install %s" python-package)))))

(defun elpy-insert--pip-button-action (widget &optional _event)
  "The :action option for the pip button widget."
  (let ((command (widget-get widget :command))
        (norpc (widget-get widget :norpc)))
    (if norpc
        (async-shell-command command)
      (with-elpy-rpc-virtualenv-activated
       (async-shell-command command)))))

;;;;;;;;;;;;
;;; Projects

(defvar elpy-project--variable-name-history nil
  "The history for `elpy-project--read-project-variable'.")

(defun elpy-project-root ()
  "Return the root of the current buffer's project.

This can very well be nil if the current file is not part of a
project.

See `elpy-project-root-finder-functions' for a way to configure
how the project root is found. You can also set the variable
`elpy-project-root' in, for example, .dir-locals.el to override
this."
  (unless elpy-project-root
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

This is marked with 'setup.py', 'setup.cfg' or 'pyproject.toml'."
  (or (locate-dominating-file default-directory "setup.py")
      (locate-dominating-file default-directory "setup.cfg")
      (locate-dominating-file default-directory "pyproject.toml")))

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
  "Prompt the user for a variable name to set project-wide using PROMPT."
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
  "Read the value for VARIABLE from the user using PROMPT."
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
    (mapcar (lambda (ext) (if (s-ends-with? "/" ext)
                              (concat "*" ext "*")
                            (concat "*" ext)))
            completion-ignored-extensions)
    (cl-copy-list elpy-ffip-prune-patterns)
    (cl-copy-list ffip-prune-patterns))))

(defun elpy-find-file (&optional dwim)
  "Efficiently find a file in the current project.

It necessitates `projectile' or `find-file-in-project' to be installed.

With prefix argument (or DWIM non-nil), tries to guess what kind of
file the user wants to open:
- On an import line, it opens the file of that module.
- Otherwise, it opens a test file associated with the current file,
if one exists. A test file is named test_<name>.py if the current
file is <name>.py, and is either in the same directory or a
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
   ((fboundp 'projectile-find-file)
    (let ((projectile-globally-ignored-file-suffixes
           (delete-dups
            (nconc
             (cl-copy-list projectile-globally-ignored-file-suffixes)
             (cl-copy-list completion-ignored-extensions))))
          (projectile-globally-ignored-directories
           (delete-dups
            (nconc
             (cl-copy-list projectile-globally-ignored-directories)
             (cl-copy-list elpy-ffip-prune-patterns)
             (elpy-project-ignored-directories))))
          (projectile-project-root (or (elpy-project-root)
                                       default-directory)))
      (projectile-find-file)))
   ((fboundp 'find-file-in-project)
    (let ((ffip-prune-patterns (elpy-ffip-prune-patterns))
          (ffip-project-root (or (elpy-project-root)
                                 default-directory))
          ;; Set up ido to use vertical file lists.
          (ffip-prefer-ido-mode t)
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
      (find-file-in-project)))
   (t
    (error "`elpy-find-file' necessitates `projectile' or `find-file-in-project' to be installed"))))

(defun elpy-find--test-file ()
  "Return the test file for the current file, if any.

If this is a test file, return the non-test file.

A test file is named test_<name>.py if the current file is
<name>.py, and is either in the same directors or a \"test\" or
\"tests\" subdirectory."
  (let* ((project-root (or (elpy-project-root) default-directory))
         (filename (file-name-base (buffer-file-name)))
         (impl-file (when (string-match "test_\\(.*\\)" filename)
                      (match-string 1 filename)))
         (files
          (cond
           ((fboundp 'projectile-find-file)
            (let ((projectile-project-root project-root))
              (projectile-current-project-files)))
           ((fboundp 'find-file-in-project)
            (cl-map 'list 'cdr (ffip-project-search nil nil project-root)))
           (t '())))
         (file (if impl-file
                    (cl-remove-if (lambda (file)
                                    (not (string=
                                          impl-file
                                          (file-name-base file))))
                                  files)
                  (cl-remove-if (lambda (file)
                                  (not (string=
                                        (format "test_%s" filename)
                                        (file-name-base file))))
                                files))))
    (when file
      (if (> (length file) 1)
          (setq file (completing-read "Which file: " file))
        (setq file (car file)))
      (if (file-name-absolute-p file)
          file
        (concat (file-name-as-directory project-root) file)))))

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
  (unless (buffer-file-name)
    (error "Can't check a buffer without a file"))
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

(defun elpy-rpc-warn-if-jedi-not-available ()
  "Display a warning if jedi is not available in the current RPC."
  (unless elpy-rpc--jedi-available
    (error "This feature requires the `jedi` package to be installed. Please check `elpy-config` for more information.")))

(defun elpy-goto-definition ()
  "Go to the definition of the symbol at point, if found."
  (interactive)
  (elpy-rpc-warn-if-jedi-not-available)
  (let ((location (elpy-rpc-get-definition)))
    (if location
        (elpy-goto-location (car location) (cadr location))
      (error "No definition found"))))

(defun elpy-goto-assignment ()
  "Go to the assignment of the symbol at point, if found."
  (interactive)
  (elpy-rpc-warn-if-jedi-not-available)
  (let ((location (elpy-rpc-get-assignment)))
    (if location
        (elpy-goto-location (car location) (cadr location))
      (error "No assignment found"))))

(defun elpy-goto-definition-other-window ()
  "Go to the definition of the symbol at point in other window, if found."
  (interactive)
  (elpy-rpc-warn-if-jedi-not-available)
  (let ((location (elpy-rpc-get-definition)))
    (if location
        (elpy-goto-location (car location) (cadr location) 'other-window)
      (error "No definition found"))))

(defun elpy-goto-assignment-other-window ()
  "Go to the assignment of the symbol at point in other window, if found."
  (interactive)
  (elpy-rpc-warn-if-jedi-not-available)
  (let ((location (elpy-rpc-get-assignment)))
    (if location
        (elpy-goto-location (car location) (cadr location) 'other-window)
      (error "No assignment found"))))

(defun elpy-goto-location (filename offset &optional other-window-p)
  "Show FILENAME at OFFSET to the user.

If OTHER-WINDOW-P is non-nil, show the same in other window."
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
  "Move the current line vertically in direction DIR."
  (let* ((beg (point-at-bol))
         (end (point-at-bol 2))
         (col (current-column))
         (region (delete-and-extract-region beg end)))
    (forward-line dir)
    (save-excursion
      (insert region))
    (goto-char (+ (point) col))))

(defun elpy--nav-move-region-vertically (beg end dir)
  "Move the current region vertically in direction DIR."
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
        (progn
          (push-mark (point))
          (push-mark (point-max) nil t)
          (goto-char (point-min)))
      (while (<= indentation (current-indentation))
        (forward-line -1))
      (forward-line 1)
      (push-mark (point) nil t)
      (while (<= indentation (current-indentation))
        (forward-line 1))
      (backward-char))))

(defadvice keyboard-quit (before collapse-region activate)
  "Abort elpy selection by indentation on quit."
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
    (unless (= (point) (line-beginning-position))
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
  "Return the name of the test at point."
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

(defun elpy-test-get-discover-runner ()
  "Return the test discover runner from `elpy-test-discover-runner-command'."
  (cl-loop
   for string in elpy-test-discover-runner-command
   if (string= string "python-shell-interpreter")
   collect python-shell-interpreter
   else
   collect string))

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
           (append (elpy-test-get-discover-runner)
                   (list test)))))
(put 'elpy-test-discover-runner 'elpy-test-runner-p t)

(defun elpy-test-green-runner (top _file module test)
  "Test the project using the green runner."
  (interactive (elpy-test-at-point))
  (let* ((test (cond
                (test (format "%s.%s" module test))
                (module module)))
         (command (if test
                      (append elpy-test-green-runner-command (list test))
                    elpy-test-green-runner-command)))
    (apply #'elpy-test-run top command)))
(put 'elpy-test-green-runner 'elpy-test-runner-p t)

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
    (unless current-prefix-arg
      (setq doc (elpy-rpc-get-docstring))
      (unless doc
        (save-excursion
          (python-nav-backward-up-list)
          (setq doc (elpy-rpc-get-docstring))))
      (unless doc
        (setq doc (elpy-rpc-get-pydoc-documentation
                   (elpy-doc--symbol-at-point))))
      (unless doc
        (save-excursion
          (python-nav-backward-up-list)
          (setq doc (elpy-rpc-get-pydoc-documentation
                     (elpy-doc--symbol-at-point))))))
    (unless doc
      (setq doc (elpy-rpc-get-pydoc-documentation
                 (elpy-doc--read-identifier-from-minibuffer
                  (elpy-doc--symbol-at-point)))))
    (if doc
        (elpy-doc--show doc)
      (error "No documentation found"))))

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

;;;;;;;;;;;;;;;;;;;;;
;;; Importmagic - make obsolete

(defun elpy-importmagic-add-import ()
  (interactive))
(defun elpy-importmagic-fixup ()
  (interactive))

(make-obsolete 'elpy-importmagic-add-import "support for importmagic has been dropped." "1.17.0")
(make-obsolete 'elpy-importmagic-fixup "support for importmagic has been dropped." "1.17.0")

;;;;;;;;;;;;;;;;;;;;;
;;; Code reformatting

(defun elpy-format-code ()
  "Format code using the available formatter."
  (interactive)
  (cond
   ((elpy-config--package-available-p "yapf")
    (when (interactive-p) (message "Autoformatting code with yapf."))
    (elpy-yapf-fix-code))
   ((elpy-config--package-available-p "autopep8")
    (when (interactive-p) (message "Autoformatting code with autopep8."))
    (elpy-autopep8-fix-code))
   ((elpy-config--package-available-p "black")
    (when (interactive-p) (message "Autoformatting code with black."))
    (elpy-black-fix-code))
   (t
    (message "Install yapf/autopep8 to format code."))))

(defun elpy-yapf-fix-code ()
  "Automatically formats Python code with yapf.

Yapf can be configured with a style file placed in the project
root directory."
  (interactive)
  (elpy--fix-code-with-formatter "fix_code_with_yapf"))

(defun elpy-autopep8-fix-code ()
  "Automatically formats Python code to conform to the PEP 8 style guide.

Autopep8 can be configured with a style file placed in the project
root directory."
  (interactive)
  (elpy--fix-code-with-formatter "fix_code"))

(defun elpy-black-fix-code ()
  "Automatically formats Python code with black."
  (interactive)
  (elpy--fix-code-with-formatter "fix_code_with_black"))

(defun elpy--fix-code-with-formatter (method)
  "Common routine for formatting python code."
  (let ((line (line-number-at-pos))
        (col (current-column))
        (directory (if (elpy-project-root)
                       (expand-file-name (elpy-project-root))
                     default-directory)))
    (if (use-region-p)
        (let ((new-block (elpy-rpc method
                                   (list (elpy-rpc--region-contents)
                                         directory)))
              (beg (region-beginning))
              (end (region-end)))
          (elpy-buffer--replace-region
           beg end (string-trim-right new-block))
          (goto-char end)
          (deactivate-mark))
      (let ((new-block (elpy-rpc method
                                 (list (elpy-rpc--buffer-contents)
                                       directory)))
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
                     (unless (member key filenames)
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
    (occur "^\s*\\(\\(async\s\\|\\)def\\|class\\)\s"))
  (let ((window (get-buffer-window "*Occur*")))
    (if window
        (select-window window)
      (switch-to-buffer "*Occur*"))))

;;;;;;;;;;;;;;;;
;;; Xref backend

(defun elpy--xref-backend ()
  "Return the name of the elpy xref backend."
  ;; If no rpc available, start one and assume jedi is available
  (if (or (and (not (elpy-rpc--process-buffer-p elpy-rpc--buffer))
               (elpy-rpc--get-rpc-buffer))
          elpy-rpc--jedi-available)
      'elpy
    nil))

(defvar elpy-xref--format-references
  (let ((str "%s:\t%s"))
    (put-text-property 0 4 'face 'compilation-line-number str)
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
    "Return identifier at point.

Is a string, formatted as \"LINE_NUMBER: VARIABLE_NAME\".
"
    (let* ((symb  (symbol-at-point))
           (symb-str (substring-no-properties (symbol-name symb))))
      (when symb
        (format "%s: %s" (line-number-at-pos) symb-str))))

  (defun elpy-xref--identifier-name (id)
    "Return the identifier ID variable name."
    (string-match ".*: \\([^:]*\\)" id)
    (match-string 1 id))

  (defun elpy-xref--identifier-line (id)
    "Return the identifier ID line number."
    (string-match "\\([^:]*\\):.*" id)
    (string-to-number (match-string 1 id)))

  (defun elpy-xref--goto-identifier (id)
    "Goto the identifier ID in the current buffer.
This is needed to get information on the identifier with jedi
\(that work only on the symbol at point\)"
    (let ((case-fold-search nil))
      (goto-char (point-min))
      (forward-line (1- (elpy-xref--identifier-line id)))
      (search-forward (elpy-xref--identifier-name id) (line-end-position))
      (goto-char (match-beginning 0))))

  ;; Find definition
  (cl-defmethod xref-backend-definitions ((_backend (eql elpy)) id)
    (elpy-xref--definitions id))

  (defun elpy-xref--definitions (id)
    "Return SYMBOL definition position as a xref object."
    (save-excursion
      (elpy-xref--goto-identifier id)
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
  (cl-defmethod xref-backend-references ((_backend (eql elpy)) id)
    (elpy-xref--references id))

  (defun elpy-xref--references (id)
    "Return SYMBOL references as a list of xref objects."
    (save-excursion
      (elpy-xref--goto-identifier id)
      (let* ((references (elpy-rpc-get-usages)))
        (cl-loop
         for ref in references
         for file = (alist-get 'filename ref)
         for pos = (+ (alist-get 'offset ref) 1)
         for line = (when file
                      (with-current-buffer (find-file-noselect file)
                        (save-excursion
                          (goto-char (+ pos 1))
                          (buffer-substring (line-beginning-position) (line-end-position)))))
         for linenumber = (when file
                            (with-current-buffer (find-file-noselect file)
                              (line-number-at-pos pos)))
         for summary = (format elpy-xref--format-references linenumber line)
         for loc = (xref-make-elpy-location file pos)
         if file
         collect (xref-make summary loc)))))

  ;; Completion table (used when calling `xref-find-references`)
  (cl-defmethod xref-backend-identifier-completion-table ((_backend (eql elpy)))
    (elpy-xref--get-completion-table))

  (defun elpy-xref--get-completion-table ()
    "Return the completion table for identifiers."
    (cl-loop
     for ref in (nreverse (elpy-rpc-get-names))
     for offset = (+ (alist-get 'offset ref) 1)
     for line = (line-number-at-pos offset)
     for id = (format "%s: %s" line (alist-get 'name ref))
     collect id))

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
  (unless elpy-modules-initialized-p
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

If you need your modeline, you can set the variable `elpy-remove-modeline-lighter' to nil"

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
  "Module for sane Emacs default for python."
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
     (require 'company-capf)
     (elpy-modules-remove-modeline-lighter 'company-mode)
     (define-key company-active-map (kbd "C-d")
       'company-show-doc-buffer)
     (add-hook 'inferior-python-mode-hook
               (lambda ()
                 ;; Workaround for company bug
                 ;; (https://github.com/company-mode/company-mode/issues/759)
                 (setq-local company-transformers
                             (remove 'company-sort-by-occurrence
                                     company-transformers))
                 ;; Be sure to trigger completion for one character variable
                 ;; (i.e. `a.`)
                 (setq-local company-minimum-prefix-length 2))))

    (`buffer-init
     ;; We want immediate completions from company.
     (set (make-local-variable 'company-idle-delay)
          0.1)
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

Add parentheses in case ANNOTATION is \"class\", \"function\", or
\"instance\",unless the completion is already looking at a left
 parenthesis,or unless NAME is a Python exception outside a reasonably
 formed raise statement,or unless NAME is no callable instance."
  (unless (looking-at-p "\(")
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
           ;; backend to eliminate the `elpy-rpc-get-calltip' call below.
           (insert "()")
           (backward-char 1)
           (unless (elpy-rpc-get-calltip)
             (backward-char 1)
             (delete-char 2))))))

(defun elpy-company--add-interpreter-completions-candidates (candidates)
  "Add completions candidates from the shell to the list of candidates.

Get completions candidates at point from the shell, normalize them to look
like what elpy-company returns, merge them with the CANDIDATES list
and return the list."
  ;; Check if prompt available
  (if (not (and elpy-get-info-from-shell
                (elpy-shell--check-if-shell-available)))
      candidates
    ;; Completion need the cursor to be at the end of the shell buffer
    (save-excursion
      (with-current-buffer (process-buffer (python-shell-get-process))
        (goto-char (point-max)))
      ;; Try to get the info with timeout
      (let* ((new-candidates (with-timeout (elpy-get-info-from-shell-timeout
                                            '(nil nil nil))
                               (python-completion-complete-at-point)))
             (start (nth 0 new-candidates))
             (end (nth 1 new-candidates))
             (completion-list (nth 2 new-candidates)))
        (if (not (and start end))
            candidates
          ;; Add the new candidates to the current ones
          (let ((candidate-names (cl-map 'list
                                         (lambda (el) (cdr (assoc 'name el)))
                                         candidates))
                (new-candidate-names (all-completions
                                      (buffer-substring start end)
                                      completion-list)))
            (cl-loop
             for pytel-cand in new-candidate-names
             for pytel-cand = (replace-regexp-in-string "($" "" pytel-cand)
             for pytel-cand = (replace-regexp-in-string "^.*\\." ""
                                                        pytel-cand)
             if (not (member pytel-cand candidate-names))
             do (push (list (cons 'name pytel-cand)) candidates)))
          candidates)))))

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
                ;; add completion candidates from python.el
                (setq result
                      (elpy-company--add-interpreter-completions-candidates
                       result))
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
     (elpy-modules-remove-modeline-lighter 'eldoc-minor-mode))
    (`buffer-init
     (eldoc-add-command-completions "python-indent-dedent-line-backspace")
     (set (make-local-variable 'company-frontends)
          (remq 'company-echo-metadata-frontend company-frontends))
     (if (boundp 'eldoc-documentation-functions)
         (add-hook 'eldoc-documentation-functions
                   'elpy-eldoc-documentation
                   nil t))
         (set (make-local-variable 'eldoc-documentation-function)
              'elpy-eldoc-documentation))
     (eldoc-mode 1))
    (`buffer-stop
     (eldoc-mode -1)
     (kill-local-variable 'eldoc-documentation-function))))

(defun elpy-eldoc-documentation (&optional callback &rest _more)
  "Return some interesting information for the code at point.

This function is good to place in
`eldoc-documentation-functions' (plural), (for newer Emacsen) and
also `eldoc-documentation-function' (for older Emacsen).

This will return flymake errors for the line at point if there
are any. If not, this will do an asynchronous call to the RPC
backend to get a call tip, and display that using
`eldoc-message'. If the backend has no call tip, this will
display the current class and method instead."
  (let ((cb (or callback
                (lambda (doc &rest plist)
                  (let ((thing (plist-get plist :thing))
                        (face (plist-get plist :face)))
                    (when thing
                      (setq doc
                            (if (version<= emacs-version "25")
                                (format "%s%s" thing doc)
                              (eldoc-docstring-format-sym-doc thing doc face))))
                    (eldoc-message doc)))))
        (flymake-error (elpy-flymake-error-at-point)))
    (if flymake-error
        flymake-error
      (elpy-rpc-get-calltip-or-oneline-docstring
       (lambda (info)
         (cond
          ;; INFO is a string, just display it
          ((stringp info)
           (funcall callback info))
          ;; INFO is a calltip
          ((string= (cdr (assq 'kind info)) "calltip")
           (let ((name (cdr (assq 'name info)))
                 (index (cdr (assq 'index info)))
                 (params (cdr (assq 'params info))))
             (when index
               (setf (nth index params)
                     (propertize (nth index params)
                                 'face
                                 'eldoc-highlight-function-argument)))
             (let ((prefix (propertize name 'face
                                       'font-lock-function-name-face))
                   (args (format "(%s)" (mapconcat #'identity params ", "))))
               (funcall callback args :thing prefix))))
          ;; INFO is a oneline docstring
          ((string= (cdr (assq 'kind info)) "oneline_doc")
           (let ((name (cdr (assq 'name info)))
                 (docs (cdr (assq 'doc info))))
             (funcall cb docs :thing name)))
          ;; INFO is nil, maybe display the current function
          (t
           (if elpy-eldoc-show-current-function
               (let ((current-defun (python-info-current-defun)))
                 (when current-defun
                   (eldoc-message
                    (concat "In: "
                            (propertize
                             (format "%s()" current-defun)
                             'face 'font-lock-function-name-face)))))
             (eldoc-message ""))))))
      (if callback
          ;; New protocol: return non-nil, non-string
          t
        ;; Old protocol: return the last message until we're done
        eldoc-last-message))))


;;;;;;;;;;;;;;;;;;;
;;; Module: Folding

(defun elpy-module-folding (command &rest _args)
  "Module allowing code folding in Python."
  (pcase command

    (`global-init
     (elpy-modules-remove-modeline-lighter 'hs-minor-mode))

    (`buffer-init
     (hs-minor-mode 1)
     (setq-local hs-set-up-overlay 'elpy-folding--display-code-line-counts)
     (setq-local hs-allow-nesting t)
     (define-key elpy-mode-map [left-fringe mouse-1]
       'elpy-folding--click-fringe)
     (define-key elpy-mode-map (kbd "<mouse-1>") 'elpy-folding--click-text)
     (elpy-folding--mark-foldable-lines)
     (add-to-list 'after-change-functions 'elpy-folding--mark-foldable-lines))

    (`buffer-stop
     (hs-minor-mode -1)
     (kill-local-variable 'hs-set-up-overlay)
     (kill-local-variable 'hs-allow-nesting)
     (define-key elpy-mode-map [left-fringe mouse-1] nil)
     (define-key elpy-mode-map (kbd "<mouse-1>") nil)
     (remove 'elpy-folding--mark-foldable-lines after-change-functions)
     (cl-loop for prop in '(elpy-hs-folded elpy-hs-foldable elpy-hs-fringe)
              do (remove-overlays (point-min) (point-max) prop t)))))

;; Fringe and folding indicators
(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'elpy-folding-fringe-marker
    (vector #b00000000
            #b00000000
            #b00000000
            #b11000011
            #b11100111
            #b01111110
            #b00111100
            #b00011000))

  (define-fringe-bitmap 'elpy-folding-fringe-foldable-marker
    (vector #b00100000
            #b00010000
            #b00001000
            #b00000100
            #b00000100
            #b00001000
            #b00010000
            #b00100000)))

(defcustom elpy-folding-fringe-face 'elpy-folding-fringe-face
  "Face for folding bitmaps appearing on the fringe."
  :type 'face
  :group 'elpy)

(defface elpy-folding-fringe-face
  '((t (:inherit 'font-lock-comment-face
                 :box (:line-width 1 :style released-button))))
  "Face for folding bitmaps appearing on the fringe."
  :group 'elpy)

(defcustom elpy-folding-face 'elpy-folding-face
  "Face for the folded region indicator."
  :type 'face
  :group 'elpy)

(defface elpy-folding-face
  '((t (:inherit 'font-lock-comment-face :box t)))
  "Face for the folded region indicator."
  :group 'elpy)

(defcustom elpy-folding-fringe-indicators t
  "Non-nil if Elpy should display folding fringe indicators."
  :set (lambda (var val)                ;
         (set-default var val)
         (dolist (buffer (buffer-list))
           (when (and (with-current-buffer buffer elpy-mode)
                      (member 'elpy-folding--mark-foldable-lines
                              after-change-functions))
             (elpy-folding--mark-foldable-lines)))))

(defvar elpy-folding-docstring-regex "[uU]?[rR]?\"\"\""
  "Regular expression matching docstrings openings and closings.")

(defvar elpy-docstring-block-start-regexp
  "^\\s-*[uU]?[rR]?\"\"\"\n?\\s-*"
  "Version of `hs-block-start-regexp' for docstrings.")

;; Indicators
(defun elpy-folding--display-code-line-counts (ov)
  "Display a folded region indicator with the number of folded lines.

Meant to be used as `hs-set-up-overlay'."
  (let* ((marker-string "*fringe-dummy*")
         (marker-length (length marker-string)))
    (cond
     ((eq 'code (overlay-get ov 'hs))
      (let* ((nmb-line (count-lines (overlay-start ov) (overlay-end ov)))
             (display-string (format "(%d)..." nmb-line)))
        ;; fringe indicator
        (when elpy-folding-fringe-indicators
          (put-text-property 0 marker-length 'display
                             (list 'left-fringe 'elpy-folding-fringe-marker
                                   'elpy-folding-fringe-face)
                             marker-string)
          (overlay-put ov 'before-string marker-string)
          (overlay-put ov 'elpy-hs-fringe t))
        ;; folding indicator
        (put-text-property 0 (length display-string)
                           'face 'elpy-folding-face display-string)
        (put-text-property 0 (length display-string)
                           'mouse-face 'highlight display-string)
        (overlay-put ov 'display display-string)
        (overlay-put ov 'elpy-hs-folded t)))
     ;; for docstring and comments, we don't display the number of line
     ((or (eq 'docstring (overlay-get ov 'hs))
          (eq 'comment (overlay-get ov 'hs)))
      (let ((display-string "..."))
        (put-text-property 0 (length display-string)
                           'mouse-face 'highlight display-string)
        (overlay-put ov 'display display-string)
        (overlay-put ov 'elpy-hs-folded t))))))

(defun elpy-folding--mark-foldable-lines (&optional beg end rm-text-len)
  "Add a fringe indicator for foldable lines.

Meant to be used as a hook to `after-change-functions'."
  (when elpy-folding-fringe-indicators
    (save-excursion
      (save-restriction
        (save-match-data
          (when (and beg end)
            (narrow-to-region (progn (goto-char beg)
                                     (line-beginning-position))
                              (progn (goto-char end)
                                     (line-end-position))))
          (remove-overlays (point-min) (point-max) 'elpy-hs-foldable t)
          (goto-char (point-min))
          (while (re-search-forward
                  python-nav-beginning-of-defun-regexp nil t)
            (let* ((beg (line-beginning-position))
                   (end (line-end-position))
                   (ov (make-overlay beg end))
                   (marker-string "*fringe-dummy*")
                   (marker-length (length marker-string)))
              (when (version<= "25.2" emacs-version)
                (put-text-property 0 marker-length
                                   'display
                                   (list
                                    'left-fringe
                                    'elpy-folding-fringe-foldable-marker
                                    'elpy-folding-fringe-face)
                                   marker-string)
                (overlay-put ov 'before-string marker-string))
              (overlay-put ov 'elpy-hs-foldable t))))))))

;; Mouse interaction
(defun elpy-folding--click-fringe (event)
  "Hide or show block on fringe click."
  (interactive "e")
  (hs-life-goes-on
   (when elpy-folding-fringe-indicators
     (mouse-set-point event)
     (let* ((folded (save-excursion
                      (end-of-line)
                      (cl-remove-if-not (lambda (ov)
                                          (overlay-get ov 'elpy-hs-folded))
                                        (overlays-at (point)))))
            (foldable (cl-remove-if-not (lambda (ov)
                                          (overlay-get ov 'elpy-hs-foldable))
                                        (overlays-at (point)))))
       (if folded
           (hs-show-block)
         (if foldable
             (hs-hide-block)))))))

(defun elpy-folding--click-text (event)
  "Show block on click."
  (interactive "e")
  (hs-life-goes-on
   (save-excursion
     (let ((window (posn-window (event-end event)))
           (pos (posn-point (event-end event))))
       (with-current-buffer (window-buffer window)
         (goto-char pos)
         (when (hs-overlay-at (point))
           (hs-show-block)
           (deactivate-mark)))))))

;; Hidding docstrings
(defun elpy-folding--hide-docstring-region (beg end)
  "Hide a region from BEG to END, marking it as a docstring.

BEG and END have to be respectively on the first and last line
of the docstring, their values are adapted to hide only the
docstring body."
  (hs-life-goes-on
   ;; do not fold oneliners
   (when (not (save-excursion
                (goto-char beg)
                (beginning-of-line)
                (re-search-forward
                 (concat elpy-folding-docstring-regex
                         ".*"
                         elpy-folding-docstring-regex)
                 (line-end-position) t)))
     ;; get begining position (do not fold first doc line)
     (save-excursion
       (goto-char beg)
       (when (save-excursion
               (beginning-of-line)
               (re-search-forward
                (concat elpy-folding-docstring-regex
                        "[[:space:]]*$")
                (line-end-position) t))
         (forward-line 1))
       (beginning-of-line)
       (back-to-indentation)
       (setq beg (point))
       (setq ov-beg (line-end-position)))
     ;; get end position
     (save-excursion
       (goto-char end)
       (setq end (line-beginning-position))
       (setq ov-end (line-end-position)))
     (hs-discard-overlays ov-beg ov-end)
     (hs-make-overlay ov-beg ov-end 'docstring (- beg ov-beg) (- end ov-end))
     (run-hooks 'hs-hide-hook)
     (goto-char beg))))

(defun elpy-folding--hide-docstring-at-point ()
  "Hide the docstring at point."
  (hs-life-goes-on
   (let ((hs-block-start-regexp elpy-docstring-block-start-regexp))
     (when (and (python-info-docstring-p) (not (hs-already-hidden-p)))
       (let (beg end line-beg line-end)
         ;; Get first doc line
         (if (not (save-excursion (forward-line -1)
                                  (python-info-docstring-p)))
             (setq beg (line-beginning-position))
           (forward-line -1)
           (end-of-line)
           (re-search-backward (concat "^[[:space:]]*"
                                       elpy-folding-docstring-regex)
                               nil t)
           (setq beg (line-beginning-position)))
         ;; Go to docstring opening (to be sure to be inside the docstring)
         (re-search-forward elpy-folding-docstring-regex nil t)
         (setq line-beg (line-number-at-pos))
         ;; Get last line
         (if (not (save-excursion (forward-line 1)
                                  (python-info-docstring-p)))
             (progn
               (setq end (line-end-position))
               (setq line-end (line-number-at-pos)))
           (re-search-forward elpy-folding-docstring-regex nil t)
           (setq end (line-end-position))
           (setq line-end (line-number-at-pos)))
         ;; hide the docstring
         (when (not (= line-end line-beg))
           (elpy-folding--hide-docstring-region beg end)))))))

(defun elpy-folding--show-docstring-at-point ()
  "Show docstring at point."
  (hs-life-goes-on
   (let ((hs-block-start-regexp elpy-docstring-block-start-regexp))
     (when (python-info-docstring-p)
       (hs-show-block)))))

(defvar-local elpy-folding-docstrings-hidden nil
  "If docstrings are globally hidden or not.")

(defun elpy-folding-toggle-docstrings ()
  "Fold or unfold every docstrings in the current buffer."
  (interactive)
  (if (not hs-minor-mode)
      (message "Please enable the 'Folding module' to use this functionality.")
    (hs-life-goes-on
     (save-excursion
       (goto-char (point-min))
       (while (python-nav-forward-defun)
         (search-forward-regexp ")\\s-*:" nil t)
         (forward-line)
         (when (and (python-info-docstring-p)
                    (progn
                      (beginning-of-line)
                      (search-forward-regexp elpy-folding-docstring-regex
                                             nil t)))
           (forward-char 2)
           (back-to-indentation)
           ;; be sure not to act on invisible docstrings
           (unless (and (hs-overlay-at (point))
                        (not (eq (overlay-get (hs-overlay-at (point)) 'hs)
                                 'docstring)))
             (if elpy-folding-docstrings-hidden
                 (elpy-folding--show-docstring-at-point)
               (elpy-folding--hide-docstring-at-point)))))))
    (setq elpy-folding-docstrings-hidden (not elpy-folding-docstrings-hidden))))

;; Hiding comments
(defvar-local elpy-folding-comments-hidden nil
  "If comments are globally hidden or not.")

(defun elpy-folding-toggle-comments ()
  "Fold or unfold every comment blocks in the current buffer."
  (interactive)
  (if (not hs-minor-mode)
      (message "Please enable the 'Folding module' to use this functionality.")
    (hs-life-goes-on
     (save-excursion
       (goto-char (point-min))
       (while (comment-search-forward (point-max) t)
         ;; be suse not to act on invisible comments
         (unless (and (hs-overlay-at (point))
                      (not (eq (overlay-get (hs-overlay-at (point)) 'hs)
                               'comment)))
           (if (not elpy-folding-comments-hidden)
               ;; be sure to be on a multiline comment
               (when (save-excursion (forward-line)
                                     (comment-only-p (line-beginning-position)
                                                     (line-end-position)))
                 (hs-hide-block))
             (hs-show-block)))
         (python-util-forward-comment (buffer-size))))
     (setq elpy-folding-comments-hidden (not elpy-folding-comments-hidden)))))

;; Hiding leafs
;;     taken from https://www.emacswiki.org/emacs/HideShow
(defun elpy-folding--hide-leafs (beg end)
  "Hide blocks that do not contain others blocks in region (BEG END)."
  (hs-life-goes-on
   (save-excursion
     (goto-char beg)
     ;; Find the current block beginning and end
     (when (hs-find-block-beginning)
       (setq beg (1+ (point)))
       (funcall hs-forward-sexp-func 1)
       (setq end (1- (point))))
     ;; Show all branches if nesting not allowed
     (unless hs-allow-nesting
       (hs-discard-overlays beg end))
     ;; recursively treat current block leafs
     (let ((leaf t))
       (while (progn
                (forward-comment (buffer-size))
                (and (< (point) end)
                     (re-search-forward hs-block-start-regexp end t)))
         (setq pos (match-beginning hs-block-start-mdata-select))
         (if (elpy-folding--hide-leafs pos end)
             (save-excursion
               (goto-char pos)
               (hs-hide-block-at-point t)))
         (setq leaf nil))
       (goto-char end)
       (run-hooks 'hs-hide-hook)
       leaf))))

(defun elpy-folding-hide-leafs ()
  "Hide all blocks that do not contain other blocks."
  (interactive)
  (if (not hs-minor-mode)
      (message "Please enable the 'Folding module' to use this functionality.")
    (hs-life-goes-on
     (let ((beg (save-excursion
                  (goto-char (if (use-region-p) (region-beginning) (point-min)))
                  (line-beginning-position)))
           (end (save-excursion
                  (goto-char (if (use-region-p) (region-end) (point-max)))
                  (1+ (line-end-position)))))
       (elpy-folding--hide-leafs beg end)))))

;; DWIM functions
(defun elpy-folding--hide-region (beg end)
  "Hide the region betwwen BEG and END."
  (hs-life-goes-on
   (save-excursion
     (let ((beg-eol (progn (goto-char beg) (line-end-position)))
           (end-eol (progn (goto-char end) (line-end-position))))
       (hs-discard-overlays beg-eol end-eol)
       (hs-make-overlay beg-eol end-eol 'code (- beg beg-eol) (- end end-eol))
       (run-hooks 'hs-hide-hook)
       (deactivate-mark)))))

(defun elpy-folding-toggle-at-point ()
  "Fold/Unfold the block, comment or docstring at point.

If a region is selected, fold that region."
  (interactive)
  (if (not hs-minor-mode)
      (message "Please enable the 'Folding module' to use this functionality.")
    (hs-life-goes-on
     ;; Use selected region
     (if (use-region-p)
         (elpy-folding--hide-region (region-beginning) (region-end))
       ;; Adapt starting regexp if on a docstring
       (let ((hs-block-start-regexp
              (if (python-info-docstring-p)
                  elpy-docstring-block-start-regexp
                hs-block-start-regexp)))
         ;; Hide or fold
         (cond
          ((hs-already-hidden-p)
           (hs-show-block))
          ((python-info-docstring-p)
           (elpy-folding--hide-docstring-at-point))
          (t
           (hs-hide-block))))))))

;;;;;;;;;;;;;;;;;;;
;;; Module: Flymake

(defun elpy-module-flymake (command &rest _args)
  "Enable Flymake support for Python."
  (pcase command
    (`global-init
     (require 'flymake)
     ;; flymake modeline is quite useful for emacs > 26.1
     (when (version< emacs-version "26.1")
       (elpy-modules-remove-modeline-lighter 'flymake-mode))
     ;; Add our initializer function.
     (unless (version<= "26.1" emacs-version)
       (add-to-list 'flymake-allowed-file-name-masks
                    '("\\.py\\'" elpy-flymake-python-init))))

    (`buffer-init
     ;; Avoid fringes clash between flymake and folding indicators
     (if (and (member 'elpy-module-folding elpy-modules)
              elpy-folding-fringe-indicators)
         (setq-local flymake-fringe-indicator-position 'right-fringe)
       (setq-local flymake-fringe-indicator-position 'left-fringe))
     ;; Set this for `elpy-check' command
     (setq-local python-check-command elpy-syntax-check-command)
     ;; For emacs > 26.1, python.el natively supports flymake,
     ;; so we just tell python.el to use the wanted syntax checker
     (when (version<= "26.1" emacs-version)
       (setq-local python-flymake-command
                   (let ((command (split-string elpy-syntax-check-command)))
                     (if (string= (file-name-nondirectory (car command))
                                  "flake8")
                         (append command '("-"))
                       command))))

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
     ;; Useless for emacs >= 26.1, as warning are handled fine
     ;; COMPAT: Obsolete variable as of 24.4
     (cond
      ((version<= "26.1" emacs-version)
       (setq-default python-flymake-msg-alist
                     '(("^W[0-9]+" . :warning)
                       ("^E[0-9]+" . :error))))
      ((boundp 'flymake-warning-predicate)
       (set (make-local-variable 'flymake-warning-predicate) "^W[0-9]"))
      (t
       (set (make-local-variable 'flymake-warning-re) "^W[0-9]")))

     ;; for emacs >= 26.1, elpy relies on `python-flymake-command`, and
     ;; doesn't need `python-check-command` anymore.
     (when (and (buffer-file-name)
                (or (version<= "26.1" emacs-version)
                    (executable-find python-check-command)))
       (flymake-mode 1)))
    (`buffer-stop
     (flymake-mode -1)
     (kill-local-variable 'flymake-no-changes-timeout)
     (kill-local-variable 'flymake-start-syntax-check-on-newline)
     ;; Disable warning faces for flake8 output.
     ;; Useless for emacs >= 26.1, as warning are handled fine
     ;; COMPAT: Obsolete variable as of 24.4
     (cond
      ((version<= "26.1" emacs-version) t)
      ((boundp 'flymake-warning-predicate)
       (kill-local-variable 'flymake-warning-predicate))
      (t
       (kill-local-variable 'flymake-warning-re))))))


(defun elpy-flymake-python-init ()
  ;; Make sure it's not a remote buffer as flymake would not work
  (unless (file-remote-p buffer-file-name)
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace)))
      (list python-check-command
            (list temp-file)
            ;; Run flake8 from / to avoid import problems (#169)
            "/"))))

(defun elpy-flymake-next-error ()
  "Move forward to the next Flymake error and show a description."
  (interactive)
  (flymake-goto-next-error)
  (elpy-flymake-show-error))

(defun elpy-flymake-previous-error ()
  "Move backward to the previous Flymake error and show a description."
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
  (cond ((boundp 'flymake-err-info)     ; emacs < 26
         (let* ((lineno (line-number-at-pos))
                (err-info (car (flymake-find-err-info flymake-err-info
                                                      lineno))))
           (when err-info
             (mapconcat #'flymake-ler-text
                        err-info
                        ", "))))
        ((and (fboundp 'flymake-diagnostic-text)
              (fboundp 'flymake-diagnostics)) ; emacs >= 26
         (let ((diag (flymake-diagnostics (point))))
           (when diag
             (mapconcat #'flymake-diagnostic-text diag ", "))))))

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
     (unless (listp yas-snippet-dirs)
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
    (`global-init
     (add-to-list 'elpy-project-root-finder-functions
                  'elpy-project-find-django-root t))
    (`global-stop
     (setq elpy-project-root-finder-functions
           (remove 'elpy-project-find-django-root
                   elpy-project-root-finder-functions)))
    (`buffer-init
     (elpy-django-setup))
    (`buffer-stop
     (elpy-django -1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Module: Autodoc

(defun elpy-module-autodoc (command &rest _args)
  "Module to automatically update documentation."
  (pcase command
    (`buffer-init
     (add-hook 'pre-command-hook 'elpy-autodoc--pre-command nil t)
     (add-hook 'post-command-hook 'elpy-autodoc--post-command nil t)
     (make-local-variable 'company-frontends)
     (add-to-list 'company-frontends 'elpy-autodoc--frontend :append))
    (`buffer-stop
     (remove-hook 'pre-command-hook 'elpy-autodoc--pre-command t)
     (remove-hook 'post-command-hook 'elpy-autodoc--post-command t)
     (setq company-frontends (remove 'elpy-autodoc--frontend company-frontends)))))


;; Auto refresh documentation on cursor motion
(defvar elpy-autodoc--timer nil
  "Timer to refresh documentation.")

(defcustom elpy-autodoc-delay .5
  "The idle delay in seconds until documentation is refreshed automatically."
  :type '(choice (const :tag "immediate (0)" 0)
                 (number :tag "seconds"))
  :group 'elpy
  :group 'elpy-autodoc)

(defun elpy-autodoc--pre-command ()
  "Cancel autodoc timer on user action."
  (when elpy-autodoc--timer
    (cancel-timer elpy-autodoc--timer)
    (setq elpy-autodoc--timer nil)))

(defun elpy-autodoc--post-command ()
  "Set up autodoc timer after user action."
  (when elpy-autodoc-delay
    (setq elpy-autodoc--timer
          (run-with-timer elpy-autodoc-delay nil
                          'elpy-autodoc--refresh-doc))))

(defun elpy-autodoc--refresh-doc ()
  "Refresh the doc asynchronously with the symbol at point."
  (when (get-buffer-window "*Python Doc*")
    (elpy-rpc-get-docstring 'elpy-autodoc--show-doc
                            (lambda (_reason) nil))))

(defun elpy-autodoc--show-doc (doc)
  "Display DOC (if any) but only if the doc buffer is currently visible."
  (when (and doc (get-buffer-window "*Python Doc*"))
    (elpy-doc--show doc)))

;; Auto refresh documentation in company candidate selection
(defun elpy-autodoc--frontend (command)
  "Elpy autodoc front-end for refreshing documentation."
  (pcase command
    (`post-command
     (when elpy-autodoc-delay
       (when elpy-autodoc--timer
         (cancel-timer elpy-autodoc--timer))
       (setq elpy-autodoc--timer
             (run-with-timer elpy-autodoc-delay
                             nil
                             'elpy-autodoc--refresh-doc-from-company))))
    (`hide
     (when elpy-autodoc--timer
       (cancel-timer elpy-autodoc--timer)))))

(defun elpy-autodoc--refresh-doc-from-company ()
  "Refresh the doc asynchronously using the current company candidate."
  (let* ((symbol (nth company-selection company-candidates))
         (doc (elpy-rpc-get-completion-docstring symbol)))
    (elpy-autodoc--show-doc doc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Backwards compatibility

;; TODO Some/most of this compatibility code can now go, as minimum required
;; Emacs version is 24.4.

;; Functions for Emacs 24 before 24.3
(unless (fboundp 'python-info-current-defun)
  (defalias 'python-info-current-defun 'python-current-defun))

(unless (fboundp 'python-nav-forward-statement)
  (defun python-nav-forward-statement (&rest ignored)
    "Function added in Emacs 24.3"
    (error "Enhanced Python navigation only available in Emacs 24.3+")))

(unless (fboundp 'python-nav-backward-up-list)
  (defun python-nav-backward-up-list ()
    "Compatibility function for older Emacsen"
    (ignore-errors
      (backward-up-list))))

(unless (fboundp 'python-shell-calculate-exec-path)
  (defun python-shell-calculate-exec-path ()
    "Compatibility function for older Emacsen."
    exec-path))

(unless (fboundp 'python-shell-calculate-process-environment)
  (defun python-shell-calculate-process-environment ()
    "Compatibility function for older Emacsen."
    process-environment))

(unless (fboundp 'python-shell-get-process-name)
  (defun python-shell-get-process-name (_dedicated)
    "Compatibility function for older Emacsen."
    "Python"))

(unless (fboundp 'python-shell-parse-command)
  (defun python-shell-parse-command ()
    "Compatibility function for older Emacsen."
    python-python-command))

(unless (fboundp 'python-shell-send-buffer)
  (defun python-shell-send-buffer (&optional _arg)
    (python-send-buffer)))

(unless (fboundp 'python-shell-send-string)
  (defalias 'python-shell-send-string 'python-send-string))

(unless (fboundp 'python-indent-shift-right)
  (defalias 'python-indent-shift-right 'python-shift-right))

(unless (fboundp 'python-indent-shift-left)
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
(unless (fboundp 'highlight-indentation-mode)
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

;; first-prompt-hook has been added in emacs 25.
;; for earlier versions, make sure Elpy's setup code is
;; still send to the python shell.
(unless (boundp 'python-shell-first-prompt-hook)
  (add-hook 'inferior-python-mode-hook
            (lambda ()
              (when (elpy-project-root)
                (let ((process (get-buffer-process (current-buffer))))
                  (python-shell-send-string
                   (format "import sys;sys.path.append('%s');del sys"
                           (elpy-project-root))
                   process))))))



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

(unless (fboundp 'python-info-docstring-p)
  (defun python-info-docstring-p (&optional syntax-ppss)
    "Return non-nil if point is in a docstring."
    (save-excursion
      (and (progn (python-nav-beginning-of-statement)
                  (looking-at "\\(\"\\|'\\)"))
           (progn (forward-line -1)
                  (beginning-of-line)
                  (python-info-looking-at-beginning-of-defun))))))

(provide 'elpy)
;;; elpy.el ends here
