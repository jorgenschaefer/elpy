;;; elpy.el --- Emacs Lisp Python Environment

;; Copyright (C) 2012, 2013  Jorgen Schaefer <forcer@forcix.cx>

;; Author: Jorgen Schaefer <forcer@forcix.cx>
;; URL: https://github.com/jorgenschaefer/elpy
;; Version: 1.0

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

;;; Code:

(require 'auto-complete-config)
(require 'elpy-refactor)
(require 'etags)
(require 'find-file-in-project)
(require 'flymake)
(require 'highlight-indentation)
(require 'idomenu)
(require 'json)
(require 'nose)
(require 'python)
(require 'thingatpt)
(require 'virtualenv)
(require 'yasnippet)


;;;;;;;;;;;;;;;
;;; Elpy itself

(defgroup elpy nil
  "The Emacs Lisp Python Environment."
  :prefix "elpy-"
  :group 'languages)

(defcustom elpy-rpc-python-command "python"
  "The command to be used for the RPC backend."
  :type 'string
  :group 'elpy)

(defcustom elpy-rpc-project-specific nil
  "Whether Elpy should use a separate process for each project."
  :type 'boolean
  :group 'elpy)

(defcustom elpy-rpc-backend nil
  "Your preferred backend.

Either nil, or a string.

nil    - Select a backend automatically.
rope   - Use the Rope refactoring library. This will create
         .ropeproject directories in your project roots.
jedi   - Use the Jedi completion library.
native - Do not use any backend, use native Python methods only."
  :type '(choice (const "rope")
                 (const "jedi")
                 (const "native")
                 (const nil))
  :group 'elpy)

(defcustom elpy-default-minor-modes '(eldoc-mode
                                      flymake-mode
                                      highlight-indentation-mode
                                      yas-minor-mode
                                      auto-complete-mode)
  "Minor modes enabled when `elpy-mode' is enabled."
  :group 'elpy)

(defcustom elpy-mode-hook nil
  "Hook run when `elpy-mode' is enabled."
  :group 'elpy)

(defvar elpy-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Alphabetical order to make it easier to find free C-c C-X
    ;; bindings in the future. Heh.

    ;; (define-key map (kbd "<backspace>") 'python-indent-dedent-line-backspace)
    ;; (define-key map (kbd "<backtab>")   'python-indent-dedent-line)
    ;; (define-key map (kbd "<tab>")       'ac-trigger-key)

    ;; (define-key map (kbd "C-M-x")   'python-shell-send-defun)
    ;; (define-key map (kbd "C-c <")   'python-indent-shift-left)
    ;; (define-key map (kbd "C-c >")   'python-indent-shift-right)
    (define-key map (kbd "C-c C-c") 'elpy-shell-send-region-or-buffer)
    (define-key map (kbd "C-c C-z") 'elpy-shell-switch-to-shell)
    (define-key map (kbd "C-c C-d") 'elpy-doc)
    (define-key map (kbd "C-c C-f") 'find-file-in-project)
    ;; (define-key map (kbd "C-c C-i") 'yasnippet-expand)
    (define-key map (kbd "C-c C-j") 'idomenu)
    (define-key map (kbd "C-c C-n") 'elpy-flymake-forward-error)
    (define-key map (kbd "C-c C-o") 'elpy-occur-definitions)
    (define-key map (kbd "C-c C-p") 'elpy-flymake-backward-error)
    (define-key map (kbd "C-c C-q") 'elpy-show-defun)
    (define-key map (kbd "C-c C-r") 'elpy-refactor)
    (define-key map (kbd "C-c C-s") 'elpy-rgrep-symbol)
    (define-key map (kbd "C-c C-t") 'elpy-test)
    (define-key map (kbd "C-c C-v") 'elpy-check)
    (define-key map (kbd "C-c C-w") 'elpy-doc-websearch)
    ;; (define-key map (kbd "C-c C-z") 'python-shell-switch-to-shell)

    (define-key map (kbd "<C-down>") 'elpy-forward-definition)
    (define-key map (kbd "<C-up>")  'elpy-backward-definition)
    ;; (define-key map (kbd "M-,")     'iedit-mode
    (define-key map (kbd "M-.")     'elpy-goto-definition)
    (define-key map (kbd "M-a")     'elpy-nav-backward-statement)
    (define-key map (kbd "M-e")     'elpy-nav-forward-statement)
    (define-key map (kbd "M-n")     'elpy-forward-definition)
    (define-key map (kbd "M-p")     'elpy-backward-definition)

    map)
  "Key map for the Emacs Lisp Python Environment.")

;;;###autoload
(defun elpy-enable (&optional skip-initialize-variables)
  "Enable Elpy in all future Python buffers.

When SKIP-INITIALIZE-VARIABLES is non-nil, this will NOT call
`elpy-initialize-variables' to configure various modes in a way
that the Elpy author considers sensible. If you'd rather
configure those modes yourself, pass t here."
  (interactive)
  (when (< emacs-major-version 24)
    (error "Elpy requires Emacs 24 or newer"))
  (let ((filename (find-lisp-object-file-name 'python-mode
                                              'symbol-function)))
    (when (and filename
               (string-match "/python-mode\\.el\\'"
                             filename))
      (error (concat "You are using python-mode.el. "
                     "Elpy only works with python.el from "
                     "Emacs 24 and above"))))
  (add-hook 'python-mode-hook 'elpy-mode)
  (when (not skip-initialize-variables)
    (elpy-initialize-variables)))

;;;###autoload
(defun elpy-disable ()
  "Disable Elpy in all future Python buffers."
  (interactive)
  (remove-hook 'python-mode-hook 'elpy-mode))

;;;###autoload
(define-minor-mode elpy-mode
  "Minor mode in Python buffers for the Emacs Lisp Python Environment.

This mode fully supports virtualenvs. Once you switch a
virtualenv using \\[virtualenv-workon], you can use
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
    (when buffer-file-name
      (set (make-local-variable 'ffip-project-root) (elpy-project-root)))
    (set (make-local-variable 'eldoc-documentation-function)
         'elpy-eldoc-documentation)
    (add-to-list 'ac-sources 'ac-source-elpy)
    (add-to-list 'ac-sources 'ac-source-elpy-dot)
    (add-hook 'before-save-hook 'elpy-rpc-before-save nil t)
    (add-hook 'after-save-hook 'elpy-rpc-after-save nil t)
    ;; Enable modes, hence the 1.
    (run-hook-with-args 'elpy-default-minor-modes 1))
   (t
    (kill-local-variable 'ffip-project-root)
    (kill-local-variable 'eldoc-documentation-function)
    (setq ac-sources
          (delq 'ac-source-elpy
                (delq 'ac-source-elpy-dot
                      ac-sources)))
    (remove-hook 'before-save-hook 'elpy-rpc-before-save t)
    (remove-hook 'after-save-hook 'elpy-rpc-after-save t))))

(defun elpy-installation-instructions (message &optional show-elpy-module)
  "Display a window with installation instructions for the Python
side of elpy.

MESSAGE is shown as the first paragraph.

If SHOW-ELPY-MODULE is non-nil, the help buffer will first
explain how to install the elpy module."
  (with-help-window "*Elpy Installation*"
    (with-current-buffer "*Elpy Installation*"
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Elpy Installation Instructions\n")
        (insert "\n")
        (insert message)
        (when (not (bolp))
          (insert "\n"))
        (insert "\n")
        (when (and (boundp 'elpy-rpc-buffer)
                   elpy-rpc-buffer)
          (let ((elpy-rpc-output (with-current-buffer elpy-rpc-buffer
                                   (buffer-string))))
            (when (not (equal elpy-rpc-output ""))
              (insert (format "The contents of the %s buffer "
                              (buffer-name elpy-rpc-buffer))
                      "might provide further information "
                      "on the problem.\n")
              (insert "\n"))))
        (when show-elpy-module
          (insert "Elpy requires the Python module \"elpy\". The module "
                  "is available from pypi, so you can install it using "
                  "the following command:\n")
          (insert "\n")
          (elpy-installation-command "elpy")
          (insert "\n"))
        (insert "To find possible completions, Elpy uses one of two "
                "Python modules. Either \"rope\" or \"jedi\". To use "
                "Elpy to its fullest potential, you should install "
                "either one of them. Which one is a matter of taste. "
                "You can try both and even switch at runtime using "
                "M-x elpy-set-backend.\n")
        (insert "\n")
        (insert "Elpy also uses the Rope module for refactoring options, "
                "so you likely want to install it even if you use jedi "
                "for completion.\n")
        (insert "\n")
        (if (string-match "Python 3" (shell-command-to-string
                                      "python --version"))
            (elpy-installation-command "rope_py3k")
          (elpy-installation-command "rope"))
        (insert "\n")
        (elpy-installation-command "jedi")
        (insert "\n")
        (insert "If you are using virtualenvs, you can use "
                "M-x virtualenv-workon command to switch to a virtualenv "
                "of your choice. Afterwards, running the command "
                "M-x elpy-rpc-restart will use the packages in "
                "that virtualenv.")
        (fill-region (point-min) (point-max))))))

(defun elpy-installation-command (python-module)
  "Insert an installation command description for PYTHON-MODULE."
  (let* ((do-user-install (not (or (getenv "VIRTUAL_ENV")
                                   virtualenv-workon-session)))
         (user-option (if do-user-install
                          "--user "
                        ""))
         (command (cond
                   ((executable-find "pip")
                    (format "pip install %s%s" user-option python-module))
                   ((executable-find "easy_install")
                    (format "easy_install %s%s" user-option python-module))
                   (t
                    nil))))
    (if (not command)
        (insert "... hm. It appears you have neither pip nor easy_install "
                "available. You might want to get the python-pip or "
                "or python-setuptools package.\n")
      (insert-text-button "[run]"
                          'action (lambda (button)
                                    (async-shell-command
                                     (button-get button 'command)))
                          'command command)
      (insert " " command "\n"))))

(defun elpy-initialize-variables ()
  "This sets some variables in other modes we like to set.

If you want to configure your own keys, do so after this function
is called (usually from `elpy-enable'), or override this function
using (defalias 'elpy-initialize-variables 'identity)"
  ;; Local variables in `python-mode'. This is not removed when Elpy
  ;; is disabled, which can cause some confusion.
  (add-hook 'python-mode-hook 'elpy-initialize-local-variables)

  ;; Flymake support using flake8, including warning faces.
  (when (executable-find "flake8")
    (setq python-check-command "flake8"))

  ;; `flymake-no-changes-timeout': The original value of 0.5 is too
  ;; short for Python code, as that will result in the current line to
  ;; be highlighted most of the time, and that's annoying. This value
  ;; might be on the long side, but at least it does not, in general,
  ;; interfere with normal interaction.
  (setq flymake-no-changes-timeout 60)

  ;; `flymake-start-syntax-check-on-newline': This should be nil for
  ;; Python, as;; most lines with a colon at the end will mean the next
  ;; line is always highlighted as error, which is not helpful and
  ;; mostly annoying.
  (setq flymake-start-syntax-check-on-newline nil)

  ;; `ac-trigger-key': TAB is a great trigger key. We also need to
  ;; tell auto-complete about the new trigger key. This is a bad
  ;; interface to set the trigger key. Don't do this. Just let the
  ;; user set the key in the keymap. Stop second-guessing the user, or
  ;; Emacs.
  (setq ac-trigger-key "TAB")
  (when (fboundp 'ac-set-trigger-key)
    (ac-set-trigger-key ac-trigger-key))

  ;; `ac-auto-show-menu': Short timeout because the menu is great.
  (setq ac-auto-show-menu 0.4)

  ;; `ac-quick-help-delay': I'd like to show the menu right with the
  ;; completions, but this value should be greater than
  ;; `ac-auto-show-menu' to show help for the first entry as well.
  (setq ac-quick-help-delay 0.5)

  ;; Fix some key bindings in ac completions. Using RET when a
  ;; completion is offered is not usually intended to complete (use
  ;; TAB for that), but done while typing and the inputer is considere
  ;; complete, with the intent to simply leave it as is and go to the
  ;; next line. Much like space will not complete, but leave it as is
  ;; and insert a space.
  (define-key ac-completing-map (kbd "RET") nil)
  (define-key ac-completing-map (kbd "<return>") nil)

  ;; `yas-trigger-key': TAB, as is the default, conflicts with the
  ;; autocompletion. We also need to tell yasnippet about the new
  ;; binding. This is a bad interface to set the trigger key. Stop
  ;; doing this.
  (let ((old (when (boundp 'yas-trigger-key)
               yas-trigger-key)))
    (setq yas-trigger-key "C-c C-i")
    (when (fboundp 'yas--trigger-key-reload)
      (yas--trigger-key-reload old)))

  ;; We provide some YASnippet snippets. Add them.
  (add-to-list 'yas-snippet-dirs
               (concat (file-name-directory (locate-library "elpy"))
                       "snippets/"))

  ;; Now load yasnippets.
  (yas-reload-all))

(defun elpy-initialize-local-variables ()
  "Initialize local variables in python-mode.

This should be run from `python-mode-hook'."
  ;; Set `forward-sexp-function' to nil in python-mode. See
  ;; http://debbugs.gnu.org/db/13/13642.html
  (setq forward-sexp-function nil)
  ;; Enable warning faces for flake8 output.
  (when (string-match "flake8" python-check-command)
    (set (make-local-variable 'flymake-warning-re) "^W[0-9]"))
  )

(defvar elpy-project-root 'not-initialized
  "The root of the project the current buffer is in.")
(make-variable-buffer-local 'elpy-project-root)
(put 'elpy-project-root 'safe-local-variable 'file-directory-p)

(defun elpy-project-root ()
  "Return the root of the current buffer's project.

You can set the variable `elpy-project-root' in, for example,
.dir-locals.el to configure this."
  (when (eq elpy-project-root 'not-initialized)
    ;; Set it to nil so when the user runs C-g on the project root
    ;; prompt, it's set to "no project root".
    (setq elpy-project-root nil)
    (setq elpy-project-root
          (or (elpy-project-find-root)
              (read-directory-name "Project root: "
                                   default-directory)))
    (when (and (stringp elpy-project-root)
               (not (file-directory-p elpy-project-root))
               (y-or-n-p "Directory does not exist, create? "))
      (make-directory elpy-project-root t)))
  elpy-project-root)

(defun elpy-project-find-root (&optional skip-current-directory)
  "Find the first directory in the tree not containing an __init__.py

If there is no __init__.py in the current directory, return the
current directory unless SKIP-CURRENT-DIRECTORY is non-nil."
  (cond
   ((file-exists-p (format "%s/__init__.py" default-directory))
    (locate-dominating-file default-directory
                            (lambda (dir)
                              (not (file-exists-p
                                    (format "%s/__init__.py" dir))))))
   ;; Don't return the user's home. That's never a good project root.
   ((and (not skip-current-directory)
         (not (equal (directory-file-name (expand-file-name default-directory))
                     (directory-file-name (expand-file-name "~")))))
    default-directory)
   (t
    nil)))

(defun elpy-set-project-root (new-root)
  "Set the Elpy project root to NEW-ROOT."
  (interactive "DNew project root: ")
  (setq elpy-project-root new-root))

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

(defun elpy-clean-modeline ()
  "Clean up the mode line by removing some lighters.

It's not necessary to see (Python Elpy yas AC ElDoc) all the
time. Honestly."
  (interactive)
  (setq eldoc-minor-mode-string nil)
  (dolist (mode '(elpy-mode yas-minor-mode auto-complete-mode
                            flymake-mode))
    (setcdr (assq mode minor-mode-alist)
            (list ""))))

(defun elpy-shell-send-region-or-buffer (&optional arg)
  "Send the active region or the buffer to the Python shell.

If there is an active region, send that. Otherwise, send the
whole buffer.

Without prefix argument, this will escape the Python idiom of
if __name__ == '__main__' to be false to avoid accidental
execution of code. With prefix argument, this code is executed."
  (interactive "P")
  ;; Ensure process exists
  (elpy-shell-get-or-create-process)
  (if (region-active-p)
      (python-shell-send-region (region-beginning)
                                (region-end))
    (python-shell-send-buffer arg))
  (elpy-shell-switch-to-shell))

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

(defun elpy-check ()
  "Run `python-check-command' on the current buffer's file."
  (interactive)
  (when (not (buffer-file-name))
    (error "Can't check a buffer without a file."))
  (save-some-buffers (not compilation-ask-about-save) nil)
  (let ((process-environment (python-shell-calculate-process-environment))
        (exec-path (python-shell-calculate-exec-path)))
    (compilation-start (concat python-check-command
                               " "
                               (shell-quote-argument (buffer-file-name)))
                       nil (lambda (mode-name)
                             "*Python Check*"))))

(defun elpy-show-defun ()
  "Show the current class and method, in case they are not on
screen."
  (interactive)
  (let ((function (python-info-current-defun)))
    (if function
        (message "%s()" function)
      (message "Not in a function"))))

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

(defun elpy-nav-forward-statement ()
  "Move forward one statement.

This will go to the end of the current statement, or the end of
the next one if already at the end."
  (interactive)
  (let ((old (point)))
    (python-nav-end-of-statement)
    (when (= old (point))
      (python-nav-forward-statement)
      (python-nav-end-of-statement))))

(defun elpy-nav-backward-statement ()
  "Move backward one statement.

This will go to the beginning of the current statement, or the
beginning of the previous one if already at the beginning."
  (interactive)
  (let ((old (point)))
    (python-nav-beginning-of-statement)
    (when (= old (point))
      (python-nav-backward-statement))))

(defun elpy-forward-definition ()
  "Move forward to the next definition (class or function)."
  (interactive)
  (if (save-excursion
        (forward-char 1)
        (re-search-forward "^ *\\(def\\|class\\) " nil t))
      (goto-char (match-beginning 1))
    (goto-char (point-max))))

(defun elpy-backward-definition ()
  "Move backward to the previous definition (class or function)."
  (interactive)
  (if (save-excursion
        (forward-char -1)
        (re-search-backward "^ *\\(def\\|class\\) " nil t))
      (goto-char (match-beginning 1))
    (goto-char (point-min))))

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
  (rgrep (format "\\b%s\\b" symbol)
         "*.py"
         (elpy-project-root))
  (with-current-buffer next-error-last-buffer
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^find .*" nil t)
          (replace-match (format "\\1\nSearching for symbol %s\n"
                                 symbol)))))))

(defun elpy-test (&optional arg)
  "Run nosetests on the current project.

With no prefix arg, all tests are run.
With one prefix arg, only the current test is run.
With two prefix args, only the current module is run."
  (interactive "p")
  (save-some-buffers)
  (cond
   ((>= arg 16) (nosetests-module))
   ((>= arg  4) (nosetests-one))
   (t           (nosetests-all))))


;;;;;;;;;;;;;;;;;
;;; Documentation

(defvar elpy-doc-history nil
  "History for the `elpy-doc' command.")

(defun elpy-doc-websearch (what)
  "Search the Python web documentation for the string WHAT."
  (interactive
   (list (read-from-minibuffer "Search Python.org for: "
                               (symbol-name (symbol-at-point)))))
  (browse-url
   (format "https://www.google.com/search?q=site:docs.python.org%%20%s"
           what)))

(defun elpy-doc (&optional use-pydoc-p symbol)
  "Show documentation on the thing at point.

If USE-PYDOC is non-nil (interactively, when a prefix argument is
given), use pydoc on the symbol SYMBOL (interactively, the symbol
at point). With a single prefix argument, the user gets a
completion interface for possible symbols. With two prefix
arguments,  the interface simply asks for a string."
  (interactive
   (list current-prefix-arg
         (let ((initial (with-syntax-table python-dotty-syntax-table
                          (let ((symbol (symbol-at-point)))
                            (if symbol
                                (symbol-name symbol)
                              nil)))))
           (cond
            ((and initial (not current-prefix-arg))
             initial)
            ((equal current-prefix-arg '(16))
             ;; C-u C-u
             (read-from-minibuffer "Pydoc: " initial nil nil
                                   'elpy-doc-history))
            (t
             (elpy-ido-recursive-completing-read "Pydoc: "
                                                 'elpy-pydoc-completions
                                                 "."
                                                 t
                                                 initial
                                                 'elpy-doc-history))))))
  (let ((doc (if use-pydoc-p
                 (elpy-rpc-get-pydoc-documentation symbol)
               (or (elpy-rpc-get-docstring)
                   ;; This will get the right position for
                   ;; multiprocessing.Queue(quxqux_|_)
                   (ignore-errors
                     (save-excursion
                       (elpy-nav-backward-statement)
                       (with-syntax-table python-dotty-syntax-table
                         (forward-symbol 1)
                         (backward-char 1))
                       (elpy-rpc-get-docstring)))))))
    (if doc
        (with-help-window "*Python Doc*"
          (with-current-buffer "*Python Doc*"
            (erase-buffer)
            (insert doc)
            (goto-char (point-min))
            (while (re-search-forward "\\(.\\)\\1" nil t)
              (replace-match (propertize (match-string 1)
                                         'face 'bold)
                             t t))))
      (message "No documentation available."))))

(defun elpy-pydoc-completions (rcr-prefix)
  "Return a list of modules available in pydoc starting with RCR-PREFIX."
  (sort (if (or (not rcr-prefix)
                (equal rcr-prefix ""))
            (elpy-rpc "get_pydoc_completions")
          (elpy-rpc "get_pydoc_completions" rcr-prefix))
        (lambda (a b)
          (if (and (string-prefix-p "_" b)
                   (not (string-prefix-p "_" a)))
              t
            (string< (downcase a)
                     (downcase b))))))


;;;;;;;;;;;;
;;; elpy-ido

;; This is a wrapper around ido-completing-read, which does not
;; provide for recursive reads by default.

(defvar elpy-ido-rcr-choice-function nil
  "Internal variable for `elpy-ido-recursive-completing-read'.

Don't touch. Won't help.")

(defvar elpy-ido-rcr-selection nil
  "Internal variable for `elpy-ido-recursive-completing-read'.

Don't touch. Won't help.")

(defvar elpy-ido-rcr-separator nil
  "Internal variable for `elpy-ido-recursive-completing-read'.

Don't touch. Won't help.")

(defvar elpy-ido-rcr-choices nil
  "Internal variable for `elpy-ido-recursive-completing-read'.

Don't touch. Won't help.")

(defun elpy-ido--rcr-selected ()
  "Return the currently selected compound."
  (mapconcat #'identity
             (reverse elpy-ido-rcr-selection)
             elpy-ido-rcr-separator))

(defun elpy-ido--rcr-setup-keymap ()
  "Set up the ido keymap for `elpy-ido-recursive-completing-read'."
  (define-key ido-completion-map (read-kbd-macro elpy-ido-rcr-separator)
    'elpy-ido-rcr-complete)
  (define-key ido-completion-map (kbd "DEL") 'elpy-ido-rcr-backspace))

(defun elpy-ido-rcr-complete ()
  "Complete the current ido completion and attempt an extension."
  (interactive)
  (let* ((new (car ido-matches))
         (full (concat (elpy-ido--rcr-selected)
                       elpy-ido-rcr-separator
                       new))
         (choices (funcall elpy-ido-rcr-choice-function full)))
    (when choices
      (setq elpy-ido-rcr-choices choices
            elpy-ido-rcr-selection (cons new elpy-ido-rcr-selection))
      (throw 'continue t))))

(defun elpy-ido-rcr-backspace (&optional n)
  "Delete the last character in the minibuffer.

If the minibuffer is empty, recurse to the last completion."
  (interactive "p")
  (if (= (minibuffer-prompt-end) (point))
      (progn
        (setq elpy-ido-rcr-selection (cdr elpy-ido-rcr-selection)
              elpy-ido-rcr-choices (funcall elpy-ido-rcr-choice-function
                                            (elpy-ido--rcr-selected)))
        (throw 'continue t))
    (delete-char (- n))))

(defun elpy-ido-recursive-completing-read (prompt choice-function
                                                  separator
                                                  &optional
                                                  require-match
                                                  initial-input
                                                  hist def)
  "An alternative to `ido-completing-read' supporting recursive selection.

The CHOICE-FUNCTION is called with a prefix string and should
find all possible selections with this prefix. The user is then
prompted with those options. When the user hits RET, the
currently selected option is returned. When the user hits the
SEPARATOR key, though, the currently selected option is appended,
with the separator, to the selected prefix, and the user is
prompted for further completions returned by CHOICE-FUNCTION.

For REQUIRE-MATCH, INITIAL-INPUT, HIST and DEF, see
`completing-read'."
  (let ((ido-setup-hook (cons 'elpy-ido--rcr-setup-keymap
                              ido-setup-hook))
        (elpy-ido-rcr-choice-function choice-function)
        (elpy-ido-rcr-separator separator)
        elpy-ido-rcr-choices
        elpy-ido-rcr-selection)
    (when initial-input
      (let ((parts (reverse (split-string initial-input
                                          (regexp-quote separator)))))
        (setq initial-input (car parts)
              elpy-ido-rcr-selection (cdr parts))))
    (setq elpy-ido-rcr-choices (funcall choice-function
                                        (elpy-ido--rcr-selected)))
    (catch 'return
      (while t
        (catch 'continue
          (throw 'return
                 (let ((completion (ido-completing-read
                                    (concat prompt
                                            (elpy-ido--rcr-selected)
                                            (if elpy-ido-rcr-selection
                                                elpy-ido-rcr-separator
                                              ""))
                                    elpy-ido-rcr-choices
                                    nil require-match
                                    initial-input hist def)))
                   (concat
                    (mapconcat (lambda (element)
                                 (concat element elpy-ido-rcr-separator))
                               (reverse elpy-ido-rcr-selection)
                               "")
                    completion))))
        ;; after the first run, we don't want initial and default
        ;; anymore.
        (setq initial-input nil
              def nil)))))


;;;;;;;;;;;;;;;;;;;;;
;;; elpy-rpc backends

;; elpy-rpc is a simple JSON-based RPC protocol. It's mostly JSON-RPC
;; 1.0, except we do not implement the full protocol as we do not need
;; all the features. Emacs starts a Python subprocess which runs a
;; special module. The module reads JSON-RPC requests and responds
;; with JSON-RPC responses.

(defvar elpy-rpc-call-id 0
  "Call id for the current elpy-rpc call.

See `elpy-rpc-call'.")
(make-variable-buffer-local 'elpy-rpc-call-id)

(defvar elpy-rpc-buffer-p nil
  "True iff the current buffer is an elpy-rpc buffer.")
(make-variable-buffer-local 'elpy-rpc-buffer-p)

(defvar elpy-rpc-buffer nil
  "The buffer-local elpy-rpc buffer.")
(make-variable-buffer-local 'elpy-rpc-buffer)

(defvar elpy-rpc-pool-table (make-hash-table :test 'equal)
  "Global hash table, which holds all associations between
project-root and elpy-rpc process buffers.")

(defun elpy-rpc-live-p (buffer)
  "Return non-nil when BUFFER is a live elpy-rpc process."
  (and buffer
       (get-buffer-process buffer)
       (process-live-p (get-buffer-process buffer))))

(defun elpy-rpc (method-name &rest params)
  "Run an elpy-rpc method on the elpy-rpc process."
  (elpy-rpc-ensure-open)
  (with-current-buffer elpy-rpc-buffer
    (apply 'elpy-rpc-call method-name params)))

(defun elpy-rpc-ensure-open ()
  "Ensure that the elpy-rpc subprocess is active."
  (when (not (elpy-rpc-live-p elpy-rpc-buffer))
    (when elpy-rpc-buffer
      (kill-buffer elpy-rpc-buffer))
    (condition-case err
        (setq elpy-rpc-buffer
              (elpy-rpc-pool-open (elpy-project-root) "*elpy-rpc*"
                                  elpy-rpc-python-command
                                  "-W" "ignore"
                                  "-m" "elpy.__main__"))
      (error
       (elpy-installation-instructions
        (format "Could not start the Python subprocess: %s"
                (cadr err))
        t)
       (error (cadr err))))
    (when (and elpy-rpc-backend
               (not (stringp elpy-rpc-backend)))
      (error "`elpy-rpc-backend' should be nil or a string."))
    (cond
     ;; User requested a backend that's not installed
     (elpy-rpc-backend
      (when (not (member elpy-rpc-backend (elpy-rpc-get-available-backends)))
        (elpy-installation-instructions
         (format (concat "The %s backend is unavailable. "
                         "Please install the appropriate Python library.")
                 elpy-rpc-backend))
        (error (format "Backend %s not found" elpy-rpc-backend)))
      (elpy-rpc-set-backend elpy-rpc-backend))
     ;; User did not specifically request the native backend, but it's
     ;; chosen by default.
     ((and (not elpy-rpc-backend)
           (equal "native" (elpy-rpc-get-backend)))
      (elpy-installation-instructions
       (concat "Only the basic native backend is available. "
               "You might want to install an appropriate "
               "Python library. If you are happy with the native "
               "backend, please add the following to your .emacs:"
               "\n\n(setq elpy-rpc-backend \"native\")"))))))

(defun elpy-rpc-restart ()
  "Restart the elpy-rpc subprocess if it is running.

Actually, just closes the elpy-rpc buffer"
  (interactive)
  (when elpy-rpc-buffer
    (kill-buffer elpy-rpc-buffer)
    (setq elpy-rpc-buffer nil)))

(defun elpy-rpc-pool-gc ()
  "Kill elpy-rpc processes and buffers, whose associated python
buffers are not available anymore."
  (interactive)
  (let (processes)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when elpy-mode
          (add-to-list 'processes elpy-rpc-buffer))))
    (maphash
     (lambda (project-root buffer)
       (unless (memq buffer processes)
         (remhash project-root elpy-rpc-pool-table)
         (when (elpy-rpc-live-p buffer)
           (kill-process (get-buffer-process buffer)))
         (kill-buffer buffer)))
     elpy-rpc-pool-table)))

(defun elpy-rpc-open-project-specific ()
  "Start a project specific elpy-rpc process and associate it
with the current project."
  (interactive)
  (let ((this-project-root (elpy-project-root)))
    (remhash this-project-root elpy-rpc-pool-table)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and elpy-mode
                   (equal this-project-root (elpy-project-root)))
          (setq elpy-rpc-buffer nil)))))
  (let ((elpy-rpc-project-specific t))
    (elpy-rpc-ensure-open)))

(defun elpy-rpc-pool-open (project-root name command &rest program-args)
  "Return a process buffer associated to the PROJECT-ROOT from
the elpy-rpc-pool-table cache.

Depending on the value of the elpy-rpc-project-specific variable
start a project specific process or just use/start a global one.

The parameters NAME, COMMAND and PROGRAM-ARGS are passed through
to the elpy-rpc-open function when necessary."
  (when (not elpy-rpc-project-specific)
    (setq project-root nil))
  (let ((cached (gethash project-root elpy-rpc-pool-table)))
    (if (elpy-rpc-live-p cached)
        cached
      (puthash project-root
               (apply 'elpy-rpc-open name command program-args)
               elpy-rpc-pool-table))))

(defun elpy-rpc-open (name program &rest program-args)
  "Start a new elpy-rpc subprocess.

NAME is a suggested name for the buffer and the name for the
process. The process will be PROGRAM called with PROGRAM-ARGS as
arguments.

This function returns the buffer created to communicate with
elpy-rpc. This buffer needs to be the current buffer for
subsequent calls to `elpy-rpc-call'."
  (let* ((buffer (elpy-generate-rpc-buffer name))
         ;; Leaving process-connection-type non-nil can truncate
         ;; communication
         (proc (let ((process-connection-type nil)
                     (default-directory "/"))
                 (apply #'start-process name buffer program program-args))))
    (set-process-query-on-exit-flag proc nil)
    (with-current-buffer buffer
      (setq elpy-rpc-buffer-p t)
      (let ((line (elpy-rpc--receive-line)))
        (cond
         ((equal line "elpy-rpc ready")
          buffer)
         ((string-match "No module named \\(.*\\)" line)
          (goto-char (point-min))
          (insert line "\n")
          (set-marker (process-mark proc) (point))
          (error (format "The Python module %s is not installed"
                         (match-string 1 line))))
         (t
          (goto-char (point-min))
          (insert line "\n")
          (set-marker (process-mark proc) (point))
          (error "Unknown output from Python elpy-rpc")))))))

(defun elpy-generate-rpc-buffer (name)
  "Return an elpy RPC buffer of name NAME.

Like `generate-new-buffer', but will also consider an old buffer
with no active process."
  (let* ((buf (get-buffer name))
        ;; (g-b-p nil) => nil
         (proc (get-buffer-process buf)))
    ;; When there is a buffer named like that, but it has no process,
    ;; or the process is not alive, kill it.
    (when (and buf
               (or (not proc)
                   (not (process-live-p proc))))
      (kill-buffer buf)))
  (generate-new-buffer name))

(defun elpy-rpc-call (method &rest params)
  "Call the METHOD with PARAMS on the current RPC server.

Ths current buffer needs to be an elpy-rpc buffer."
  (when (not elpy-rpc-buffer-p)
    (error "`elpy-rpc-call' called outside of an RPC buffer"))
  (erase-buffer)
  (setq elpy-rpc-call-id (1+ elpy-rpc-call-id))
  (elpy-rpc--send-json `((id . ,elpy-rpc-call-id)
                         (method . ,method)
                         (params . ,params)))
  (let ((response (elpy-rpc--receive-json)))
    (cond
     ((not (= elpy-rpc-call-id (cdr (assq 'id response))))
      (error "Protocol desynchronization, restart subprocess"))
     ((cdr (assq 'error response))
      (error (cdr (assq 'error response))))
     (t
      (cdr (assq 'result response))))))

(defun elpy-rpc--send-json (obj)
  "Send an object encoded as JSON to the current process."
  (process-send-string (get-buffer-process (current-buffer))
                       (format "%s\n" (json-encode obj))))

(defun elpy-rpc--receive-line ()
  "Read a single line from the current process."
  (let ((inhibit-quit nil))
    (while (not (progn
                  (goto-char (point-min))
                  (re-search-forward "^\\(.*\\)\n" nil t)))
      (accept-process-output)))
  (let ((line (match-string 1)))
    (replace-match "")
    line))

(defun elpy-rpc--receive-json ()
  "Read a single JSON object from the current process."
  (let ((json-array-type 'list))
    (json-read-from-string (elpy-rpc--receive-line))))

(defun elpy-rpc-get-completions ()
  "Call the find_completions API function.

Returns a list of possible completions for the Python symbol at
point."
  (when (elpy-project-root)
    (elpy-rpc "get_completions"
              (expand-file-name (elpy-project-root))
              buffer-file-name
              (buffer-string)
              (- (point)
                 (point-min)))))

(defun elpy-rpc-get-calltip ()
  "Call the get_calltip API function.

Returns a calltip string for the function call at point."
  (when (elpy-project-root)
    (elpy-rpc "get_calltip"
              (expand-file-name (elpy-project-root))
              buffer-file-name
              (buffer-string)
              (- (point)
                 (point-min)))))

(defun elpy-rpc-get-docstring ()
  "Call the get_docstring API function.

Returns a possible multi-line docstring for the symbol at point."
  (elpy-rpc "get_docstring"
            (expand-file-name (elpy-project-root))
            buffer-file-name
            (buffer-string)
            (- (point)
               (point-min))))

(defun elpy-rpc-get-pydoc-documentation (symbol)
  "Get the Pydoc documentation for SYMBOL.

Returns a possible multi-line docstring."
    (elpy-rpc "get_pydoc_documentation" symbol))

(defun elpy-rpc-get-definition ()
  "Call the find_definition API function.

Returns nil or a list of (filename, point)."
  (elpy-rpc "get_definition"
            (expand-file-name (elpy-project-root))
            buffer-file-name
            (buffer-string)
            (- (point)
               (point-min))))

(defun elpy-rpc-before-save ()
  "Call the before_save API function.

Used for state keeping in the backend."
  ;; If there is no backend, we do not need to keep state.
  (when elpy-rpc-buffer
    (elpy-rpc "before_save"
              (expand-file-name (elpy-project-root))
              buffer-file-name)))

(defun elpy-rpc-after-save ()
  "Call the after_save API function.

Used for state keeping in the backend."
  ;; If there is no backend, we do not need to keep state.
  (when elpy-rpc-buffer
    (elpy-rpc "after_save"
              (expand-file-name (elpy-project-root))
              buffer-file-name)))

(defun elpy-rpc-get-backend ()
  "Call the get_backend API function.

Returns the name of the backend currently in use."
  (elpy-rpc "get_backend"))

(defun elpy-rpc-get-available-backends ()
  "Call the get_available_backends API function.

Returns a list of names of available backends, depending on which
Python libraries are installed."
  (elpy-rpc "get_available_backends"))

(defun elpy-rpc-set-backend (backend)
  "Call the set_backend API function.

This changes the current backend to the named backend. Raises an
error if the backend is not supported."
  (elpy-rpc "set_backend" backend))

(defun elpy-set-backend (backend)
  "Interactively change the backend used by elpy.

To set the preferred backend in your .emacs, use the
`elpy-rpc-backend' variable."
  (interactive
   (list (completing-read
          (format "Switch elpy backend (currently %s): "
                  (elpy-rpc-get-backend))
          (elpy-rpc-get-available-backends)
          nil t)))
  (elpy-rpc-set-backend backend))


;;;;;;;;;
;;; Eldoc

(defun elpy-eldoc-documentation ()
  "Return a call tip for the python call at point."
  (let ((calltip (elpy-rpc-get-calltip)))
    (when calltip
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
        ;; possibly quite complex argument values making calculation of
        ;; the current argument tricky.

        ;; Hence, we don't do anything for now.
        (buffer-string)))))


;;;;;;;;;;;
;;; Flymake

(eval-after-load "flymake"
  '(add-to-list 'flymake-allowed-file-name-masks
                '("\\.py\\'" elpy-flymake-python-init)))

(defun elpy-flymake-python-init ()
  ;; Make sure it's not a remote buffer as flymake would not work
  (when (not (file-remote-p buffer-file-name))
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace)))
      (list python-check-command (list temp-file)))))

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
  (let* ((lineno (flymake-current-line-no))
         (err-info (car (flymake-find-err-info flymake-err-info
                                               lineno)))
         (text (mapconcat #'flymake-ler-text
                          err-info
                          ", ")))
    (message "%s" text)))


;;;;;;;;
;;; nose

(eval-after-load "nose"
  '(defalias 'nose-find-project-root 'elpy-project-find-root))


;;;;;;;;;;;;;
;;; Yasnippet

;; No added configuration needed. Nice mode. :o)


;;;;;;;;;;;;;;;;;
;;; Auto-Complete

(defvar elpy--ac-cache nil
  "List of current expansions and docstrings.")

(defun elpy--ac-candidates ()
  "Return a list of possible expansions at points.

This also initializes `elpy--ac-cache'."
  (setq elpy--ac-cache nil)
  (dolist (completion (condition-case err
                          (elpy-rpc-get-completions)
                        (error
                         (message "Getting completions: %s"
                                  (cadr err))
                         nil)))
    (let ((name (car completion))
          (doc (cadr completion)))
      (when (not (string-prefix-p "_" name))
        (push (cons (concat ac-prefix name)
                    doc)
              elpy--ac-cache))))
  (mapcar #'car elpy--ac-cache))

(defun elpy--ac-document (name)
  "Return the documentation for the symbol NAME."
  (assoc-default name elpy--ac-cache))

(ac-define-source elpy
  '((candidates . elpy--ac-candidates)
    (symbol     . "p")
    (document   . elpy--ac-document)
    (cache      . t)))

(ac-define-source elpy-dot
  '((candidates . elpy--ac-candidates)
    (symbol     . "p")
    (document   . elpy--ac-document)
    (cache      . t)
    (prefix     . c-dot)
    (requires   . 0)))


;;;;;;;;;;;;;;
;;; Virtualenv

(defadvice virtualenv-workon (around ad-elpy-virtualenv-workon activate)
  "Restart the elpy-rpc backend on virtualenv change."
  (let ((old-env virtualenv-workon-session))
    ad-do-it
    (when (and virtualenv-workon-starts-python
               elpy-rpc-buffer
               (not (equal old-env virtualenv-workon-session))
               (y-or-n-p "Virtualenv changed, restart Elpy-RPC? "))
      (elpy-rpc-restart))))

(defadvice virtualenv-deactivate (after ad-elpy-virtualenv-deactivate activate)
  "Restart the elpy-rpc backend on virtualenv change."
  (when (and virtualenv-workon-starts-python
             elpy-rpc-buffer
             (y-or-n-p "Virtualenv deactivated, restart Elpy-RPC? "))
    (elpy-rpc-restart)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Backwards compatibility

;; Functions for Emacs 24 before 24.3
(when (not (fboundp 'python-shell-send-region))
  (defalias 'python-shell-send-region 'python-send-region))
(when (not (fboundp 'python-shell-send-buffer))
  (defalias 'python-shell-send-buffer 'python-send-buffer))
(when (not (fboundp 'python-info-current-defun))
  (defalias 'python-info-current-defun 'python-current-defun))
(when (not (fboundp 'python-nav-end-of-statement))
  (defalias 'python-nav-end-of-statement 'python-end-of-statement))
(when (not (fboundp 'python-nav-beginning-of-statement))
  (require 'thingatpt)
  (defalias 'python-nav-beginning-of-statement 'beginning-of-sexp))
(when (not (fboundp 'python-nav-forward-statement))
  (defalias 'python-nav-forward-statement 'forward-sexp))
(when (not (fboundp 'python-nav-backward-statement))
  (defalias 'python-nav-backward-statement 'backward-sexp))

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
     ((and (= on-or-off 1)
           (not highlight-indent-active))
      (highlight-indentation))
     ((and (= on-or-off 0)
           highlight-indent-active)
      (highlight-indentation)))))

(provide 'elpy)
;;; elpy.el ends here
