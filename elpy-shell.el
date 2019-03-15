;;; elpy-shell.el --- Interactive Python support for elpy -*- lexical-binding: t -*-
;;
;; Copyright (C) 2012-2016  Jorgen Schaefer
;;
;; Author: Jorgen Schaefer <contact@jorgenschaefer.de>, Rainer Gemulla <rgemulla@gmx.de>
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
;; Adds support for interactive Python to elpy
;;
;;; Code:

(eval-when-compile (require 'subr-x))
(require 'pyvenv)
(require 'python)

;;;;;;;;;;;;;;;;;;;;;;
;;; User customization

(defcustom elpy-dedicated-shells nil
  "Non-nil if Elpy should use dedicated shells.

Elpy can use a unique Python shell for all buffers and support
manually started dedicated shells. Setting this option to non-nil
force the creation of dedicated shells for each buffers."
  :type 'boolean
  :group 'elpy)
(make-obsolete-variable 'elpy-dedicated-shells
                        "Dedicated shells are no longer supported by Elpy.
You can use `(add-hook 'elpy-mode-hook (lambda () (elpy-shell-toggle-dedicated-shell 1)))' to achieve the same result."
                        "1.17.0")

(defcustom elpy-shell-display-buffer-after-send nil ;
  "Whether to display the Python shell after sending something to it."
  :type 'boolean
  :group 'elpy)

(defcustom elpy-shell-echo-output 'when-shell-not-visible
  "Whether to echo the Python shell output in the echo area after input has been sent to the shell.

  Possible choices are nil (=never), `when-shell-not-visible', or
  t (=always)."
  :type '(choice (const :tag "Never" nil)
                 (const :tag "When shell not visible" when-shell-not-visible)
                 (const :tag "Always" t))
  :group 'elpy)

(defcustom elpy-shell-capture-last-multiline-output t
  "Whether to capture the output of the last Python statement when sending multiple statements to the Python shell.

  If nil, no output is captured (nor echoed in the shell) when
  sending multiple statements. This is the default behavior of
  python.el. If non-nil and the last statement is an expression,
  captures its output so that it is echoed in the shell."
  :type 'boolean
  :group 'elpy)
(make-obsolete-variable 'elpy-shell-capture-last-multiline-output
                        "The last multiline output is now always captured."
                        "February 2019")

(defcustom elpy-shell-echo-input t
  "Whether to echo input sent to the Python shell as input in the
shell buffer.

Truncation of long inputs can be controlled via
`elpy-shell-echo-input-lines-head' and
`elpy-shell-echo-input-lines-tail'."
  :type 'boolean
  :group 'elpy)

(defcustom elpy-shell-echo-input-cont-prompt t
  "Whether to show a continuation prompt when echoing multi-line
input to the Python shell."
  :type 'boolean
  :group 'elpy)

(defcustom elpy-shell-echo-input-lines-head 10
  "Maximum number of lines to show before truncating input echoed
in the Python shell."
  :type 'integer
  :group 'elpy)

(defcustom elpy-shell-echo-input-lines-tail 10
  "Maximum number of lines to show after truncating input echoed
in the Python shell."
  :type 'integer
  :group 'elpy)

(defcustom elpy-shell-use-project-root t
  "Whether to use project root as default directory when starting a Python shells.

The project root is determined using `elpy-project-root`. If this variable is set to
nil, the current directory is used instead."
  :type 'boolean
  :group 'elpy)

(defcustom elpy-shell-cell-boundary-regexp
  (concat "^\\(?:"
          "##.*" "\\|"
          "#\\s-*<.+>" "\\|"
          "#\\s-*\\(?:In\\|Out\\)\\[.*\\]:"
          "\\)\\s-*$")
  "Regular expression for matching a line indicating the boundary
of a cell (beginning or ending). By default, lines starting with
``##`` are treated as a cell boundaries, as are the boundaries in
Python files exported from IPython or Jupyter notebooks (e.g.,
``# <markdowncell>``, ``# In[1]:'', or ``# Out[1]:``)."
  :type 'string
  :group 'elpy)

(defcustom elpy-shell-codecell-beginning-regexp
  (concat "^\\(?:"
          "##.*" "\\|"
          "#\\s-*<codecell>" "\\|"
          "#\\s-*In\\[.*\\]:"
          "\\)\\s-*$")
  "Regular expression for matching a line indicating the
beginning of a code cell. By default, lines starting with ``##``
are treated as beginnings of a code cell, as are the code cell
beginnings (and only the code cell beginnings) in Python files
exported from IPython or Jupyter notebooks (e.g., ``#
<codecell>`` or ``# In[1]:``).

Note that `elpy-shell-cell-boundary-regexp' must also match
the code cell beginnings defined here."
  :type 'string
  :group 'elpy)


;;;;;;;;;;;;;;;;;;
;;; Shell commands

(defvar elpy--shell-last-py-buffer nil
  "Help keep track of python buffer when changing to pyshell.")

(defun elpy-shell-display-buffer ()
  "Display inferior Python process buffer."
  (display-buffer (process-buffer (elpy-shell-get-or-create-process))
                  nil
                  'visible))

;; better name would be pop-to-shell
(defun elpy-shell-switch-to-shell ()
  "Switch to inferior Python process buffer."
  (interactive)
  (setq elpy--shell-last-py-buffer (buffer-name))
  (pop-to-buffer (process-buffer (elpy-shell-get-or-create-process))))

(defun elpy-shell-switch-to-buffer ()
  "Switch from inferior Python process buffer to recent Python buffer."
  (interactive)
  (pop-to-buffer elpy--shell-last-py-buffer))

(defun elpy-shell-switch-to-shell-in-current-window ()
  (interactive)
  (setq elpy--shell-last-py-buffer (buffer-name))
  (switch-to-buffer (process-buffer (elpy-shell-get-or-create-process))))

(defun elpy-shell-switch-to-buffer-in-current-window ()
  (interactive)
  (switch-to-buffer elpy--shell-last-py-buffer))

(defun elpy-shell-kill (&optional kill-buff)
  "Kill the current python shell.

If KILL-BUFF is non-nil, also kill the associated buffer."
  (interactive)
  (let ((shell-buffer (python-shell-get-buffer)))
    (cond
     (shell-buffer
      (delete-process shell-buffer)
      (when kill-buff
	(kill-buffer shell-buffer))
      (message "Killed %s shell" shell-buffer))
     (t
      (message "No python shell to kill")))))

(defun elpy-shell-kill-all (&optional kill-buffers ask-for-each-one)
  "Kill all active python shells.

If KILL-BUFFERS is non-nil, also kill the associated buffers.
If ASK-FOR-EACH-ONE is non-nil, ask before killing each python process."
  (interactive)
  (let ((python-buffer-list ()))
    ;; Get active python shell buffers and kill inactive ones (if asked)
    (cl-loop for buffer being the buffers do
	  (when (and (buffer-name buffer)
		     (string-match (rx bol "*Python" (opt "[" (* (not (any "]"))) "]") "*" eol)
				   (buffer-name buffer)))
	    (if (get-buffer-process buffer)
		(push buffer python-buffer-list)
	      (when kill-buffers
		(kill-buffer buffer)))))
    (cond
     ;; Ask for each buffers and kill
     ((and python-buffer-list ask-for-each-one)
      (cl-loop for buffer in python-buffer-list do
	    (when (y-or-n-p (format "Kill %s ? " buffer))
		(delete-process buffer)
		(when kill-buffers
		  (kill-buffer buffer)))))
     ;; Ask and kill every buffers
     (python-buffer-list
      (if (y-or-n-p (format "Kill %s python shells ? " (length python-buffer-list)))
	  (cl-loop for buffer in python-buffer-list do
		(delete-process buffer)
		(when kill-buffers
		  (kill-buffer buffer)))))
     ;; No shell to close
     (t
      (message "No python shell to close")))))

(defun elpy-shell-get-or-create-process (&optional sit)
  "Get or create an inferior Python process for current buffer and return it.

If SIT is non-nil, sit for that many seconds after creating a
Python process. This allows the process to start up."
  (let* ((bufname (format "*%s*" (python-shell-get-process-name nil)))
         (proc (get-buffer-process bufname)))
    (if proc
        proc
      (when (not (executable-find python-shell-interpreter))
        (error "Python shell interpreter `%s' cannot be found. Please set `python-shell-interpreter' to an valid python binary."
               python-shell-interpreter))
      (let ((default-directory (or (and elpy-shell-use-project-root
                                        (elpy-project-root))
                                   default-directory)))
        (run-python (python-shell-parse-command) nil t))
      (when sit (sit-for sit))
      (when (elpy-project-root)
        (python-shell-send-string
         (format "import sys;sys.path.append('%s')" (elpy-project-root))))
      (get-buffer-process bufname))))

(defun elpy-shell-toggle-dedicated-shell (&optional arg)
  "Toggle the use of a dedicated python shell for the current buffer.

if ARG is positive, enable the use of a dedicated shell.
if ARG is negative or 0, disable the use of a dedicated shell."
  (interactive)
  (let ((arg (or arg
                 (if (local-variable-p 'python-shell-buffer-name) 0 1))))
    (if (<= arg 0)
        (kill-local-variable 'python-shell-buffer-name)
      (setq-local python-shell-buffer-name
                  (format "Python[%s]"
                          (file-name-sans-extension
                          (buffer-name)))))))

(defun elpy-shell-set-local-shell (&optional shell-name)
  "Associate the current buffer to a specific shell.

Meaning that the code from the current buffer will be sent to this shell.

If SHELL-NAME is not specified, ask with completion for a shell name.

If SHELL-NAME is \"Global\", associate the current buffer to the main python
shell (often \"*Python*\" shell)."
  (interactive)
  (let* ((current-shell-name (if (local-variable-p 'python-shell-buffer-name)
                                 (progn
                                   (string-match "Python\\[\\(.*?\\)\\]"
                                                 python-shell-buffer-name)
                                   (match-string 1 python-shell-buffer-name))
                               "Global"))
         (shell-names (cl-loop
                for buffer in (buffer-list)
                for buffer-name = (file-name-sans-extension (substring-no-properties (buffer-name buffer)))
                if (string-match "\\*Python\\[\\(.*?\\)\\]\\*" buffer-name)
                collect (match-string 1 buffer-name)))
         (candidates (remove current-shell-name
                           (delete-dups
                           (append (list (file-name-sans-extension
                                          (buffer-name)) "Global")
                                   shell-names))))
         (prompt (format "Shell name (current: %s): " current-shell-name))
         (shell-name (or shell-name (completing-read prompt candidates))))
    (if (string= shell-name "Global")
       (kill-local-variable 'python-shell-buffer-name)
      (setq-local python-shell-buffer-name (format "Python[%s]" shell-name)))))

(defun elpy-shell--ensure-shell-running ()
  "Ensure that the Python shell for the current buffer is running.

If the shell is not running, waits until the first prompt is visible and
commands can be sent to the shell."
  (with-current-buffer (process-buffer (elpy-shell-get-or-create-process))
    (let ((cumtime 0))
      (while (and (when (boundp 'python-shell--first-prompt-received)
                    (not python-shell--first-prompt-received))
                  (< cumtime 3))
        (sleep-for 0.1)
        (setq cumtime (+ cumtime 0.1)))))
  (elpy-shell-get-or-create-process))

(defun elpy-shell--string-without-indentation (string)
  "Return the current string, but without indentation."
  (if (string-empty-p string)
      string
    (let ((indent-level nil)
          (indent-tabs-mode nil))
      (with-temp-buffer
        (insert string)
        (goto-char (point-min))
        (while (< (point) (point-max))
          (cond
           ((or (elpy-shell--current-line-only-whitespace-p)
                (python-info-current-line-comment-p)))
           ((not indent-level)
            (setq indent-level (current-indentation)))
           ((and indent-level
                 (< (current-indentation) indent-level))
            (error (message "X%sX" (thing-at-point 'line)))))
          ;; (error "Can't adjust indentation, consecutive lines indented less than starting line")))
          (forward-line))
        (indent-rigidly (point-min)
                        (point-max)
                        (- indent-level))
        ;; 'indent-rigidly' introduces tabs despite the fact that 'indent-tabs-mode' is nil
        ;; 'untabify' fix that
        (untabify (point-min) (point-max))
        (buffer-string)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flash input sent to shell

;; functions for flashing a region; only flashes when package eval-sexp-fu is
;; loaded and its minor mode enabled
(defun elpy-shell--flash-and-message-region (begin end)
  "Displays information about code fragments sent to the shell.

BEGIN and END refer to the region of the current buffer containing the code being sent. Displays a message with the first line of that region. If `eval-sexp-fu-flash-mode' is active, additionally flashes that region briefly."
  (when (> end begin)
    (save-excursion
      (goto-char begin)
      (end-of-line)
      (if (<= end (point))
          (message "Sent: %s" (string-trim (thing-at-point 'line)))
        (message "Sent: %s..." (string-trim (thing-at-point 'line)))))
    (when (bound-and-true-p eval-sexp-fu-flash-mode)
      (multiple-value-bind (_bounds hi unhi _eflash)
          (eval-sexp-fu-flash (cons begin end))
        (eval-sexp-fu-flash-doit (lambda () t) hi unhi)))))

;;;;;;;;;;;;;;;;;;;
;; Helper functions

(defun elpy-shell--current-line-else-or-elif-p ()
  (eq (string-match-p "\\s-*el\\(?:se:\\|if[^\w]\\)" (thing-at-point 'line)) 0))

(defun elpy-shell--current-line-indented-p ()
  (eq (string-match-p "\\s-+[^\\s-]+" (thing-at-point 'line)) 0))

(defun elpy-shell--current-line-only-whitespace-p ()
  "Whether the current line contains only whitespace characters (or is empty)."
  (eq (string-match-p "\\s-*$" (thing-at-point 'line)) 0))

(defun elpy-shell--current-line-code-line-p ()
  (and (not (elpy-shell--current-line-only-whitespace-p))
       (not (python-info-current-line-comment-p))))

(defun elpy-shell--current-line-defun-p ()
  "Whether a function definition starts at the current line."
  (eq (string-match-p
       "\\s-*\\(?:def\\|async\\s-+def\\)\\s\-"
       (thing-at-point 'line))
      0))

(defun elpy-shell--current-line-defclass-p ()
  "Whether a class definition starts at the current line."
  (eq (string-match-p
       "\\s-*class\\s\-"
       (thing-at-point 'line))
      0))

(defun elpy-shell--skip-to-next-code-line (&optional backwards)
  "Move the point to the next line containing code.

If the current line has code, point is not moved. If BACKWARDS is
non-nil, skips backwards."
  (if backwards
      (while (and (not (elpy-shell--current-line-code-line-p))
                  (not (eq (point) (point-min))))
        (forward-line -1))
    (while (and (not (elpy-shell--current-line-code-line-p))
                (not (eq (point) (point-max))))
      (forward-line))))

(defun elpy-shell--check-if-shell-available ()
  "Check if the associated python shell is available.

Return non-nil is the shell is running and not busy, nil otherwise."
  (and (python-shell-get-process)
       (with-current-buffer (process-buffer (python-shell-get-process))
         (save-excursion
           (goto-char (point-max))
           (let ((inhibit-field-text-motion t))
             (python-shell-comint-end-of-output-p
              (buffer-substring (line-beginning-position)
                                (line-end-position))))))))
;;;;;;;;;;
;; Echoing

(defmacro elpy-shell--with-maybe-echo (body)
  ;; Echoing is apparently buggy for emacs < 25...
  (if (<= 25 emacs-major-version)
      `(elpy-shell--with-maybe-echo-output
        (elpy-shell--with-maybe-echo-input
         ,body))
    body))


(defmacro elpy-shell--with-maybe-echo-input (body)
  "Run BODY so that it adheres `elpy-shell-echo-input' and `elpy-shell-display-buffer'."
  `(progn
     (elpy-shell--enable-echo)
     (prog1
         (if elpy-shell-display-buffer-after-send
             (prog1 (progn ,body)
               (elpy-shell-display-buffer))
           (cl-flet ((elpy-shell-display-buffer () ()))
             (progn ,body)))
       (elpy-shell--disable-echo))))

(defvar-local elpy-shell--capture-output nil
  "Non-nil when the Python shell should capture output for display in the echo area.")

(defvar-local elpy-shell--captured-output nil
  "Current captured output of the Python shell.")

(defmacro elpy-shell--with-maybe-echo-output (body)
  "Run BODY and grab shell output according to `elpy-shell-echo-output'."
  `(cl-letf (((symbol-function 'python-shell-send-file)
              (if elpy-shell-echo-output
                  (symbol-function 'elpy-shell-send-file)
                (symbol-function 'python-shell-send-file))))
     (let* ((process (elpy-shell--ensure-shell-running))
            (process-buf (process-buffer process))
            (shell-visible (or elpy-shell-display-buffer-after-send
                               (get-buffer-window process-buf))))
       (with-current-buffer process-buf
         (setq-local elpy-shell--capture-output
                     (and elpy-shell-echo-output
                          (or (not (eq elpy-shell-echo-output 'when-shell-not-visible))
                              (not shell-visible)))))
       (progn ,body))))

(defun elpy-shell--enable-output-filter ()
    (add-hook 'comint-output-filter-functions 'elpy-shell--output-filter nil t))

(defun elpy-shell--output-filter (string)
  "Filter used in `elpy-shell--with-maybe-echo-output' to grab output.

No actual filtering is performed. STRING is the output received
to this point from the process. If `elpy-shell--capture-output'
is set, captures and messages shell output in the echo area (once
complete). Otherwise, does nothing."
  ;; capture the output and message it when complete
  (when elpy-shell--capture-output
    ;; remember the new output
    (setq-local elpy-shell--captured-output
                (concat elpy-shell--captured-output (ansi-color-filter-apply string)))

    ;; Output ends when `elpy-shell--captured-output' contains
    ;; the prompt attached at the end of it. If so, message it.
    (when (python-shell-comint-end-of-output-p elpy-shell--captured-output)
      (let ((output (substring
                     elpy-shell--captured-output
                     0 (match-beginning 0)))
            (message-log-max))
        (if (string-match-p "Traceback (most recent call last):" output)
            (message "Exception during evaluation.")
          (if (string-empty-p output)
              (message "No output was produced.")
            (message "%s" (replace-regexp-in-string "\n\\'" "" output))))
        (setq-local elpy-shell--captured-output nil))))

  ;; return input unmodified
  string)

(defun elpy-shell--insert-and-font-lock (string face &optional no-font-lock)
  "Inject STRING into the Python shell buffer."
  (let ((from-point (point)))
    (insert string)
    (if (not no-font-lock)
        (add-text-properties from-point (point)
                             (list 'front-sticky t 'font-lock-face face)))))

(defun elpy-shell--append-to-shell-output (string &optional no-font-lock prepend-cont-prompt)
  "Append the given STRING to the output of the Python shell buffer.

Unless NO-FONT-LOCK is set, formats STRING as shell input.
Prepends a continuation promt if PREPEND-CONT-PROMPT is set."
  (when (not (string-empty-p string))
  (let* ((process (elpy-shell-get-or-create-process))
         (process-buf (process-buffer process))
         (mark-point (process-mark process)))
    (with-current-buffer process-buf
      (save-excursion
        (goto-char mark-point)
        (if prepend-cont-prompt
            (let* ((column (+ (- (point) (progn (forward-line -1) (end-of-line) (point))) 1))
                   (prompt (concat (make-string (max 0 (- column 7)) ? ) "...: "))
                   (lines (split-string string "\n")))
              (goto-char mark-point)
              (elpy-shell--insert-and-font-lock
               (car lines) 'comint-highlight-input no-font-lock)
              (when (cdr lines)
                  ;; no additional newline at end for multiline
                  (dolist (line (cdr lines))
                    (insert "\n")
                    (elpy-shell--insert-and-font-lock
                     prompt 'comint-highlight-prompt no-font-lock)
                    (elpy-shell--insert-and-font-lock
                     line 'comint-highlight-input no-font-lock)))
                ;; but put one for single line
                (insert "\n"))
          (elpy-shell--insert-and-font-lock
           string 'comint-highlight-input no-font-lock))
        (set-marker (process-mark process) (point)))))))

(defun elpy-shell--string-head-lines (string n)
  "Extract the first N lines from STRING."
  (let* ((line "\\(?:\\(?:.*\n\\)\\|\\(?:.+\\'\\)\\)")
         (lines (concat line "\\{" (number-to-string n) "\\}"))
         (regexp (concat "\\`" "\\(" lines "\\)")))
    (if (string-match regexp string)
        (match-string 1 string)
      string)))

(defun elpy-shell--string-tail-lines (string n)
  "Extract the last N lines from STRING."
  (let* ((line "\\(?:\\(?:.*\n\\)\\|\\(?:.+\\'\\)\\)")
         (lines (concat line "\\{" (number-to-string n) "\\}"))
         (regexp (concat "\\(" lines "\\)" "\\'")))
    (if (string-match regexp string)
        (match-string 1 string)
      string)))

(defun elpy-shell--python-shell-send-string-echo-advice (string &optional _process _msg)
  "Advice to enable echoing of input in the Python shell."
  (interactive)
  (let* ((append-string ; strip setup code from Elpy
          (if (string-match "import sys, codecs, os, ast;__pyfile = codecs.open.*$" string)
              (replace-match "" nil nil string)
            string))
         (append-string ; strip setup code from python.el
          (if (string-match "import codecs, os;__pyfile = codecs.open(.*;exec(compile(__code, .*$" append-string)
              (replace-match "" nil nil append-string)
            append-string))
         (append-string ; here too
          (if (string-match "^# -\\*- coding: utf-8 -\\*-\n*$" append-string)
              (replace-match "" nil nil append-string)
            append-string))
         (append-string ; Strip "if True:", added when sending regions
          (if (string-match "^if True:$" append-string)
              (replace-match "" nil nil append-string)
            append-string))
         (append-string ; strip newlines from beginning and white space from end
          (string-trim-right
           (if (string-match "\\`\n+" append-string)
               (replace-match "" nil nil append-string)
             append-string)))
         (append-string ; Dedent region
          (elpy-shell--string-without-indentation append-string))
         (head (elpy-shell--string-head-lines append-string elpy-shell-echo-input-lines-head))
         (tail (elpy-shell--string-tail-lines append-string elpy-shell-echo-input-lines-tail))
         (append-string (if (> (length append-string) (+ (length head) (length tail)))
                            (concat head "...\n" tail)
                          append-string)))

    ;; append the modified string to the shell output; prepend a newline for
    ;; multi-line strings
    (if elpy-shell-echo-input-cont-prompt
        (elpy-shell--append-to-shell-output append-string nil t)
      (elpy-shell--append-to-shell-output
       (concat (if (string-match "\n" append-string) "\n" "")
               append-string
               "\n")))))

(defun elpy-shell--enable-echo ()
  "Enable input echoing when `elpy-shell-echo-input' is set."
  (when elpy-shell-echo-input
    (advice-add 'python-shell-send-string
                :before 'elpy-shell--python-shell-send-string-echo-advice)))

(defun elpy-shell--disable-echo ()
  "Disable input echoing."
  (advice-remove 'python-shell-send-string
                 'elpy-shell--python-shell-send-string-echo-advice))

(defun elpy-shell-send-file (file-name &optional process temp-file-name
                                         delete msg)
  "Like `python-shell-send-file' but evaluates last expression separately.

See `python-shell-send-file' for a description of the
arguments. This function differs in that it breaks up the
Python code in FILE-NAME into statements. If the last statement
is a Python expression, it is evaluated separately in 'eval'
mode. This way, the interactive python shell can capture (and
print) the output of the last expression."
  (interactive
   (list
    (read-file-name "File to send: ")   ; file-name
    nil                                 ; process
    nil                                 ; temp-file-name
    nil                                 ; delete
    t))                                 ; msg
  (let* ((process (or process (python-shell-get-process-or-error msg)))
         (encoding (with-temp-buffer
                     (insert-file-contents
                      (or temp-file-name file-name))
                     (python-info-encoding)))
         (file-name (expand-file-name
                     (or (file-remote-p file-name 'localname)
                         file-name)))
         (temp-file-name (when temp-file-name
                           (expand-file-name
                            (or (file-remote-p temp-file-name 'localname)
                                temp-file-name)))))
    (python-shell-send-string
     (format
      (concat
       "import sys, codecs, os, ast;"
       "__pyfile = codecs.open('''%s''', encoding='''%s''');"
       "__code = __pyfile.read().encode('''%s''');"
       "__pyfile.close();"
       (when (and delete temp-file-name)
         (format "os.remove('''%s''');" temp-file-name))
       "__block = ast.parse(__code, '''%s''', mode='exec');"
       ;; Has to ba a oneliner, which make conditionnal statements a bit complicated...
       " __block.body = (__block.body if not isinstance(__block.body[0], ast.If) else __block.body if not isinstance(__block.body[0].test, ast.Name) else __block.body if not __block.body[0].test.id == 'True' else __block.body[0].body) if sys.version_info[0] < 3 else (__block.body if not isinstance(__block.body[0], ast.If) else __block.body if not isinstance(__block.body[0].test, ast.NameConstant) else __block.body if not __block.body[0].test.value is True else __block.body[0].body);"
       "__last = __block.body[-1];" ;; the last statement
       "__isexpr = isinstance(__last,ast.Expr);" ;; is it an expression?
       "_ = __block.body.pop() if __isexpr else None;" ;; if so, remove it
       "exec(compile(__block, '''%s''', mode='exec'));" ;; execute everything else
       "eval(compile(ast.Expression(__last.value), '''%s''', mode='eval')) if __isexpr else None" ;; if it was an expression, it has been removed; now evaluate it
       )
      (or temp-file-name file-name) encoding encoding file-name file-name file-name)
     process)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation commands for sending

(defun elpy-shell--nav-beginning-of-statement ()
  "Move the point to the beginning of the current or next Python statement.

If the current line starts with a statement, behaves exactly like
`python-nav-beginning-of-statement'. If the line is part of a
statement but not a statement itself, goes backwards to the
beginning of the statement. If the current line is not a code
line, skips forward to the next code line and navigates from
there."
  (elpy-shell--skip-to-next-code-line)
  (python-nav-beginning-of-statement)
  (let ((p))
    (while (and (not (eq p (point)))
                (elpy-shell--current-line-else-or-elif-p))
      (elpy-nav-backward-block)
      (setq p (point)))))

(defun elpy-shell--nav-end-of-statement ()
  "Move the point to the end of the current Python statement.

Assumes that the point is precisely at the beginning of a
statement (e.g., after calling
`elpy-shell--nav-beginning-of-statement')."
  (let ((continue t)
        (p))
    (while (and (not (eq p (point)))
                continue)
      ;; check if there is a another block at the same indentation level
      (setq p (point))
      (elpy-nav-forward-block)

      ;; if not, go to the end of the block and done
      (if (eq p (point))
          (progn
            (python-nav-end-of-block)
            (setq continue nil))
        ;; otherwise check if its an else/elif clause
        (unless (elpy-shell--current-line-else-or-elif-p)
          (forward-line -1)
          (elpy-shell--skip-to-next-code-line t)
          (setq continue nil)))))
  (end-of-line))

(defun elpy-shell--nav-beginning-of-top-statement ()
  "Move the point to the beginning of the current or next top-level statement.

If the point is within a top-level statement, moves to its
beginning. Otherwise, moves to the beginning of the next top-level
statement."
  (interactive)
  (elpy-shell--nav-beginning-of-statement)
  (let ((p))
    (while (and (not (eq p (point)))
                (elpy-shell--current-line-indented-p))
      (forward-line -1)
      (elpy-shell--skip-to-next-code-line t)
      (elpy-shell--nav-beginning-of-statement))))

(defun elpy-shell--nav-beginning-of-def (def-p)
  "Move point to the beginning of the current definition.

DEF-P is a predicate function that decides whether the current
line starts a definition.

It the current line starts a definition, uses this definition. If
the current line does not start a definition and is a code line,
searches for the definition that contains the current line.
Otherwise, searches for the definition that contains the next
code line.

If a definition is found, moves point to the start of the
definition and returns t. Otherwise, retains point position and
returns nil."
  (if (funcall def-p)
      (progn
        (python-nav-beginning-of-statement)
        t)
    (let ((beg-ts (save-excursion
                    (elpy-shell--skip-to-next-code-line t)
                    (elpy-shell--nav-beginning-of-top-statement)
                    (point)))
          (orig-p (point))
          (max-indent (save-excursion
                        (elpy-shell--skip-to-next-code-line)
                        (- (current-indentation) 1)))
          (found))
      (while (and (not found)
                  (>= (point) beg-ts))
        (if (and (funcall def-p)
                 (<= (current-indentation) max-indent))
            (setq found t)
          (when (elpy-shell--current-line-code-line-p)
            (setq max-indent (min max-indent
                                  (- (current-indentation) 1))))
          (forward-line -1)))
      (if found
          (python-nav-beginning-of-statement)
        (goto-char orig-p))
      found)))

(defun elpy-shell--nav-beginning-of-defun ()
  "Move point to the beginning of the current function definition.

If a definition is found, moves point to the start of the
definition and returns t. Otherwise, retains point position and
returns nil.

See `elpy-shell--nav-beginning-of-def' for details."
  (elpy-shell--nav-beginning-of-def 'elpy-shell--current-line-defun-p))

(defun elpy-shell--nav-beginning-of-defclass ()
  "Move point to the beginning of the current class definition.

If a definition is found, moves point to the start of the
definition and returns t. Otherwise, retains point position and
returns nil.

See `elpy-shell--nav-beginning-of-def' for details."
  (elpy-shell--nav-beginning-of-def 'elpy-shell--current-line-defclass-p))

(defun elpy-shell--nav-beginning-of-group ()
  "Move point to the beginning of the current or next group of top-level statements.

A sequence of top-level statements is a group if they are not
separated by empty lines. Empty lines within each top-level
statement are ignored.

If the point is within a top-level statement, moves to the
beginning of the group containing this statement. Otherwise, moves
to the first top-level statement below point."
  (elpy-shell--nav-beginning-of-top-statement)
  (while (not (or (elpy-shell--current-line-only-whitespace-p)
                  (eq (point) (point-min))))
    (unless (python-info-current-line-comment-p)
      (elpy-shell--nav-beginning-of-top-statement))
    (forward-line -1)
    (beginning-of-line))
  (when (elpy-shell--current-line-only-whitespace-p)
    (forward-line 1)
    (beginning-of-line)))

;;;;;;;;;;;;;;;;;
;;; Send commands

(defun elpy-shell-send-statement-and-step ()
  "Send current or next statement to Python shell and step.

If the current line is part of a statement, sends this statement.
Otherwise, skips forward to the next code line and sends the
corresponding statement."
  (interactive)
  (elpy-shell--ensure-shell-running)
  (when (not elpy-shell-echo-input) (elpy-shell--append-to-shell-output "\n"))
  (let ((beg (progn (elpy-shell--nav-beginning-of-statement)
                    (save-excursion
                      (beginning-of-line)
                      (point))))
        (end (progn (elpy-shell--nav-end-of-statement) (point))))
    (unless (eq beg end)
      (elpy-shell--flash-and-message-region beg end)
        (elpy-shell--with-maybe-echo
         (python-shell-send-string (python-shell-buffer-substring beg end)))))
  (python-nav-forward-statement))

(defun elpy-shell-send-top-statement-and-step ()
  "Send the current or next top-level statement to the Python shell and step.

If the current line is part of a top-level statement, sends this
top-level statement. Otherwise, skips forward to the next code
line and sends the corresponding top-level statement."
  (interactive)
  (elpy-shell--ensure-shell-running)
  (let* ((beg (progn (elpy-shell--nav-beginning-of-top-statement) (point)))
         (end (progn (elpy-shell--nav-end-of-statement) (point))))
    (elpy-shell--flash-and-message-region beg end)
    (if (string-match-p "\\`[^\n]*\\'" (buffer-substring beg end))
        ;; single line
        (elpy-shell-send-statement-and-step)
      ;; multiple lines
      (elpy-shell--with-maybe-echo
       (python-shell-send-region beg end))
      (setq mark-active nil)
      (python-nav-forward-statement))))

(defun elpy-shell-send-defun-and-step ()
  "Send the function definition that contains the current line
to the Python shell and steps.

See `elpy-shell--nav-beginning-of-def' for details."
  (interactive)
  (if (elpy-shell--nav-beginning-of-defun)
      (elpy-shell-send-statement-and-step)
    (message "There is no function definition that includes the current line.")))

(defun elpy-shell-send-defclass-and-step ()
  "Send the class definition that contains the current line to
the Python shell and steps.

See `elpy-shell--nav-beginning-of-def' for details."
  (interactive)
  (if (elpy-shell--nav-beginning-of-defclass)
      (elpy-shell-send-statement-and-step)
    (message "There is no class definition that includes the current line.")))

(defun elpy-shell-send-group-and-step ()
  "Send the current or next group of top-level statements to the Python shell and step.

A sequence of top-level statements is a group if they are not
separated by empty lines. Empty lines within each top-level
statement are ignored.

If the point is within a top-level statement, send the group
around this statement. Otherwise, go to the top-level statement
below point and send the group around this statement."
  (interactive)
  (elpy-shell--ensure-shell-running)
  (let* ((beg (progn (elpy-shell--nav-beginning-of-group) (point)))
         (end (progn
                ;; go forward to end of group
                (unless (python-info-current-line-comment-p)
                  (elpy-shell--nav-end-of-statement))
                (let ((p))
                  (while (not (eq p (point)))
                    (setq p (point))
                    (forward-line)
                    (if (elpy-shell--current-line-only-whitespace-p)
                        (goto-char p) ;; done
                      (unless (python-info-current-line-comment-p)
                        (elpy-shell--nav-end-of-statement)))))
                (point))))
    (if (> end beg)
        (progn
          (elpy-shell--flash-and-message-region beg end)
          ;; send the region and jump to next statement
          (if (string-match-p "\\`[^\n]*\\'" (buffer-substring beg end))
              ;; single line
              (elpy-shell-send-statement-and-step)
            ;; multiple lines
            (when (not elpy-shell-echo-input)
              (elpy-shell--append-to-shell-output "\n"))
            (elpy-shell--with-maybe-echo
             (python-shell-send-region beg end))
            (python-nav-forward-statement)))
      (goto-char (point-max)))
    (setq mark-active nil)))

(defun elpy-shell-send-codecell-and-step ()
  "Send the current code cell to the Python shell and step.

Signals an error if the point is not inside a code cell.

Cell beginnings and cell boundaries can be customized via the
variables `elpy-shell-cell-boundary-regexp' and
`elpy-shell-codecell-beginning-regexp', which see."
  (interactive)
  (let ((beg (save-excursion
               (end-of-line)
               (re-search-backward elpy-shell-cell-boundary-regexp nil t)
               (beginning-of-line)
               (and (string-match-p elpy-shell-codecell-beginning-regexp
                                    (thing-at-point 'line))
                    (point))))
        (end (save-excursion
               (forward-line)
               (if (re-search-forward elpy-shell-cell-boundary-regexp nil t)
                   (forward-line -1)
                 (goto-char (point-max)))
               (end-of-line)
               (point))))
    (if beg
        (progn
          (elpy-shell--flash-and-message-region beg end)
          (when (not elpy-shell-echo-input)
            (elpy-shell--append-to-shell-output "\n"))
          (elpy-shell--with-maybe-echo
           (python-shell-send-region beg end))
          (goto-char end)
          (python-nav-forward-statement))
      (message "Not in a codecell."))))

(defun elpy-shell-send-region-or-buffer-and-step (&optional arg)
  "Send the active region or the buffer to the Python shell and step.

If there is an active region, send that. Otherwise, send the
whole buffer.

In Emacs 24.3 and later, without prefix argument and when there
is no active region, this will escape the Python idiom of if
__name__ == '__main__' to be false to avoid accidental execution
of code. With prefix argument, this code is executed."
  (interactive "P")
  (if (use-region-p)
      (elpy-shell--flash-and-message-region (region-beginning) (region-end))
    (elpy-shell--flash-and-message-region (point-min) (point-max)))
  (elpy-shell--with-maybe-echo
   (elpy-shell--send-region-or-buffer-internal arg))
  (if (use-region-p)
      (goto-char (region-end))
    (goto-char (point-max))))

(defun elpy-shell--send-region-or-buffer-internal (&optional arg)
  "Send the active region or the buffer to the Python shell and step.

If there is an active region, send that. Otherwise, send the
whole buffer.

In Emacs 24.3 and later, without prefix argument and when there
is no active region, this will escape the Python idiom of if
__name__ == '__main__' to be false to avoid accidental execution
of code. With prefix argument, this code is executed."
  (interactive "P")
  (elpy-shell--ensure-shell-running)
  (when (not elpy-shell-echo-input) (elpy-shell--append-to-shell-output "\n"))
  (let ((if-main-regex "^if +__name__ +== +[\"']__main__[\"'] *:")
        (has-if-main-and-removed nil))
    (if (use-region-p)
        (let ((region (python-shell-buffer-substring
                       (region-beginning) (region-end))))
          (when (string-match "\t" region)
            (message "Region contained tabs, this might cause weird errors"))
          (python-shell-send-string region))
      (unless arg
        (save-excursion
          (goto-char (point-min))
          (setq has-if-main-and-removed (re-search-forward if-main-regex nil t))))
      (python-shell-send-buffer arg))
    (when has-if-main-and-removed
      (message (concat "Removed if __name__ == '__main__' construct, "
                       "use a prefix argument to evaluate.")))))

(defun elpy-shell-send-buffer-and-step (&optional arg)
  "Send entire buffer to Python shell.

In Emacs 24.3 and later, without prefix argument, this will
escape the Python idiom of if __name__ == '__main__' to be false
to avoid accidental execution of code. With prefix argument, this
code is executed."
  (interactive "P")
  (let ((p))
    (save-mark-and-excursion
      (deactivate-mark)
      (elpy-shell-send-region-or-buffer-and-step arg)
      (setq p (point)))
    (goto-char p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Send command variations (with/without step; with/without go)

(defun elpy-shell--send-with-step-go (step-fun step go my-prefix-arg)
  "Run a function with STEP and/or GO.

STEP-FUN should be a function that sends something to the shell
and moves point to code position right after what has been sent.

When STEP is nil, keeps point position. When GO is non-nil,
switches focus to Python shell buffer."
  (let ((orig (point)))
    (setq current-prefix-arg my-prefix-arg)
    (call-interactively step-fun)
    (when (not step)
      (goto-char orig)))
  (when go
    (elpy-shell-switch-to-shell)))

(defmacro elpy-shell--defun-step-go (fun-and-step)
  "Defines fun, fun-and-go, fun-and-step-and-go for the given FUN-AND-STEP function."
  (let ((name (string-remove-suffix "-and-step" (symbol-name fun-and-step))))
    (list
     'progn
     (let ((fun (intern name)))
       `(defun ,fun (&optional arg)
          ,(concat "Run `" (symbol-name fun-and-step) "' but retain point position.")
          (interactive "P")
          (elpy-shell--send-with-step-go ',fun-and-step nil nil arg)))
     (let ((fun-and-go (intern (concat name "-and-go"))))
       `(defun ,fun-and-go (&optional arg)
          ,(concat "Run `" (symbol-name fun-and-step) "' but retain point position and switch to Python shell.")
          (interactive "P")
          (elpy-shell--send-with-step-go ',fun-and-step nil t arg)))
     (let ((fun-and-step-and-go (intern (concat name "-and-step-and-go"))))
       `(defun ,fun-and-step-and-go (&optional arg)
          ,(concat "Run `" (symbol-name fun-and-step) "' and switch to Python shell.")
          (interactive "P")
          (elpy-shell--send-with-step-go ',fun-and-step t t arg))))))

(elpy-shell--defun-step-go elpy-shell-send-statement-and-step)
(elpy-shell--defun-step-go elpy-shell-send-top-statement-and-step)
(elpy-shell--defun-step-go elpy-shell-send-defun-and-step)
(elpy-shell--defun-step-go elpy-shell-send-defclass-and-step)
(elpy-shell--defun-step-go elpy-shell-send-group-and-step)
(elpy-shell--defun-step-go elpy-shell-send-codecell-and-step)
(elpy-shell--defun-step-go elpy-shell-send-region-or-buffer-and-step)
(elpy-shell--defun-step-go elpy-shell-send-buffer-and-step)


;;;;;;;;;;;;;;;;;;;;;;;
;; Debugging features

(when (version<= "25" emacs-version)
(defun elpy-pdb--clear-breakpoints-from-pdb ()
  "Clean the breakpoints fromm pdb.

This is necessary as pdb remember the breakpoints of the previous
pdb sessions."
  (python-shell-send-string-no-output
   "import bdb as __bdb;__bdb.Breakpoint.bpbynumber=[None]; __bdb.Breakpoint.next=1; __bdb.Breakpoint.bplist={}"))

(defun elpy-pdb--start-pdb (&optional output)
  "Start pdb on the current script.

if OUTPUT is non-nil, display the prompt after execution."
  (let ((string
         (format "import pdb as __pdb; __pdb.Pdb()._runscript('''%s''')"
                 (buffer-file-name))))
    (if output
        (python-shell-send-string string)
      (python-shell-send-string-no-output string))))

(defun elpy-pdb--get-breakpoint-positions ()
  "Return the list of lines with breakpoints."
  (let* ((overlays (overlay-lists))
         (overlays (append (car overlays) (cdr overlays)))
         (bp-lines '()))
    (dolist (ov overlays)
      (when (overlay-get ov 'elpy-breakpoint)
        (add-to-list 'bp-lines
                     (line-number-at-pos (overlay-start ov)))))
    bp-lines))

(defun elpy-pdb-debug-buffer (&optional arg)
  "Run pdb on the current buffer.

If breakpoints are set in the current buffer, jump to the first one.
If no breakpoints are set, debug from the beginning of the script.

With a prefix argument, ignore the present breakpoints."
  (interactive "P")
  (if arg
      (elpy-pdb-debug-buffer-from-beginning)
    (let ((bp-lines (elpy-pdb--get-breakpoint-positions)))
      (if (not bp-lines)
          (elpy-pdb-debug-buffer-from-beginning)
        (elpy-shell--ensure-shell-running)
        (save-buffer)
        (elpy-pdb--clear-breakpoints-from-pdb)
        (elpy-pdb--start-pdb)
        (dolist (bp-line bp-lines)
          (python-shell-send-string-no-output (format "break %s" bp-line)))
        (python-shell-send-string "continue")))))

(defun elpy-pdb-debug-buffer-from-beginning ()
  "Ignore the breakpoints and run pdb on the current buffer."
  (elpy-shell--ensure-shell-running)
  (save-buffer)
  (elpy-pdb--clear-breakpoints-from-pdb)
  (elpy-pdb--start-pdb t))

(defun elpy-pdb-break-at-point ()
  "Run pdb on the current buffer and break at point.

Ignore the the existing breakpoints.
Pdb can directly exit if the current line is not a
statement that is actually run (blank line, comment line, ...)."
  (interactive)
  (elpy-shell--ensure-shell-running)
  (save-buffer)
  (let ((line-number (line-number-at-pos)))
    (elpy-pdb--clear-breakpoints-from-pdb)
    (elpy-pdb--start-pdb)
    (python-shell-send-string-no-output (format "break %s" line-number))
    (python-shell-send-string "continue")))

(defun elpy-pdb-debug-last-exception ()
  "Run post-mortem pdb on the last exception."
  (interactive)
  (elpy-shell--ensure-shell-running)
  ;; check if there is a last exception
  (if (not (with-current-buffer (format "*%s*"
                                        (python-shell-get-process-name nil))
             (save-excursion
               (goto-char (point-max))
               (search-backward "Traceback (most recent call last):"
                                nil t))))
      (error "No traceback on the current shell")
    (python-shell-send-string
     "import pdb as __pdb;__pdb.pm()")))

;; Fringe indicators
(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'elpy-breakpoint-fringe-marker
    (vector
     #b00000000
     #b00111100
     #b01111110
     #b01111110
     #b01111110
     #b01111110
     #b00111100
     #b00000000)))

(defcustom elpy-breakpoint-fringe-face 'elpy-breakpoint-fringe-face
   "Face for breakpoints bitmaps appearing on the fringe."
   :type 'face
   :group 'elpy)

(defface elpy-breakpoint-fringe-face
   '((t (:foreground "red"
         :box (:line-width 1 :color "red" :style released-button))))
   "Face for breakpoint bitmaps appearing on the fringe."
   :group 'elpy)

(defun elpy-pdb-toggle-breakpoint-at-point (&optional arg)
  "Add or remove a breakpoint at the current line.

With a prefix argument, remove all the breakpoints from the current buffer."
  (interactive "P")
  (if arg
      (elpy-pdb-clear-breakpoints)
    (let ((overlays (overlays-at (point)))
          bp-at-line)
      ;; Check if already a breakpoint
      (while overlays
        (let ((overlay (car overlays)))
          (when (overlay-get overlay 'elpy-breakpoint)
            (setq bp-at-line t))
          (setq overlays (cdr overlays))))
      (cond
       ;; If so, remove it
       (bp-at-line
        (remove-overlays (line-beginning-position) (line-end-position)
                         'elpy-breakpoint t))
       ;; Else add a new breakpoint
       (t
        (let* ((ov (make-overlay (line-beginning-position) (line-end-position)))
              (marker-string "*fringe-dummy*")
              (marker-length (length marker-string)))
          (put-text-property 0 marker-length
                             'display
                             (list 'left-fringe
                                   'elpy-breakpoint-fringe-marker
                                   'elpy-breakpoint-fringe-face)
                             marker-string)
          (overlay-put ov 'before-string marker-string)
          (overlay-put ov 'priority 200)
          (overlay-put ov 'elpy-breakpoint t)))))))

(defun elpy-pdb-clear-breakpoints ()
  "Remove the breakpoints in the current region or buffer."
  (if (use-region-p)
      (remove-overlays (region-beginning) (region-end) 'elpy-breakpoint t)
    (remove-overlays (point-min) (point-max) 'elpy-breakpoint t)))
)


;;;;;;;;;;;;;;;;;;;;;;;
;; Deprecated functions

(defun elpy-use-ipython (&optional _ipython)
  "Deprecated; see https://elpy.readthedocs.io/en/latest/ide.html#interpreter-setup"
  (error "elpy-use-ipython is deprecated; see https://elpy.readthedocs.io/en/latest/ide.html#interpreter-setup"))
(make-obsolete 'elpy-use-ipython nil "Jan 2017")

(defun elpy-use-cpython (&optional _cpython)
  "Deprecated; see https://elpy.readthedocs.io/en/latest/ide.html#interpreter-setup"
  (error "elpy-use-cpython is deprecated; see https://elpy.readthedocs.io/en/latest/ide.html#interpreter-setup"))
(make-obsolete 'elpy-use-cpython nil "Jan 2017")

(provide 'elpy-shell)

;;; elpy-shell.el ends here
