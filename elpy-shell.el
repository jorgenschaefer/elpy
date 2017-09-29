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

;;;;;;;;;;;;;;;;;;;;;;
;;; User customization

(defcustom elpy-dedicated-shells nil
  "Non-nil if Elpy should use dedicated shells.

Elpy can use a unique python shell for all buffers and support
manually started dedicated shells. Setting this option to non-nil
force the creation of dedicated shells for each buffers."
  :type 'boolean
  :group 'elpy)

;;;;;;;;;;;;;;;;;;;;;
;;; Interactive Shell

(defvar elpy--shell-last-py-buffer nil
  "Help keep track of python buffer when changing to pyshell.")

(defun elpy-use-ipython (&optional ipython)
  "Set defaults to use IPython instead of the standard interpreter.

With prefix arg, prompt for the command to use."
  (interactive (list (when current-prefix-arg
                       (read-file-name "IPython command: "))))
  (when (not ipython)
    (setq ipython "ipython"))
  (when (not (executable-find ipython))
    (error "Command %S not found" ipython))
  ;; Needed for IPython 5+
  (setenv "IPY_TEST_SIMPLE_PROMPT" "1")
  (cond
   ;; Emacs 24 until 24.3
   ((boundp 'python-python-command)
    (setq python-python-command ipython))
   ;; Emacs 24.3
   ((and (version<= "24.3" emacs-version)
         (not (boundp 'python-shell-interpreter-interactive-arg)))
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
          "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))
   ;; Emacs 24.4
   ((boundp 'python-shell-interpreter-interactive-arg)
    (setq python-shell-interpreter ipython
          python-shell-interpreter-args "-i")
    ;; Windows requires some special handling here, see #422
    (let ((exe "C:\\Python27\\python.exe")
          (ipython_py "C:\\Python27\\Scripts\\ipython-script.py"))
      (when (and (eq system-type 'windows-nt)
                 (file-exists-p exe)
                 (file-exists-p ipython_py))
        (setq python-shell-interpreter exe
              python-shell-interpreter-args "-i " + ipython_py))))
   (t
    (error "I don't know how to set ipython settings for this Emacs"))))

(defun elpy-use-cpython (&optional cpython)
  "Set defaults to use the standard interpreter instead of IPython.

With prefix arg, prompt for the command to use."
  (interactive (list (when current-prefix-arg
                       (read-file-name "Python command: "))))
  (when (not cpython)
    (setq cpython "python"))
  (when (not (executable-find cpython))
    (error "Command %S not found" cpython))
  (cond
   ;; Emacs 24 until 24.3
   ((boundp 'python-python-command)
    (setq python-python-command cpython))
   ;; Emacs 24.3 and onwards.
   ((and (version<= "24.3" emacs-version)
         (not (boundp 'python-shell-interpreter-interactive-arg)))
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
          "';'.join(__COMPLETER_all_completions('''%s'''))\n"))
   ;; Emacs 24.4
   ((boundp 'python-shell-interpreter-interactive-arg)
    (setq python-shell-interpreter cpython
          python-shell-interpreter-args "-i"))
   (t
    (error "I don't know how to set ipython settings for this Emacs"))))

(defun elpy-shell-display-buffer ()
  "Display inferior Python process buffer."
  (display-buffer (process-buffer (elpy-shell-get-or-create-process))
                  nil
                  'visible))

;;;;;;;;;;;;;;;;;;
;;; Shell commands

(defun elpy-shell-switch-to-shell ()
  "Switch to inferior Python process buffer."
  (interactive)
  (setq elpy--shell-last-py-buffer (buffer-name))
  (pop-to-buffer (process-buffer (elpy-shell-get-or-create-process))))

(defun elpy-shell-switch-to-buffer ()
  "Switch from inferior Python process buffer to recent Python buffer."
  (interactive)
  (pop-to-buffer elpy--shell-last-py-buffer))

(defun elpy-shell-kill (&optional kill-buff)
  "Kill the current python shell.

If `elpy-dedicated-shells' is non-nil,
kill the current buffer dedicated shell.

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
If ASK-FOR-EACH-ONE is non-nil, ask before killing each python process.
"
  (interactive)
  (let ((python-buffer-list ()))
    ;; Get active python shell buffers and kill inactive ones (if asked)
    (loop for buffer being the buffers do
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
      (loop for buffer in python-buffer-list do
	    (when (y-or-n-p (format "Kill %s ?" buffer))
		(delete-process buffer)
		(when kill-buffers
		  (kill-buffer buffer)))))
     ;; Ask and kill every buffers
     (python-buffer-list
      (if (y-or-n-p (format "Kill %s python shells ?" (length python-buffer-list)))
	  (loop for buffer in python-buffer-list do
		(delete-process buffer)
		(when kill-buffers
		  (kill-buffer buffer)))))
     ;; No shell to close
     (t
      (message "No python shell to close")))))

(defun elpy-shell-get-or-create-process ()
  "Get or create an inferior Python process for current buffer and return it."
  (let* ((bufname (format "*%s*" (python-shell-get-process-name nil)))
         (dedbufname (format "*%s*" (python-shell-get-process-name t)))
         (proc (get-buffer-process bufname))
         (dedproc (get-buffer-process dedbufname)))
    (if elpy-dedicated-shells
        (if dedproc
            dedproc
          (run-python (python-shell-parse-command) t)
          (get-buffer-process dedbufname))
      (if dedproc
          dedproc
        (if proc
            proc
          (run-python (python-shell-parse-command))
          (get-buffer-process bufname))))))

(defun elpy-shell--region-without-indentation (beg end)
  "Return the current region as a string, but without indentation."
  (if (= beg end)
      ""
    (let ((region (buffer-substring beg end))
          (indent-level nil)
          (indent-tabs-mode nil))
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
        ;; 'indent-rigidly' introduces tabs despite the fact that 'indent-tabs-mode' is nil
        ;; 'untabify' fix that
	(untabify (point-min) (point-max))
        (buffer-string)))))

;;;;;;;;;;;;;;;;;
;;; Send commands

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
    (if (use-region-p)
        (let ((region (elpy-shell--region-without-indentation
                       (region-beginning) (region-end))))
          (setq has-if-main (string-match if-main-regex region))
          (when (string-match "\t" region)
            (message "Region contained tabs, this might cause weird errors"))
          (python-shell-send-string region))
      (save-excursion
        (goto-char (point-min))
        (setq has-if-main (re-search-forward if-main-regex nil t)))
      (python-shell-send-buffer arg))
    (elpy-shell-display-buffer)
    (when has-if-main
      (message (concat "Removed if __name__ == '__main__' construct, "
                       "use a prefix argument to evaluate.")))))

(defun elpy-shell-send-current-statement ()
  "Send current statement to Python shell."
  (interactive)
  (let ((beg (python-nav-beginning-of-statement))
        (end (python-nav-end-of-statement)))
    (elpy-shell-get-or-create-process)
    (python-shell-send-string (buffer-substring beg end)))
  (elpy-shell-display-buffer)
  (python-nav-forward-statement))

(provide 'elpy-shell)
;;; elpy-shell.el ends here
