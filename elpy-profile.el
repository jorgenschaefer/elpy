;;; elpy-profile.el --- Profiling capabilitiss for elpy

;; Copyright (C) 2013-2019  Jorgen Schaefer

;; Author: Gaby Launay <gaby.launay@tutanota.com>
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

;; This file serves as an extension to elpy by adding profiling capabilities

;;; Code:


;;;;;;;;;;;;;;;;;;;;;;
;;; User customization

(defcustom elpy-profile-visualizer "snakeviz"
  "Visualizer for elpy profile results."
  :type '(choice (const :tag "Snakeviz" "snakeviz")
                 (const :tag "RunSnakeRun" "runsnake")
                 (const :tag "pyprof2calltree" "pyprof2calltree -k -i")
                 (string :tag "Other"))
  :group 'elpy)

;;;;;;;;;;;;;;;;;;;;;;
;;; Helper Functions

(defun elpy-profile--display-profiling (file)
  "Display the profile result FILE using `elpy-profile-visualizer'."
  (let ((exec (car (split-string elpy-profile-visualizer " " t)))
        (args (append (cdr (split-string elpy-profile-visualizer " " t)) (list file))))
    (if (executable-find exec)
        (apply 'call-process exec nil 0 nil args)
      (message "Elpy profile visualizer '%s' not found" exec))))

(defun elpy-profile--sentinel (process string)
  "Elpy profile sentinel."
  (let ((filename (file-name-nondirectory (process-get process 'file)))
        (prof-file (process-get process 'prof-file))
        (dont-display (process-get process 'dont-display)))
    (with-current-buffer "*elpy-profile-log*"
      (view-mode))
    (if (not (string-equal string "finished\n"))
        (progn
          (message  "[%s] Profiling failed" filename)
          (display-buffer  "*elpy-profile-log*"))
      (message  "[%s] Profiling succeeded" filename)
      (when (not dont-display)
        (elpy-profile--display-profiling prof-file)))))

(defun elpy-profile--file (file &optional in-dir dont-display)
  "Profile asynchronously FILE and display the result using
`elpy-profile-visualizer'.

If IN-DIR is non nil, profile result is saved in the same
directory as the script.
If DONT-DISPLAY is non nil, don't display the profile results."
  (ignore-errors (kill-buffer "*elpy-profile-log*"))
  (let* ((prof-file (if in-dir
                        (concat (file-name-sans-extension file) ".profile")
                      (concat (make-temp-file "elpy-profile-" nil ".profile"))))
         (proc-name (format "elpy-profile-%s" file))
         (proc-cmd (list elpy-rpc-python-command "-m" "cProfile" "-o" prof-file file))
         (proc (make-process :name proc-name
                             :buffer "*elpy-profile-log*"
                             :sentinel 'elpy-profile--sentinel
                             :command proc-cmd)))
    (message "[%s] Profiling ..." (file-name-nondirectory file))
    (process-put proc 'prof-file prof-file)
    (process-put proc 'file file)
    (process-put proc 'dont-display dont-display)
    prof-file))

;;;;;;;;;;;;;;;;;;;;;;
;;; User Functions

(defun elpy-profile-buffer-or-region (&optional in-dir dont-display)
  "Profile asynchronously the active region or the current buffer
and display the result using `elpy-profile-visualizer'.

If IN-DIR is non nil, profile result is saved in the same
directory as the script.
If DONT-DISPLAY is non nil, don't display the profile results."
  (interactive "P")
  (let* ((file-name (buffer-name))
         (file-dir (file-name-directory (buffer-file-name)))
         (beg (if (region-active-p) (region-beginning) (point-min)))
         (end (if (region-active-p) (region-end) (point-max)))
         (tmp-file-prefix (if (region-active-p) "_region_" ""))
         (tmp-file (if in-dir
                       (concat file-dir "/" tmp-file-prefix file-name)
                     (concat (make-temp-file "elpy-profile-" t)  "/" tmp-file-prefix file-name)))
         (region (python-shell-buffer-substring beg end)))
    (with-temp-buffer
      (insert region)
      (write-region (point-min) (point-max) tmp-file nil t))
    (elpy-profile--file tmp-file t dont-display)))

(provide 'elpy-profile)
;;; elpy-profile.el ends here
