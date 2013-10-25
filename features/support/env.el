(require 'f)

(defvar elpy-support-path
  (f-dirname load-file-name))

(defvar elpy-features-path
  (f-parent elpy-support-path))

(defvar elpy-root-path
  (f-parent elpy-features-path))

(add-to-list 'load-path elpy-root-path)

(require 'elpy)
(require 'espuds)
(require 'ert)

(defvar elpy-feature--cleanups nil)

(defun add-cleanup (fun &rest args)
  (add-to-list 'elpy-feature--cleanups (cons fun args)))

(defun run-cleanup ()
  (dolist (cleanup elpy-feature--cleanups)
    (ignore-errors
      (apply (car cleanup) (cdr cleanup))))
  (setq elpy-feature--cleanups nil))

(defun kill-buffer-no-questions (buf)
  (let ((kill-buffer-query-functions nil))
    (kill-buffer buf)))


(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 (run-cleanup)
 )

(Teardown
 ;; After when everything has been run
 )
