;; Add melpa
(require 'package)
(setq package-archives '(("MELPA"        . "https://melpa.org/packages/")))
(package-initialize)
;; Install dependencies
(package-install 'company)
(package-install 'find-file-in-project)
(package-install 'highlight-indentation)
(package-install 'pyvenv)
(package-install 'yasnippet)
(package-install 's)
;; Load local Elpy
(require 'f)
(require 'cl-extra)
(let ((elpy-dir (f-parent (f-dirname (f-this-file)))))
  (setq load-path (cons elpy-dir load-path))
  (setq process-environment (cons (format "PYTHONPATH=%s:%s"
                                          elpy-dir
                                          (getenv "PYTHONPATH"))
                                  process-environment)))
(require 'elpy)
(elpy-enable)
