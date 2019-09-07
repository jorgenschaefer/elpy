;; Load Elpy
(require 'f)
(require 'cl-extra)
(let ((elpy-dir (f-parent (f-dirname (f-this-file)))))
  (add-to-list 'load-path elpy-dir)
  (add-to-list 'process-environment (format "PYTHONPATH=%s:%s"
					    elpy-dir
					    (getenv "PYTHONPATH")))
  (add-to-list 'process-environment "ELPY_TEST=1"))
(require 'elpy)
;; blocking if not overwritten
(defun elpy-doc--read-identifier-from-minibuffer (initial)
  nil)


;; Main function to run benchmarks
(defun elpy-benchmark-tests (&optional repetitions)
  "Benchmark every elpy's tests to identify stalling origins."
  (interactive)
  (let* ((dir (file-name-directory (locate-library "elpy.el")))
         (files (directory-files (concat (file-name-as-directory dir)
                                         "test")
                                 t
                                 "\\(.*-test\\.el$\\|test-helper\\.el\\)"
                                 ))
         (test-symbols '())
         (test-benchmarks '()))
    ;; Load every test
    (dolist (test files)
      (load-file test))
    ;; Get tests list
    (mapatoms (lambda (symb)
                (when (and (ert-test-boundp symb)
                           (string-match "elpy" (symbol-name symb)))
                  (add-to-list 'test-symbols symb)))
              obarray)
    ;; Run every test with benchmark
    (dolist (test test-symbols)
      (message "Test: %s" (symbol-name test))

      (add-to-list 'test-benchmarks
                   (cons (symbol-name test)
                         (benchmark-run (or repetitions 1)
                           (ert-run-tests test (lambda (&rest _rest) t))))))
    ;; Sort by time elapsed
    (setq test-benchmarks
          (sort test-benchmarks
                (lambda (elem1 elem2)
                  (< (nth 1 elem1) (nth 1 elem2)))))
    ;; Display
    (message "=== Ran %s tests ===" (length test-symbols))
    (let ((tot-time 0))
      (dolist (test test-benchmarks)
        (setq tot-time (+ tot-time (nth 1 test)))
        (message "%s" test))
      (message "Total elapsed time: %s" tot-time))))



;; Run benchmarks
(elpy-benchmark-tests 1)
