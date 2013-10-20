(require 'ert)

(ert-deftest elpy-rpc--process-buffer-p ()
  "Test that this finds actual process buffers."
  (with-temp-buffer
    (setq elpy-rpc--buffer-p t)
    (should (elpy-rpc--process-buffer-p (current-buffer))))
  (with-temp-buffer
    (should (not (elpy-rpc--process-buffer-p (current-buffer))))))

(ert-deftest elpy-rpc--live-p ()
  "Test that it finds live buffers."
  (should (not (elpy-rpc--live-p nil)))
  (with-temp-buffer
    (should (not (elpy-rpc--live-p (current-buffer)))))
  (with-temp-buffer
    (let ((proc (start-process "*elpy-rpc-test*" (current-buffer) "cat")))
      (set-process-query-on-exit-flag proc nil)
      (should (elpy-rpc--live-p (current-buffer)))
      (delete-process proc)
      (should (not (elpy-rpc--live-p (current-buffer)))))))

(ert-deftest elpy-rpc--buffer ()
  "Test that buffers are found."
  ;; Error outside of elpy-mode buffers
  (with-temp-buffer
    (setq elpy-mode nil)
    (should-error (elpy-rpc--get-rpc-buffer)))
  ;; If we already have a buffer, return that
  (let (rpc-buf prg-buf)
    (with-temp-buffer
      (setq rpc-buf (current-buffer)
            elpy-rpc--buffer-p t)
      ;; A stupid test process
      (set-process-query-on-exit-flag
       (start-process "elpy-rpc-test" rpc-buf "cat")
       nil)
      (with-temp-buffer
        (setq prg-buf (current-buffer)
              elpy-mode t
              elpy-rpc--buffer rpc-buf)
        (should (equal rpc-buf (elpy-rpc--get-rpc-buffer))))))
  ;; Find existing buffer for this project
  (let (rpc-buf prg-buf)
    (with-temp-buffer
      (setq rpc-buf (current-buffer)
            elpy-rpc--buffer-p t

            )
      (with-temp-buffer
        (setq prg-buf (current-buffer))
        )))

    )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Helper functions to make writing tests actually productive

;; flet is deprectated; cl-flet is static; could import dflet from
;; some library, but why add *another* dependency for external
;; libraries just for testing...
(defmacro elpy-test-funlet (bindings &rest body)
  "Bind function symbols in BINDINGS dynamically for the duration of BODY.

Much like `cl-flet', except it's dyanmic.

\(fn ((FUNCT ARGLIST BODY...) ...) FORM...)"
  (declare (indent 1))
  (let ((bindings (mapcar (lambda (binding)
                            (cons (make-symbol
                                   (concat "backup-"
                                           (symbol-name (car binding))))
                                  binding))
                          bindings)))
    `(unwind-protect
         (progn
           ,@(mapcar (lambda (binding)
                       `(progn
                          (setq ,(car binding)
                                (symbol-function ',(cadr binding)))
                          (fset ',(cadr binding)
                                (lambda ,@(cddr binding)))))
                     bindings)
           ,@body)
       ,@(mapcar (lambda (binding)
                   `(fset ',(cadr binding)
                          ,(car binding)))
                 bindings))))


(defmacro elpy-test-with-temp-dir (name &rest body)
  "Create a temporary directory and bind the symbol NAME to the path.

Run BODY with that binding."
  (declare (indent 1))
  `(let ((,name (make-temp-file "elpy-test-" t)))
     (unwind-protect
         (progn ,@body)
       (ignore-errors
         (delete-directory ,name t)))))

;;; A with macro because ERT does not support setUp/tearDown-style
;;; tests. According to the ERT documentation, Emacs Lisp has
;;; `condition-case' and hence does not need the ability to have the
;;; same setup for multiple tests or something.
(defmacro elpy-test-with-buffer (contents &rest body)
  "Create a buffer with CONTENTS and run BODY in it.

If CONTENTS contain the char sequence _|_, it's deleted and point
is placed there. The buffer is set up with elpy mode and a
project root of an empty directory."
  (declare (indent 1))
  (let ((dir (make-symbol "dir")))
    `(elpy-test-with-temp-dir ,dir
       (with-temp-buffer
         (let ((elpy-project-root ,dir))
           (python-mode)
           (elpy-mode 1)
           (insert ,@contents)
           (goto-char (point-min))
           (when (search-forward "_|_" nil t)
             (delete-forward-char -3))
           ,@body)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The actual tests now

(require 'package)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize t)
(package-refresh-contents)

(dolist (pkg (with-temp-buffer
               (insert-file-contents-literally "elpy-pkg.el.in")
               (goto-char (point-min))
               (let ((define-package (read (current-buffer))))
                 (cadr (nth 4 define-package)))))
  (when (not (package-installed-p (car pkg)))
    (package-install (car pkg))))

(package-initialize)

(require 'elpy)

(ert-deftest test-elpy-show-defun ()
  "Test that `elpy-show-defun' actually shows a message."

  (elpy-test-with-buffer
      ("class Foo(object):\n"
       "    def bar(self):\n"
       "        _|_pass\n")
    (should (equal "Foo.bar()"
                   (elpy-show-defun))))

  (elpy-test-with-buffer
      ("def bar(self):\n"
       "    _|_pass\n")
    (should (equal "bar()"
                   (elpy-show-defun))))

  (elpy-test-with-buffer
      ("class Foo(object):\n"
       "    def bar(self):\n"
       "        def baz(x, y):\n"
       "            _|_return x + y\n"
       "        return baz(2, 3)")
    (should (equal "Foo.bar.baz()"
                   (elpy-show-defun)))))

(ert-deftest test-elpy-nav-forward-statement ()
  "Should skip forward over consecutive statements."
  (elpy-test-with-buffer
      ("_|_foo = bar(2, 3, fnord(4))\n"
       "baz = quux\n")
    (elpy-nav-forward-statement)
    (should (looking-at "\nbaz = "))
    (elpy-nav-forward-statement)
    (should (looking-at "\n\\'"))))

(ert-deftest test-elpy-nav-backward-statement ()
  "Should skip backward over consecutive statements."
  :expected-result (if (string-match "GNU Emacs 24\\.[12]" (emacs-version))
                       :failed
                     :passed)
  (elpy-test-with-buffer
      ("foo = bar(2, 3, fnord(4))\n"
       "baz = quux\n_|_")
    (elpy-nav-backward-statement)
    (should (looking-at "baz = "))
    (elpy-nav-backward-statement)
    (should (looking-at "\\`foo"))))

(ert-deftest test-elpy-forward-definition ()
  "This should jump to the next function, class or method."
  (elpy-test-with-buffer
      ("import foo\n"
       "\n"
       "FOO = 1\n"
       "\n"
       "def function(x, y):\n"
       "    def inner_function(z):\n"
       "        return x+y+z\n"
       "    return function\n"
       "\n"
       "class Foo(object):\n"
       "    CLASSVAR = 1\n"
       "    def bar(self, z):\n"
       "        return function(1, 2)(z)\n")
    (should (looking-at "import foo"))
    (elpy-forward-definition)
    (should (looking-at "def function"))
    (elpy-forward-definition)
    (should (looking-at "def inner_function"))
    (elpy-forward-definition)
    (should (looking-at "class Foo"))
    (elpy-forward-definition)
    (should (looking-at "def bar"))
    (elpy-forward-definition)
    (should (looking-at "\\'"))))

(ert-deftest test-elpy-backward-definition ()
  "This should jump to the next function, class or method."
  (elpy-test-with-buffer
      ("import foo\n"
       "\n"
       "FOO = 1\n"
       "\n"
       "def function(x, y):\n"
       "    def inner_function(z):\n"
       "        return x+y+z\n"
       "    return function\n"
       "\n"
       "class Foo(object):\n"
       "    CLASSVAR = 1\n"
       "    def bar(self, z):\n"
       "        return function(1, 2)(z)\n"
       "_|_")
    (should (looking-at "\\'"))
    (elpy-backward-definition)
    (should (looking-at "def bar"))
    (elpy-backward-definition)
    (should (looking-at "class Foo"))
    (elpy-backward-definition)
    (should (looking-at "def inner_function"))
    (elpy-backward-definition)
    (should (looking-at "def function"))
    (elpy-backward-definition)
    (should (looking-at "import foo"))))

(ert-deftest test-elpy-project-find-root ()
  "Should find the first directory without __init__.py"
  (elpy-test-with-temp-dir library-root
    (let ((default-directory (concat library-root "/foo/bar/baz")))
      (make-directory default-directory t)
      (dolist (file (list (concat library-root "/foo/__init__.py")
                          (concat library-root "/foo/bar/__init__.py")
                          (concat library-root "/foo/bar/baz/__init__.py")))
        (with-temp-buffer
          (write-region (point-min) (point-max)
                        file)))
      (should (equal (expand-file-name (elpy-project-find-root))
                     (expand-file-name (concat library-root "/"))))))

  (elpy-test-with-temp-dir library-root
    (let ((default-directory (concat library-root "/foo/bar/baz")))
      (make-directory default-directory t)
      (should (equal (elpy-project-find-root)
                     default-directory))
      (should (equal (elpy-project-find-root t)
                     nil)))))

(ert-deftest test-elpy-refactor-mode ()
  "Test that we can run `elpy-refactor-mode' at all."
  (with-temp-buffer
    (elpy-refactor-mode)))

(ert-deftest test-elpy-shell-get-or-create-process ()
  "Test that we can run `elpy-shell-get-or-create-process' at all.

Backwards compatibility issues, see #124."
  (let ((proc (elpy-shell-get-or-create-process)))
    (and (processp proc)
         (process-live-p proc))))

(ert-deftest test-elpy-rpc-echo ()
 "Test that the backend communication works at all."
 (let ((elpy-rpc--timeout 30))
   (with-temp-buffer
     (setq elpy-mode t)
     (dolist (args '(("foo" nil 1 2 3)))
       (should (equal args
                      (apply #'elpy-rpc "echo" (list args))))))))

(ert-deftest test-elpy--region-without-indentation ()
  "Test the function."
  (with-temp-buffer
    (insert "def foo():\n"
            "  pass")
    (should (equal (elpy--region-without-indentation (point-min)
                                                     (point-max))
                   "def foo():\n  pass")))
  (with-temp-buffer
    (insert "  \n"
            "  def foo():\n"
            "    pass\n"
            "  ")
    (should (equal (elpy--region-without-indentation (point-min)
                                                     (point-max))
                   "\ndef foo():\n  pass\n")))

  (with-temp-buffer
    (insert "  def foo():\n"
            "    return 23\n"
            "\n"
            "  print foo()")
    (should (equal (elpy--region-without-indentation (point-min)
                                                     (point-max))
                   "def foo():\n  return 23\n\nprint foo()")))

  (with-temp-buffer
    (insert "  def foo():\n"
            "    pass\n"
            "\n"
            "meh\n")
    (should-error (elpy--region-without-indentation (point-min)
                                                    (point-max)))))
