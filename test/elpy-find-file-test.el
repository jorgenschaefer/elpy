(ert-deftest elpy-find-file-should-use-import-at-point ()
  (elpy-testcase ()
    (mletf* ((buffer-file-name () "test-file")
             (called-module nil)
             (elpy-find--resolve-module (module) (setq called-module module))
             (found-file nil)
             (find-file (path) (setq found-file path)))
      (insert "import foo")

      (elpy-find-file t)

      (should (equal called-module "foo"))
      (should (equal found-file "foo")))))

(ert-deftest elpy-find-file-should-use-deep-import-at-point ()
  (elpy-testcase ()
    (mletf* ((buffer-file-name () "test-file")
             (called-module nil)
             (elpy-find--resolve-module (module) (setq called-module module))
             (found-file nil)
             (find-file (path) (setq found-file path)))
      (insert "import foo.bar")

      (elpy-find-file t)

      (should (equal called-module "foo.bar"))
      (should (equal found-file "foo.bar")))))

(ert-deftest elpy-find-file-should-use-from-import-at-point ()
  (elpy-testcase ()
    (mletf* ((buffer-file-name () "test-file")
             (called-module nil)
             (elpy-find--resolve-module (module) (setq called-module module))
             (found-file nil)
             (find-file (path) (setq found-file path)))
      (insert "from foo import bar")

      (elpy-find-file t)

      (should (equal called-module "foo.bar"))
      (should (equal found-file "foo.bar")))))

(ert-deftest elpy-find-file-should-use-deep-from-import-at-point ()
  (elpy-testcase ()
    (mletf* ((buffer-file-name () "test-file")
             (called-module nil)
             (elpy-find--resolve-module (module) (setq called-module module))
             (found-file nil)
             (find-file (path) (setq found-file path)))
      (insert "from foo.bar import baz.Qux")

      (elpy-find-file t)

      (should (equal called-module "foo.bar.baz.Qux"))
      (should (equal found-file "foo.bar.baz.Qux")))))

(ert-deftest elpy-find-file-should-open-test-file ()
  (elpy-testcase ((:project project-root "module.py" "test_module.py"))
    (find-file (f-join project-root "module.py"))

    (fmakunbound 'projectile-find-file)
    (defun find-file-in-project ())
    (require 'find-file-in-project)
    (elpy-find-file t)

    (should (f-equal? (buffer-file-name)
                      (f-join project-root "test_module.py")))))

(ert-deftest elpy-find-file-should-call-ffip ()
  (elpy-testcase ()
    ;; The test failed on Travis in 24.3 because the function was not
    ;; defined. Weird. Well, call it in explicitly.
    (fmakunbound 'projectile-find-file)
    (defun find-file-in-project ())
    (require 'find-file-in-project)
    (mletf* ((ffip-called nil)
             (find-file-in-project () (setq ffip-called t)))

      (elpy-find-file)

      (should ffip-called))))

(ert-deftest elpy-find-file-should-call-projectile ()
  (elpy-testcase ()
    ;; The test failed on Travis in 24.3 because the function was not
    ;; defined. Weird. Well, call it in explicitly.
    (fmakunbound 'find-file-in-project)
    (defun projectile-find-file ())
    (when (require 'projectile nil t)
    (mletf* ((projectile-called nil)
             (projectile-find-file () (setq projectile-called t)))

     (elpy-find-file)
     (should projectile-called)))))
