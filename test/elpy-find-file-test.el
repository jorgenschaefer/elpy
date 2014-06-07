(ert-deftest elpy-find-file-should-use-import-at-point ()
  (elpy-testcase ()
    (mletf* ((buffer-file-name () "test-file")
             (called-module nil)
             (elpy--resolve-module (module) (setq called-module module))
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
             (elpy--resolve-module (module) (setq called-module module))
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
             (elpy--resolve-module (module) (setq called-module module))
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
             (elpy--resolve-module (module) (setq called-module module))
             (found-file nil)
             (find-file (path) (setq found-file path)))
      (insert "from foo.bar import baz.Qux")

      (elpy-find-file t)

      (should (equal called-module "foo.bar.baz.Qux"))
      (should (equal found-file "foo.bar.baz.Qux")))))

(ert-deftest elpy-find-file-should-open-test-file ()
  (elpy-testcase ((:project project-root "module.py" "test_module.py"))
    (find-file (f-join project-root "module.py"))

    (elpy-find-file t)

    (should (f-equal? (buffer-file-name)
                      (f-join project-root "test_module.py")))))

(ert-deftest elpy-find-file-should-call-ffip ()
  (elpy-testcase ()
    (mletf* ((ffip-called nil)
             (find-file-in-project () (setq ffip-called t)))

      (elpy-find-file)

      (should ffip-called))))
