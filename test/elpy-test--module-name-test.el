(ert-deftest elpy-test--module-name ()
  (elpy-testcase ()
    (should
     (equal (elpy-test--module-name "/project/root"
                                    "/project/root/module.py")
            "module"))

    (should
     (equal (elpy-test--module-name "/project/root"
                                    "/project/root/package/__init__.py")
            "package"))

    (should
     (equal (elpy-test--module-name "/project/root"
                                    "/project/root/package/module.py")
            "package.module"))

    (should
     (equal (elpy-test--module-name "/project/root/subdir"
                                    "/project/package/module.py")
            "...package.module"))))
