(ert-deftest elpy-importmagic-add-import-test ()
  (elpy-testcase ()
    (insert "# test
import os

print()
")
    (when (gethash "importmagic_version" (elpy-config--get-config))
      (elpy-importmagic-add-import "from mymodule import mysymbol")
      (should (s-contains-p "from mymodule import mysymbol\n" (buffer-string))))))
