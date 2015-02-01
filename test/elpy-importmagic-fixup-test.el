(ert-deftest elpy-importmagic-fixup-test ()
  (elpy-testcase ()
    (insert "# test
import sys

os.getcwd()
")
    (when (gethash "importmagic_version" (elpy-config--get-config))
      (mletf* ((completing-read (prompt vals &optional func require-match default history)
                                "import os"))
        (elpy-importmagic-fixup)
        (should (s-contains-p "import os\n" (buffer-string)))
        (should-not (s-contains-p "import sys\n" (buffer-string)))))))
