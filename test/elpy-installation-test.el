(ert-deftest elpy-installation-instructions-should-run ()
  (unwind-protect
      (progn
        (elpy-installation-instructions "Hello, World")
        (with-current-buffer "*Elpy Installation*"
          ;; Is displayed
          (should (get-buffer-window (current-buffer)))
          (should (equal (point-min)
                         (window-start (selected-window))))
          (should (re-search-forward "Hello, World"))))
    (ignore-errors
      (kill-buffer "*Elpy Installation*"))))
