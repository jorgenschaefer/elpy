(ert-deftest elpy-multiedit-stop-should-delete-overlays ()
  (elpy-testcase ()
    (let* ((ov (make-overlay (point) (point)))
           (elpy-multiedit-overlays (list ov)))

      (elpy-multiedit-stop)

      (should-not (overlay-buffer ov))
      (should (null elpy-multiedit-overlays)))))
