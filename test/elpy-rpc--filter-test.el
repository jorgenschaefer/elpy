(ert-deftest elpy-rpc--filter-should-not-fail-on-dead-buffer ()
  (elpy-testcase ()
    (mletf* ((buf (get-buffer-create "*temp*"))
             (process-buffer (proc) buf))
      (kill-buffer buf)
      (elpy-rpc--filter 'process ""))))

(ert-deftest elpy-rpc--filter-should-read-json ()
  (elpy-testcase ()
    (mletf* ((process-buffer (proc) (current-buffer))
             (read nil)
             (elpy-rpc--handle-json (json) (setq read json)))

      (elpy-rpc--filter 'process "{\"fnord\": 5}\n")

      (should (equal read '((fnord . 5)))))))

(ert-deftest elpy-rpc--filter-should-read-json-in-parts ()
  (elpy-testcase ()
    (mletf* ((process-buffer (proc) (current-buffer))
             (read nil)
             (elpy-rpc--handle-json (json) (setq read json)))

      (elpy-rpc--filter 'process "{\"fnord\":")
      (elpy-rpc--filter 'process " 5}\n")

      (should (equal read '((fnord . 5)))))))

(ert-deftest elpy-rpc--filter-should-know-elpy-1-1 ()
  (elpy-testcase ()
    (mletf* ((process-buffer (proc) (current-buffer))
             (called-version nil)
             (elpy-rpc--check-backend-version
              (version)
              (setq called-version version)))

      (elpy-rpc--filter 'process "elpy-rpc ready\n")

      (should (equal called-version "1.1")))))

(ert-deftest elpy-rpc--filter-should-parse-version-greeting ()
  (elpy-testcase ()
    (mletf* ((process-buffer (proc) (current-buffer))
             (called-version nil)
             (elpy-rpc--check-backend-version
              (version)
              (setq called-version version)))

      (elpy-rpc--filter 'process "elpy-rpc ready (23.5)\n")

      (should (equal called-version "23.5")))))

(ert-deftest elpy-rpc--filter-should-call-unexpected-line ()
  (elpy-testcase ()
    (mletf* ((process-buffer (proc) (current-buffer))
             (unexpected-called nil)
             (elpy-rpc--handle-unexpected-line
              (line)
              (setq unexpected-called t)))

      (elpy-rpc--filter 'process "Beware the Jubjub bird\n")

      (should unexpected-called))))
