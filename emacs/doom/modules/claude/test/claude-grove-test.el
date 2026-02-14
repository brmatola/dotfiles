;;; claude-grove-test.el --- Tests for grove CLI wrapper -*- lexical-binding: t; -*-

(require 'ert)
(add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name)))
(require 'claude-grove)

;;; Test Helpers

(defvar claude-grove-test--callback-result nil
  "Stores the last callback result for assertions.")

(defun claude-grove-test--callback (ok data error-msg)
  "Test callback that stores result."
  (setq claude-grove-test--callback-result
        (list :ok ok :data data :error error-msg)))

(defmacro claude-grove-test--with-clean-state (&rest body)
  "Run BODY with a fresh grove state."
  `(let ((claude-grove--pending (make-hash-table :test 'equal))
         (claude-grove-test--callback-result nil))
     ,@body))

;;; Executable Detection Tests

(ert-deftest claude-grove-test-not-found ()
  "Test callback when grove executable is not found."
  (claude-grove-test--with-clean-state
   (let ((claude-grove-executable "nonexistent-grove-binary-xyz"))
     (claude-grove-repo-list #'claude-grove-test--callback)
     (should (equal (plist-get claude-grove-test--callback-result :ok) nil))
     (should (equal (plist-get claude-grove-test--callback-result :error)
                    "grove not found")))))

;;; JSON Parsing Tests

(ert-deftest claude-grove-test-parse-success-envelope ()
  "Test parsing a successful JSON envelope."
  (let ((output "{\"ok\":true,\"data\":{\"repos\":[]}}"))
    (let* ((json-object-type 'plist)
           (json-key-type 'keyword)
           (parsed (json-parse-string output
                                      :object-type 'plist
                                      :null-object nil
                                      :false-object nil)))
      (should (eq (plist-get parsed :ok) t))
      (should (plist-get parsed :data)))))

(ert-deftest claude-grove-test-parse-error-envelope ()
  "Test parsing an error JSON envelope."
  (let ((output "{\"ok\":false,\"error\":\"repo not found\"}"))
    (let* ((json-object-type 'plist)
           (json-key-type 'keyword)
           (parsed (json-parse-string output
                                      :object-type 'plist
                                      :null-object nil
                                      :false-object nil)))
      (should (not (eq (plist-get parsed :ok) t)))
      (should (equal (plist-get parsed :error) "repo not found")))))

(ert-deftest claude-grove-test-parse-repo-list ()
  "Test parsing grove repo list JSON output."
  (let ((output "{\"ok\":true,\"data\":{\"repos\":[{\"name\":\"dotfiles\",\"path\":\"/Users/test/dotfiles\",\"exists\":true,\"workspaces\":[{\"id\":\"dotfiles-fix-zsh\",\"branch\":\"fix-zsh\",\"status\":\"active\",\"root\":\"/Users/test/worktrees/dotfiles/fix-zsh\",\"repoCount\":1}]}]}}"))
    (let* ((json-object-type 'plist)
           (json-key-type 'keyword)
           (parsed (json-parse-string output
                                      :object-type 'plist
                                      :null-object nil
                                      :false-object nil))
           (data (plist-get parsed :data))
           (repos (plist-get data :repos))
           (first-repo (aref repos 0))
           (workspaces (plist-get first-repo :workspaces))
           (first-ws (aref workspaces 0)))
      (should (equal (plist-get first-repo :name) "dotfiles"))
      (should (equal (plist-get first-ws :branch) "fix-zsh"))
      (should (equal (plist-get first-ws :status) "active")))))

(ert-deftest claude-grove-test-parse-workspace-status ()
  "Test parsing grove workspace status JSON output."
  (let ((output "{\"ok\":true,\"data\":{\"id\":\"acorn-feature-auth\",\"status\":\"active\",\"branch\":\"feature-auth\",\"repos\":[{\"name\":\"acorn\",\"role\":\"parent\",\"dirty\":0,\"commits\":2},{\"name\":\"public\",\"role\":\"child\",\"dirty\":2,\"commits\":5}]}}"))
    (let* ((json-object-type 'plist)
           (json-key-type 'keyword)
           (parsed (json-parse-string output
                                      :object-type 'plist
                                      :null-object nil
                                      :false-object nil))
           (data (plist-get parsed :data))
           (repos (plist-get data :repos))
           (parent (aref repos 0))
           (child (aref repos 1)))
      (should (equal (plist-get data :branch) "feature-auth"))
      (should (equal (plist-get parent :name) "acorn"))
      (should (equal (plist-get parent :role) "parent"))
      (should (equal (plist-get parent :dirty) 0))
      (should (equal (plist-get parent :commits) 2))
      (should (equal (plist-get child :dirty) 2)))))

;;; In-Flight Guard Tests

(ert-deftest claude-grove-test-pending-check ()
  "Test the in-flight guard tracking."
  (claude-grove-test--with-clean-state
   (should (not (claude-grove--request-pending-p "repo-list")))
   ;; Simulate an in-flight process
   (let ((fake-proc (start-process "test-proc" nil "sleep" "100")))
     (puthash "repo-list" fake-proc claude-grove--pending)
     (should (claude-grove--request-pending-p "repo-list"))
     ;; Clean up
     (delete-process fake-proc)
     ;; After process dies, should not be pending
     (should (not (claude-grove--request-pending-p "repo-list"))))))

;;; Error Extraction Tests

(ert-deftest claude-grove-test-extract-error-from-json ()
  "Test error extraction from JSON error response."
  (should (equal (claude-grove--extract-error
                  "{\"ok\":false,\"error\":\"repo not found\"}")
                 "repo not found")))

(ert-deftest claude-grove-test-extract-error-from-plain-text ()
  "Test error extraction from plain text output."
  (should (equal (claude-grove--extract-error "something went wrong")
                 "something went wrong")))

(ert-deftest claude-grove-test-extract-error-nil ()
  "Test error extraction from nil/empty."
  (should (null (claude-grove--extract-error nil)))
  (should (null (claude-grove--extract-error ""))))

;;; Monitor Tests

(require 'claude-monitor)

(ert-deftest claude-monitor-test-workspace-for-buffer ()
  "Test buffer name to workspace name extraction."
  (with-temp-buffer
    (rename-buffer "*claude:dotfiles:fix-zsh*" t)
    (should (equal (claude-monitor--workspace-for-buffer (current-buffer))
                   "dotfiles:fix-zsh")))
  ;; Non-claude buffer
  (with-temp-buffer
    (should (null (claude-monitor--workspace-for-buffer (current-buffer))))))

(ert-deftest claude-monitor-test-workspace-for-buffer-home ()
  "Test buffer name extraction for home workspace."
  (with-temp-buffer
    (rename-buffer "*claude:dotfiles:home*" t)
    (should (equal (claude-monitor--workspace-for-buffer (current-buffer))
                   "dotfiles:home"))))

(ert-deftest claude-monitor-test-attention-patterns ()
  "Test attention pattern matching."
  (should (claude--matches-attention-pattern-p "Allow bash to run this command? [y/n]"))
  (should (claude--matches-attention-pattern-p "> "))
  (should (claude--matches-attention-pattern-p "Do you want to proceed"))
  (should (not (claude--matches-attention-pattern-p "Building project...")))
  (should (not (claude--matches-attention-pattern-p nil))))

(ert-deftest claude-monitor-test-workspace-attention ()
  "Test workspace attention query."
  ;; No buffer → nil
  (should (null (claude-workspace-attention "nonexistent:branch")))
  ;; Buffer exists, not needing attention → working
  (with-temp-buffer
    (rename-buffer "*claude:testrepo:testbranch*" t)
    (setq claude--needs-attention nil)
    (should (eq (claude-workspace-attention "testrepo:testbranch") 'working)))
  ;; Buffer exists, needs attention → idle
  (with-temp-buffer
    (rename-buffer "*claude:testrepo2:testbranch2*" t)
    (setq claude--needs-attention t)
    (should (eq (claude-workspace-attention "testrepo2:testbranch2") 'idle))))

(ert-deftest claude-monitor-test-claude-buffers ()
  "Test finding claude buffers."
  (let ((buf1 (generate-new-buffer "*claude:repo1:branch1*"))
        (buf2 (generate-new-buffer "*claude:repo2:branch2*"))
        (buf3 (generate-new-buffer "*not-claude*")))
    (unwind-protect
        (let ((claude-bufs (claude-monitor--claude-buffers)))
          (should (memq buf1 claude-bufs))
          (should (memq buf2 claude-bufs))
          (should (not (memq buf3 claude-bufs))))
      (kill-buffer buf1)
      (kill-buffer buf2)
      (kill-buffer buf3))))

(provide 'claude-grove-test)
;;; claude-grove-test.el ends here
