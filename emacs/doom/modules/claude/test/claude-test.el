;;; claude-test.el --- Tests for Claude workflow module -*- lexical-binding: t; -*-

;;; Commentary:
;; Integration tests for the Claude multi-workspace system.
;; Run with: ./test/run-tests.sh

;;; Code:

(require 'ert)

;; Load the modules under test
(add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name)))
(require 'claude-worktree)
(require 'claude-workspace)

;;; Unit Tests - Pure Functions

(ert-deftest claude-test-workspace-name ()
  "Test workspace name generation."
  (should (equal (claude-workspace-name "rithmly" "feat-auth")
                 "rithmly:feat-auth"))
  (should (equal (claude-workspace-name "dotfiles" "__home__")
                 "dotfiles:__home__")))

(ert-deftest claude-test-buffer-name ()
  "Test buffer name generation."
  (should (equal (claude-buffer-name "rithmly" "feat-auth")
                 "*claude:rithmly:feat-auth*"))
  (should (equal (claude-buffer-name "dotfiles" "__home__")
                 "*claude:dotfiles:__home__*")))

(ert-deftest claude-test-parse-workspace-name ()
  "Test parsing workspace names."
  (should (equal (claude-parse-workspace-name "rithmly:feat-auth")
                 '("rithmly" . "feat-auth")))
  (should (equal (claude-parse-workspace-name "dotfiles:__home__")
                 '("dotfiles" . "__home__")))
  ;; Invalid names
  (should (null (claude-parse-workspace-name "invalid")))
  (should (null (claude-parse-workspace-name "too:many:colons")))
  (should (null (claude-parse-workspace-name nil))))

(ert-deftest claude-test-home-workspace-p ()
  "Test home workspace detection."
  (should (claude-home-workspace-p "rithmly:__home__"))
  (should (claude-home-workspace-p "dotfiles:__home__"))
  (should-not (claude-home-workspace-p "rithmly:feat-auth"))
  (should-not (claude-home-workspace-p "rithmly:home"))
  (should-not (claude-home-workspace-p nil)))

(ert-deftest claude-test-repo-name ()
  "Test repo name extraction from path."
  (should (equal (claude-repo-name "/Users/bmatola/repos/rithmly")
                 "rithmly"))
  (should (equal (claude-repo-name "/Users/bmatola/repos/rithmly/")
                 "rithmly"))
  (should (equal (claude-repo-name "/path/to/my-project")
                 "my-project")))

(ert-deftest claude-test-metadata-path ()
  "Test metadata path generation."
  (let ((claude-metadata-dir "/tmp/test-metadata"))
    (should (equal (claude-metadata-path "rithmly" "feat-auth")
                   "/tmp/test-metadata/rithmly/feat-auth.json"))))

(ert-deftest claude-test-worktree-path ()
  "Test worktree path generation."
  (let ((claude-worktree-dir "/tmp/test-worktrees"))
    (should (equal (claude-worktree-path "rithmly" "feat-auth")
                   "/tmp/test-worktrees/rithmly/feat-auth"))))

;;; Integration Tests - Metadata

(ert-deftest claude-test-metadata-roundtrip ()
  "Test writing and reading metadata."
  (let ((claude-metadata-dir (make-temp-file "claude-test-meta" t)))
    (unwind-protect
        (progn
          ;; Write metadata (data is a plist)
          (claude-metadata-write "test-repo" "test-branch"
                                 '(:parent_branch "main"
                                   :parent_repo "/tmp/repo"
                                   :created "2026-02-01T00:00:00Z"))
          ;; Read it back
          (let ((meta (claude-metadata-read "test-repo" "test-branch")))
            (should meta)
            (should (equal (plist-get meta :parent_branch) "main"))
            (should (equal (plist-get meta :parent_repo) "/tmp/repo"))
            (should (plist-get meta :created)))
          ;; Delete it
          (claude-metadata-delete "test-repo" "test-branch")
          (should (null (claude-metadata-read "test-repo" "test-branch"))))
      ;; Cleanup
      (delete-directory claude-metadata-dir t))))

;;; Integration Tests - Terminal Naming

(ert-deftest claude-test-terminal-buffer-name-gaps ()
  "Test terminal buffer naming with gap reuse."
  ;; Mock existing buffers by creating them
  (let ((buf1 (get-buffer-create "*term:test:branch:1*"))
        (buf3 (get-buffer-create "*term:test:branch:3*"))
        (buf4 (get-buffer-create "*term:test:branch:4*")))
    (unwind-protect
        (progn
          ;; Should find gap at 2
          (should (equal (claude-terminal-buffer-name "test" "branch")
                         "*term:test:branch:2*"))
          ;; Create buffer 2
          (get-buffer-create "*term:test:branch:2*")
          ;; Now should get 5
          (should (equal (claude-terminal-buffer-name "test" "branch")
                         "*term:test:branch:5*")))
      ;; Cleanup
      (kill-buffer buf1)
      (kill-buffer buf3)
      (kill-buffer buf4)
      (when (get-buffer "*term:test:branch:2*")
        (kill-buffer "*term:test:branch:2*")))))

(provide 'claude-test)
;;; claude-test.el ends here
