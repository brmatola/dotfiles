;;; claude-cleanup-test.el --- Tests for claude-cleanup.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for the cleanup workflow.
;; Run with: emacs --batch -l ert -l claude-state.el -l claude-cleanup.el -l test/claude-cleanup-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)

;; Load the module under test
(add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name)))

;; Define the customization variables before loading modules
(defvar claude-worktree-dir "/tmp/test-worktrees"
  "Test directory for worktrees.")
(defvar claude-metadata-dir "/tmp/test-metadata"
  "Test directory for metadata.")

;; Stub out functions that require vterm/Doom
(defun claude--kill-workspace-buffers (_repo-name _branch-name) nil)
(defun claude-worktree-remove (_repo-name _branch-name) nil)
(defun claude-worktree-remove-force (_repo-name _branch-name) nil)
(defun claude-git-commits-ahead (_path _branch) 0)
(defun claude-git-merge-branch (_repo _parent _branch) '(t . nil))
(defun claude-git-delete-branch (_repo _branch) nil)
(defun claude-git-has-uncommitted-changes (_path) nil)
(defun claude-git-fetch (_repo) nil)
(defun +workspace-current-name () "test:branch")
(defun +workspace-list-names () '("test:branch"))
(defun +workspace/switch-to (_name) nil)
(defun +workspace-kill (_name) nil)
(defun claude-workspace-current () nil)
(defun claude-workspace-list () nil)
(defun claude-monitor-stop () nil)

(require 'claude-state)
(require 'claude-cleanup)

;;; Keymap Tests

(ert-deftest claude-cleanup-test-keymap-has-pr-option ()
  "Test that cleanup keymap has the p binding for push-pr."
  (should (eq (lookup-key claude-cleanup-mode-map "p")
              #'claude-cleanup-push-pr)))

(ert-deftest claude-cleanup-test-keymap-has-all-bindings ()
  "Test that cleanup keymap has all expected bindings."
  (should (eq (lookup-key claude-cleanup-mode-map "v") #'claude-cleanup-view-diff))
  (should (eq (lookup-key claude-cleanup-mode-map "m") #'claude-cleanup-merge))
  (should (eq (lookup-key claude-cleanup-mode-map "p") #'claude-cleanup-push-pr))
  (should (eq (lookup-key claude-cleanup-mode-map "d") #'claude-cleanup-delete))
  (should (eq (lookup-key claude-cleanup-mode-map "c") #'claude-cleanup-cancel))
  (should (eq (lookup-key claude-cleanup-mode-map "q") #'claude-cleanup-cancel)))

(provide 'claude-cleanup-test)
;;; claude-cleanup-test.el ends here
