;;; claude-state-test.el --- Tests for claude-state.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for the state machine and metadata operations.
;; Run with: emacs --batch -l ert -l claude-state.el -l test/claude-state-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)

;; Load the module under test
(add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name)))

;; Define the customization variables before loading claude-state
(defvar claude-worktree-dir "/tmp/test-worktrees"
  "Test directory for worktrees.")
(defvar claude-metadata-dir "/tmp/test-metadata"
  "Test directory for metadata.")

(require 'claude-state)

;;; State Machine Tests

(ert-deftest claude-state-test-valid-states ()
  "Test that all expected states are valid."
  (dolist (state '(creating active closing failed broken stuck))
    (should (claude-state-valid-p state))))

(ert-deftest claude-state-test-invalid-state ()
  "Test that unknown states are invalid."
  (should-not (claude-state-valid-p 'unknown))
  (should-not (claude-state-valid-p 'pending))
  (should-not (claude-state-valid-p nil)))

(ert-deftest claude-state-test-valid-transitions ()
  "Test all valid state transitions."
  ;; creating -> active
  (should (claude-state-transition-valid-p 'creating 'active))
  ;; creating -> failed
  (should (claude-state-transition-valid-p 'creating 'failed))
  ;; active -> closing
  (should (claude-state-transition-valid-p 'active 'closing))
  ;; active -> broken
  (should (claude-state-transition-valid-p 'active 'broken))
  ;; closing -> stuck
  (should (claude-state-transition-valid-p 'closing 'stuck))
  ;; stuck -> closing (retry)
  (should (claude-state-transition-valid-p 'stuck 'closing))
  ;; broken -> active (repair)
  (should (claude-state-transition-valid-p 'broken 'active))
  ;; broken -> closing (cleanup)
  (should (claude-state-transition-valid-p 'broken 'closing)))

(ert-deftest claude-state-test-invalid-transitions ()
  "Test that invalid transitions are rejected."
  ;; Can't go back to creating
  (should-not (claude-state-transition-valid-p 'active 'creating))
  (should-not (claude-state-transition-valid-p 'closing 'creating))
  ;; Can't skip states
  (should-not (claude-state-transition-valid-p 'creating 'closing))
  (should-not (claude-state-transition-valid-p 'creating 'stuck))
  ;; Failed is terminal (only deletion)
  (should-not (claude-state-transition-valid-p 'failed 'active))
  (should-not (claude-state-transition-valid-p 'failed 'creating)))

;;; Naming Tests

(ert-deftest claude-state-test-workspace-name ()
  "Test workspace name generation."
  (should (equal (claude--workspace-name "repo" "branch")
                 "repo:branch"))
  (should (equal (claude--workspace-name "dotfiles" "__home__")
                 "dotfiles:__home__")))

(ert-deftest claude-state-test-buffer-name ()
  "Test buffer name generation."
  (should (equal (claude--buffer-name "repo" "branch")
                 "*claude:repo:branch*"))
  (should (equal (claude--buffer-name "dotfiles" "__home__")
                 "*claude:dotfiles:__home__*")))

(ert-deftest claude-state-test-terminal-buffer-name ()
  "Test terminal buffer name generation."
  (should (equal (claude--terminal-buffer-name "repo" "branch" 1)
                 "*term:repo:branch:1*"))
  (should (equal (claude--terminal-buffer-name "repo" "branch" 42)
                 "*term:repo:branch:42*")))

(ert-deftest claude-state-test-parse-workspace-name ()
  "Test parsing workspace names."
  ;; Valid names
  (should (equal (claude--parse-workspace-name "repo:branch")
                 '("repo" . "branch")))
  (should (equal (claude--parse-workspace-name "dotfiles:__home__")
                 '("dotfiles" . "__home__")))
  ;; Branch with special chars (valid git branch)
  (should (equal (claude--parse-workspace-name "repo:feature/auth")
                 '("repo" . "feature/auth")))
  ;; Invalid names
  (should (null (claude--parse-workspace-name "invalid")))
  (should (null (claude--parse-workspace-name nil)))
  (should (null (claude--parse-workspace-name ""))))

(ert-deftest claude-state-test-parse-workspace-name-inverse ()
  "Test that workspace-name and parse-workspace-name are inverses."
  (let ((repo "dotfiles")
        (branch "feature-auth"))
    (let* ((name (claude--workspace-name repo branch))
           (parsed (claude--parse-workspace-name name)))
      (should (equal (car parsed) repo))
      (should (equal (cdr parsed) branch)))))

;;; Home Workspace Tests

(ert-deftest claude-state-test-home-workspace-detection-by-name ()
  "Test home workspace detection from workspace name."
  (should (claude--home-workspace-p "repo:__home__"))
  (should (claude--home-workspace-p "dotfiles:__home__"))
  (should-not (claude--home-workspace-p "repo:feature"))
  (should-not (claude--home-workspace-p "repo:home"))
  (should-not (claude--home-workspace-p "repo:__home"))
  (should-not (claude--home-workspace-p nil)))

(ert-deftest claude-state-test-home-workspace-detection-by-metadata ()
  "Test home workspace detection from metadata plist."
  (should (claude--home-workspace-p '(:type "home" :branch_name "__home__")))
  (should-not (claude--home-workspace-p '(:type "worktree" :branch_name "feature")))
  (should-not (claude--home-workspace-p '(:type "worktree" :branch_name "__home__"))))

;;; Path Tests

(ert-deftest claude-state-test-worktree-path ()
  "Test worktree path generation."
  (let ((claude-worktree-dir "/tmp/worktrees"))
    (should (equal (claude--worktree-path "repo" "branch")
                   "/tmp/worktrees/repo/branch"))))

(ert-deftest claude-state-test-metadata-path ()
  "Test metadata path generation."
  (let ((claude-metadata-dir "/tmp/metadata"))
    (should (equal (claude--metadata-path "repo" "branch")
                   "/tmp/metadata/repo/branch.json"))))

(ert-deftest claude-state-test-repo-name ()
  "Test repo name extraction from path."
  (should (equal (claude--repo-name "/Users/user/repos/myrepo")
                 "myrepo"))
  (should (equal (claude--repo-name "/Users/user/repos/myrepo/")
                 "myrepo"))
  (should (equal (claude--repo-name "~/dotfiles")
                 "dotfiles")))

;;; Metadata Operations Tests

(ert-deftest claude-state-test-metadata-roundtrip ()
  "Test writing and reading metadata."
  (let ((claude-metadata-dir (make-temp-file "claude-test-meta" t)))
    (unwind-protect
        (progn
          ;; Write metadata
          (claude-metadata-write "test-repo" "test-branch"
                                 '(:version 1
                                   :status "active"
                                   :type "worktree"
                                   :parent_branch "main"
                                   :parent_repo "/tmp/repo"))
          ;; Read it back
          (let ((meta (claude-metadata-read "test-repo" "test-branch")))
            (should meta)
            (should (equal (plist-get meta :version) 1))
            (should (equal (plist-get meta :status) "active"))
            (should (equal (plist-get meta :type) "worktree"))
            (should (equal (plist-get meta :parent_branch) "main"))
            (should (equal (plist-get meta :parent_repo) "/tmp/repo")))
          ;; Delete it
          (claude-metadata-delete "test-repo" "test-branch")
          (should (null (claude-metadata-read "test-repo" "test-branch"))))
      ;; Cleanup
      (delete-directory claude-metadata-dir t))))

(ert-deftest claude-state-test-metadata-auto-version ()
  "Test that metadata gets version automatically."
  (let ((claude-metadata-dir (make-temp-file "claude-test-meta" t)))
    (unwind-protect
        (progn
          ;; Write without version
          (claude-metadata-write "test-repo" "test-branch"
                                 '(:status "active" :parent_branch "main"))
          ;; Read back should have version
          (let ((meta (claude-metadata-read "test-repo" "test-branch")))
            (should (equal (plist-get meta :version) 1))))
      (delete-directory claude-metadata-dir t))))

;;; Migration Tests

(ert-deftest claude-state-test-migration-v0-to-v1 ()
  "Test v0 to v1 metadata migration."
  (let ((claude-metadata-dir (make-temp-file "claude-test-meta" t))
        (claude-worktree-dir "/tmp/worktrees"))
    (unwind-protect
        (progn
          ;; Write v0 format directly (bypassing claude-metadata-write)
          (let ((dir (expand-file-name "test-repo" claude-metadata-dir)))
            (make-directory dir t)
            (with-temp-file (expand-file-name "feature.json" dir)
              (insert (json-encode '(:parent_branch "main"
                                     :parent_repo "/tmp/repo"
                                     :created "2026-01-01T00:00:00Z")))))
          ;; Read should trigger migration
          (let ((meta (claude-metadata-read "test-repo" "feature")))
            ;; Check v1 fields
            (should (equal (plist-get meta :version) 1))
            (should (equal (plist-get meta :status) "active"))
            (should (equal (plist-get meta :type) "worktree"))
            (should (equal (plist-get meta :repo_name) "test-repo"))
            (should (equal (plist-get meta :branch_name) "feature"))
            ;; Original fields preserved
            (should (equal (plist-get meta :parent_branch) "main"))
            (should (equal (plist-get meta :parent_repo) "/tmp/repo"))
            (should (equal (plist-get meta :created_at) "2026-01-01T00:00:00Z"))))
      (delete-directory claude-metadata-dir t))))

(ert-deftest claude-state-test-no-migration-for-v1 ()
  "Test that v1 metadata is not migrated."
  (let ((claude-metadata-dir (make-temp-file "claude-test-meta" t)))
    (unwind-protect
        (progn
          ;; Write v1 format
          (claude-metadata-write "test-repo" "feature"
                                 '(:version 1
                                   :status "broken"
                                   :type "worktree"))
          ;; Read should return as-is
          (let ((meta (claude-metadata-read "test-repo" "feature")))
            (should (equal (plist-get meta :version) 1))
            (should (equal (plist-get meta :status) "broken"))))
      (delete-directory claude-metadata-dir t))))

;;; Validation Tests

(ert-deftest claude-state-test-validate-branch-name-valid ()
  "Test valid branch names pass validation."
  (should (null (claude--validate-branch-name "feature")))
  (should (null (claude--validate-branch-name "feature-auth")))
  (should (null (claude--validate-branch-name "feature/auth")))
  (should (null (claude--validate-branch-name "fix_bug_123"))))

(ert-deftest claude-state-test-validate-branch-name-invalid ()
  "Test invalid branch names fail validation."
  ;; Empty
  (should (claude--validate-branch-name ""))
  ;; Starts with dash
  (should (claude--validate-branch-name "-feature"))
  ;; Contains ..
  (should (claude--validate-branch-name "feature..branch"))
  ;; Ends with .
  (should (claude--validate-branch-name "feature."))
  ;; Contains @{
  (should (claude--validate-branch-name "feature@{1}"))
  ;; Reserved name
  (should (claude--validate-branch-name "__home__")))

;;; Listing Tests

(ert-deftest claude-state-test-list-workspaces-by-status ()
  "Test listing workspaces filtered by status."
  (let ((claude-metadata-dir (make-temp-file "claude-test-meta" t)))
    (unwind-protect
        (progn
          ;; Create workspaces with different statuses
          (claude-metadata-write "repo1" "branch1" '(:version 1 :status "active"))
          (claude-metadata-write "repo1" "branch2" '(:version 1 :status "active"))
          (claude-metadata-write "repo2" "branch1" '(:version 1 :status "broken"))
          (claude-metadata-write "repo2" "branch2" '(:version 1 :status "creating"))
          ;; Test filtering
          (let ((active (claude--list-workspaces-by-status "active")))
            (should (= (length active) 2)))
          (let ((broken (claude--list-workspaces-by-status 'broken)))
            (should (= (length broken) 1))
            (should (equal (caar broken) "repo2")))
          (let ((creating (claude--list-workspaces-by-status "creating")))
            (should (= (length creating) 1))))
      (delete-directory claude-metadata-dir t))))

;;; Initial Metadata Creation Tests

(ert-deftest claude-state-test-create-metadata-worktree ()
  "Test creating initial metadata for worktree workspace."
  (let ((claude-metadata-dir (make-temp-file "claude-test-meta" t))
        (claude-worktree-dir "/tmp/worktrees"))
    (unwind-protect
        (progn
          (claude--create-metadata "repo" "feature" "main" "/path/to/repo")
          (let ((meta (claude-metadata-read "repo" "feature")))
            (should (equal (plist-get meta :version) 1))
            (should (equal (plist-get meta :status) "creating"))
            (should (equal (plist-get meta :type) "worktree"))
            (should (equal (plist-get meta :repo_name) "repo"))
            (should (equal (plist-get meta :branch_name) "feature"))
            (should (equal (plist-get meta :parent_branch) "main"))
            (should (equal (plist-get meta :parent_repo) "/path/to/repo"))
            (should (equal (plist-get meta :worktree_path) "/tmp/worktrees/repo/feature"))
            (should (plist-get meta :created_at))
            (should (plist-get meta :updated_at))))
      (delete-directory claude-metadata-dir t))))

(ert-deftest claude-state-test-create-metadata-home ()
  "Test creating initial metadata for home workspace."
  (let ((claude-metadata-dir (make-temp-file "claude-test-meta" t))
        (claude-worktree-dir "/tmp/worktrees"))
    (unwind-protect
        (progn
          (claude--create-metadata "repo" "__home__" nil "/path/to/repo" "home")
          (let ((meta (claude-metadata-read "repo" "__home__")))
            (should (equal (plist-get meta :type) "home"))
            (should (null (plist-get meta :worktree_path)))
            (should (null (plist-get meta :parent_branch)))))
      (delete-directory claude-metadata-dir t))))

;;; Workflow Phase Tests

(ert-deftest claude-state-test-workflow-phase-nil-when-no-workflow ()
  "Test that workflow phase returns nil when no workflow in metadata."
  (let ((claude-metadata-dir (make-temp-file "claude-test-meta" t)))
    (unwind-protect
        (progn
          (claude-metadata-write "repo" "feature"
                                 '(:version 1 :status "active"))
          (should (null (claude--workflow-phase "repo" "feature"))))
      (delete-directory claude-metadata-dir t))))

(ert-deftest claude-state-test-workflow-phase-returns-phase ()
  "Test that workflow phase returns phase when workflow present."
  (let ((claude-metadata-dir (make-temp-file "claude-test-meta" t)))
    (unwind-protect
        (progn
          (claude-metadata-write "repo" "feature"
                                 '(:version 1
                                   :status "active"
                                   :workflow (:plan "test-plan"
                                              :phase "implement"
                                              :started "2026-02-01T10:00:00Z")))
          (should (equal (claude--workflow-phase "repo" "feature") "implement")))
      (delete-directory claude-metadata-dir t))))

(ert-deftest claude-state-test-workflow-phase-nil-for-nonexistent ()
  "Test that workflow phase returns nil for nonexistent workspace."
  (let ((claude-metadata-dir (make-temp-file "claude-test-meta" t)))
    (unwind-protect
        (should (null (claude--workflow-phase "nonexistent" "nope")))
      (delete-directory claude-metadata-dir t))))

(provide 'claude-state-test)
;;; claude-state-test.el ends here
