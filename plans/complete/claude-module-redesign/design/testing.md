# Claude Module: Testing Strategy

Last updated: 2026-02-01

## Test Pyramid

```
        /\
       /  \     Integration (~10%)
      /    \    - Full flows, real git, real Doom
     /──────\
    /        \  Reconciler (~20%)
   /          \ - Mocked systems, logic verification
  /────────────\
 /              \ Unit/State (~70%)
/                \ - Pure functions, state transitions
──────────────────
```

## Layer 1: Unit/State Tests (~70%)

Pure functions, run in batch mode, no Doom required.

### State Machine Tests

```elisp
;; Test all valid transitions
(ert-deftest claude-state-test-creating-to-active ()
  (let ((state (claude-state-create :status 'creating)))
    (should (eq (claude-state-transition state 'active) 'active))))

(ert-deftest claude-state-test-invalid-transition ()
  (let ((state (claude-state-create :status 'active)))
    (should-error (claude-state-transition state 'creating))))

;; Test all states
(ert-deftest claude-state-test-all-states ()
  (dolist (status '(creating active closing failed broken stuck))
    (let ((state (claude-state-create :status status)))
      (should (eq (claude-state-status state) status)))))
```

### Metadata Tests

```elisp
(ert-deftest claude-metadata-test-roundtrip ()
  (let* ((temp-dir (make-temp-file "claude-test" t))
         (claude-metadata-dir temp-dir)
         (metadata '((status . "active")
                     (type . "worktree")
                     (repo_name . "test-repo")
                     (branch_name . "feature"))))
    (unwind-protect
        (progn
          (claude-metadata-write "test-repo" "feature" metadata)
          (should (equal (claude-metadata-read "test-repo" "feature")
                         metadata)))
      (delete-directory temp-dir t))))
```

### Naming Convention Tests

```elisp
(ert-deftest claude-naming-test-workspace-name ()
  (should (equal (claude--workspace-name "repo" "branch") "repo:branch")))

(ert-deftest claude-naming-test-buffer-name ()
  (should (equal (claude--buffer-name "repo" "branch") "*claude:repo:branch*")))

(ert-deftest claude-naming-test-parse-workspace ()
  (should (equal (claude--parse-workspace-name "repo:branch")
                 '("repo" . "branch"))))

(ert-deftest claude-naming-test-home-detection ()
  (should (claude--home-workspace-p "repo:__home__"))
  (should-not (claude--home-workspace-p "repo:feature")))
```

### Path Utility Tests

```elisp
(ert-deftest claude-path-test-worktree-path ()
  (let ((claude-worktree-dir "/tmp/worktrees"))
    (should (equal (claude--worktree-path "repo" "branch")
                   "/tmp/worktrees/repo/branch"))))

(ert-deftest claude-path-test-metadata-path ()
  (let ((claude-metadata-dir "/tmp/metadata"))
    (should (equal (claude--metadata-path "repo" "branch")
                   "/tmp/metadata/repo/branch.json"))))
```

## Layer 2: Reconciler Tests (~20%)

Mock git/doom/vterm, test reconciliation logic.

### Mock Setup

```elisp
(defvar claude-test--mock-worktrees nil)
(defvar claude-test--mock-workspaces nil)
(defvar claude-test--mock-buffers nil)

(defun claude-test--mock-check-worktree (metadata)
  (member (plist-get metadata :worktree_path) claude-test--mock-worktrees))

(defun claude-test--mock-check-doom (metadata)
  (member (claude--workspace-name metadata) claude-test--mock-workspaces))

(defun claude-test--mock-check-vterm (metadata)
  (member (claude--buffer-name metadata) claude-test--mock-buffers))
```

### Reconciler Tests

```elisp
(ert-deftest claude-reconcile-test-all-healthy ()
  (let ((claude-test--mock-worktrees '("/tmp/worktrees/repo/branch"))
        (claude-test--mock-workspaces '("repo:branch"))
        (claude-test--mock-buffers '("*claude:repo:branch*"))
        (metadata '(:status "active"
                    :worktree_path "/tmp/worktrees/repo/branch"
                    :repo_name "repo"
                    :branch_name "branch")))
    (cl-letf (((symbol-function 'claude--check-worktree) #'claude-test--mock-check-worktree)
              ((symbol-function 'claude--check-doom-workspace) #'claude-test--mock-check-doom)
              ((symbol-function 'claude--check-vterm-buffer) #'claude-test--mock-check-vterm))
      (should (eq (claude--reconcile metadata) 'active)))))

(ert-deftest claude-reconcile-test-missing-worktree ()
  (let ((claude-test--mock-worktrees nil)  ; Worktree missing
        (claude-test--mock-workspaces '("repo:branch"))
        (claude-test--mock-buffers '("*claude:repo:branch*"))
        (metadata '(:status "active"
                    :worktree_path "/tmp/worktrees/repo/branch"
                    :repo_name "repo"
                    :branch_name "branch")))
    (cl-letf (((symbol-function 'claude--check-worktree) #'claude-test--mock-check-worktree)
              ((symbol-function 'claude--check-doom-workspace) #'claude-test--mock-check-doom)
              ((symbol-function 'claude--check-vterm-buffer) #'claude-test--mock-check-vterm)
              ((symbol-function 'claude-metadata-write) #'ignore))
      (should (eq (claude--reconcile metadata) 'broken)))))

(ert-deftest claude-reconcile-test-repairs-doom ()
  (let ((repaired nil)
        (claude-test--mock-worktrees '("/tmp/worktrees/repo/branch"))
        (claude-test--mock-workspaces nil)  ; Workspace missing
        (claude-test--mock-buffers '("*claude:repo:branch*"))
        (metadata '(:status "active"
                    :worktree_path "/tmp/worktrees/repo/branch"
                    :repo_name "repo"
                    :branch_name "branch")))
    (cl-letf (((symbol-function 'claude--check-worktree) #'claude-test--mock-check-worktree)
              ((symbol-function 'claude--check-doom-workspace) #'claude-test--mock-check-doom)
              ((symbol-function 'claude--check-vterm-buffer) #'claude-test--mock-check-vterm)
              ((symbol-function 'claude--repair-doom-workspace)
               (lambda (_) (setq repaired t))))
      (claude--reconcile metadata)
      (should repaired))))
```

## Layer 3: Integration Tests (~10%)

Full flows with real git repos in temp directories.

### Test Harness

```elisp
(defmacro claude-integration-test (name &rest body)
  "Run integration test with fresh temp environment."
  `(ert-deftest ,name ()
     (let* ((temp-base (make-temp-file "claude-int-test" t))
            (claude-worktree-dir (expand-file-name "worktrees" temp-base))
            (claude-metadata-dir (expand-file-name "metadata" temp-base))
            (test-repo (expand-file-name "test-repo" temp-base)))
       (unwind-protect
           (progn
             ;; Setup
             (make-directory claude-worktree-dir t)
             (make-directory claude-metadata-dir t)
             (claude-test--create-git-repo test-repo)
             ;; Run test
             ,@body)
         ;; Teardown
         (delete-directory temp-base t)))))

(defun claude-test--create-git-repo (path)
  "Create a minimal git repo at PATH."
  (make-directory path t)
  (let ((default-directory path))
    (shell-command "git init")
    (shell-command "git commit --allow-empty -m 'Initial commit'")))
```

### Integration Test Cases

```elisp
(claude-integration-test claude-int-test-full-lifecycle
  ;; Create workspace
  (let ((workspace (claude-create-workspace-noninteractive
                    test-repo "test-branch" "main")))
    ;; Verify created
    (should (claude-metadata-read "test-repo" "test-branch"))
    (should (+workspace-exists-p "test-repo:test-branch"))

    ;; Simulate some work
    (let ((default-directory (claude--worktree-path "test-repo" "test-branch")))
      (write-region "test" nil "test-file.txt")
      (shell-command "git add . && git commit -m 'Test commit'"))

    ;; Close with merge
    (claude-close-workspace-noninteractive "test-repo" "test-branch" 'merge)

    ;; Verify cleaned up
    (should-not (claude-metadata-read "test-repo" "test-branch"))
    (should-not (+workspace-exists-p "test-repo:test-branch"))

    ;; Verify merged
    (let ((default-directory test-repo))
      (should (= 0 (shell-command "git log --oneline | grep -q 'Test commit'"))))))

(claude-integration-test claude-int-test-broken-detection
  ;; Create workspace
  (claude-create-workspace-noninteractive test-repo "test-branch" "main")

  ;; Manually delete worktree (simulate crash)
  (delete-directory (claude--worktree-path "test-repo" "test-branch") t)

  ;; Reconcile should detect broken
  (let ((metadata (claude-metadata-read "test-repo" "test-branch")))
    (claude--reconcile metadata)
    (should (equal (plist-get (claude-metadata-read "test-repo" "test-branch") :status)
                   "broken"))))

(claude-integration-test claude-int-test-conflict-handling
  ;; Create workspace
  (claude-create-workspace-noninteractive test-repo "test-branch" "main")

  ;; Make conflicting change in main
  (let ((default-directory test-repo))
    (write-region "main content" nil "conflict-file.txt")
    (shell-command "git add . && git commit -m 'Main change'"))

  ;; Make conflicting change in worktree
  (let ((default-directory (claude--worktree-path "test-repo" "test-branch")))
    (write-region "branch content" nil "conflict-file.txt")
    (shell-command "git add . && git commit -m 'Branch change'"))

  ;; Attempt close - should go to stuck state
  (claude-close-workspace-noninteractive "test-repo" "test-branch" 'merge)
  (should (equal (plist-get (claude-metadata-read "test-repo" "test-branch") :status)
                 "stuck")))
```

## Running Tests

### Unit/State Tests (Fast)

```bash
#!/bin/bash
# test/run-unit-tests.sh
emacs --batch \
  -l ert \
  -l claude-state.el \
  -l test/claude-state-test.el \
  -l test/claude-naming-test.el \
  -l test/claude-metadata-test.el \
  -f ert-run-tests-batch-and-exit
```

### Reconciler Tests (Medium)

```bash
#!/bin/bash
# test/run-reconciler-tests.sh
emacs --batch \
  -l ert \
  -l claude-state.el \
  -l claude-reconcile.el \
  -l test/claude-reconcile-test.el \
  -f ert-run-tests-batch-and-exit
```

### Integration Tests (Slow)

```bash
#!/bin/bash
# test/run-integration-tests.sh
# Requires Doom - run interactively or with full init
doom run \
  -l test/claude-integration-test.el \
  --eval "(ert-run-tests-batch-and-exit)"
```

### All Tests

```bash
#!/bin/bash
# test/run-all-tests.sh
echo "=== Unit Tests ==="
./test/run-unit-tests.sh || exit 1

echo "=== Reconciler Tests ==="
./test/run-reconciler-tests.sh || exit 1

echo "=== Integration Tests ==="
./test/run-integration-tests.sh || exit 1

echo "=== All tests passed ==="
```

## Test Coverage Goals

| Component | Target Coverage |
|-----------|-----------------|
| State machine | 100% of transitions |
| Metadata operations | 100% of CRUD |
| Naming conventions | 100% of functions |
| Path utilities | 100% of functions |
| Reconciler logic | All state combinations |
| Integration flows | Happy path + key error cases |

## Additional Test Cases

### Concurrent Operations

```elisp
(claude-integration-test claude-int-test-concurrent-create
  ;; Start two workspace creations "simultaneously"
  (let ((ws1-done nil)
        (ws2-done nil))
    ;; Note: In practice these run sequentially in Emacs
    ;; but we test that state doesn't get corrupted
    (claude-create-workspace-noninteractive test-repo "branch-1" "main")
    (claude-create-workspace-noninteractive test-repo "branch-2" "main")

    ;; Both should exist and be independent
    (should (claude-metadata-read "test-repo" "branch-1"))
    (should (claude-metadata-read "test-repo" "branch-2"))
    (should (+workspace-exists-p "test-repo:branch-1"))
    (should (+workspace-exists-p "test-repo:branch-2"))))

(claude-integration-test claude-int-test-close-during-create
  ;; This shouldn't happen in practice, but test the edge case
  (let* ((repo-name "test-repo")
         (branch-name "test-branch"))
    ;; Manually set metadata to creating state
    (claude-metadata-write repo-name branch-name
                           '(:version 1 :status "creating" :type "worktree"))

    ;; Attempting to close should be blocked
    (should-error (claude-close-workspace-noninteractive repo-name branch-name 'delete)
                  :type 'user-error)))
```

### Crash Recovery

```elisp
(claude-integration-test claude-int-test-crash-during-creation
  ;; Simulate crash: metadata exists with status=creating, but no worktree
  (let ((repo-name "test-repo")
        (branch-name "test-branch"))
    (claude-metadata-write repo-name branch-name
                           (list :version 1
                                 :status "creating"
                                 :type "worktree"
                                 :created_at (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                                                                 (time-subtract nil 120) t)))

    ;; Startup recovery should mark as failed (stalled creation)
    (claude--check-stalled-creations)

    (should (equal (plist-get (claude-metadata-read repo-name branch-name) :status)
                   "failed"))))

(claude-integration-test claude-int-test-crash-during-cleanup
  ;; Simulate crash: metadata has cleanup_progress but status=closing
  (claude-create-workspace-noninteractive test-repo "test-branch" "main")

  ;; Simulate partial cleanup state
  (let* ((metadata (claude-metadata-read "test-repo" "test-branch")))
    (plist-put metadata :status "closing")
    (plist-put metadata :cleanup_progress
               '(:buffers_killed t :workspace_removed nil))
    (claude-metadata-write "test-repo" "test-branch" metadata))

  ;; Recovery should detect and mark stuck
  (claude--recover-in-flight-workspaces)

  (should (equal (plist-get (claude-metadata-read "test-repo" "test-branch") :status)
                 "stuck")))
```

### Migration

```elisp
(ert-deftest claude-migration-test-v0-to-v1 ()
  (let* ((temp-dir (make-temp-file "claude-test" t))
         (claude-metadata-dir temp-dir)
         (v0-metadata '(:parent_branch "main"
                        :parent_repo "/tmp/repo"
                        :created "2026-01-01T00:00:00Z")))
    (unwind-protect
        (progn
          ;; Write v0 format directly
          (make-directory (expand-file-name "test-repo" temp-dir) t)
          (with-temp-file (expand-file-name "test-repo/feature.json" temp-dir)
            (insert (json-encode v0-metadata)))

          ;; Read should trigger migration
          (let ((migrated (claude-metadata-read "test-repo" "feature")))
            (should (= (plist-get migrated :version) 1))
            (should (equal (plist-get migrated :status) "active"))
            (should (equal (plist-get migrated :type) "worktree"))
            (should (equal (plist-get migrated :repo_name) "test-repo"))
            (should (equal (plist-get migrated :branch_name) "feature"))
            ;; Original fields preserved
            (should (equal (plist-get migrated :parent_branch) "main"))))
      (delete-directory temp-dir t))))
```

### Network Failures

```elisp
(claude-integration-test claude-int-test-merge-network-failure
  ;; Create workspace with commits
  (claude-create-workspace-noninteractive test-repo "test-branch" "main")
  (let ((default-directory (claude--worktree-path "test-repo" "test-branch")))
    (write-region "test" nil "file.txt")
    (shell-command "git add . && git commit -m 'Test'"))

  ;; Mock git push to fail
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (cmd)
               (if (string-match-p "git push" cmd)
                   "fatal: Could not read from remote repository"
                 (funcall #'shell-command-to-string cmd)))))

    ;; PR creation should handle gracefully
    (should-error (claude--create-pr "test-repo" "test-branch"))))
```

### Repo Collision

```elisp
(ert-deftest claude-test-repo-name-collision ()
  (let* ((temp-dir (make-temp-file "claude-test" t))
         (claude-metadata-dir temp-dir))
    (unwind-protect
        (progn
          ;; Create workspace for repo A
          (claude-metadata-write "api" "feature"
                                 (list :parent_repo "/code/company-a/api"))

          ;; Attempting to create for repo B with same name should error
          (should-error
           (claude--check-repo-collision "api" "feature" "/code/company-b/api")
           :type 'user-error))
      (delete-directory temp-dir t))))
```

### Performance (Manual)

For large-scale testing, run manually:

```elisp
(defun claude-test-many-workspaces (n)
  "Create N workspaces and measure performance."
  (let ((start-time (current-time)))
    (dotimes (i n)
      (claude-metadata-write "perf-repo" (format "branch-%d" i)
                             (list :version 1 :status "active")))

    ;; Time metadata listing
    (let ((list-start (current-time)))
      (claude-worktree-list)
      (message "List %d workspaces: %.3fs"
               n (float-time (time-subtract nil list-start))))

    ;; Time reconciliation
    (let ((reconcile-start (current-time)))
      (claude--startup-reconcile)
      (message "Reconcile %d workspaces: %.3fs"
               n (float-time (time-subtract nil reconcile-start))))

    ;; Cleanup
    (dotimes (i n)
      (claude-metadata-delete "perf-repo" (format "branch-%d" i)))

    (message "Total time for %d workspaces: %.3fs"
             n (float-time (time-subtract nil start-time)))))

;; Target: 100 workspaces should complete in <5s
```

## Debugging Support

Add test utilities for debugging:

```elisp
(defun claude-debug-dump-state ()
  "Dump all Claude state for debugging."
  (interactive)
  (with-current-buffer (get-buffer-create "*claude-debug*")
    (erase-buffer)
    (insert "=== Claude Debug State ===\n\n")

    (insert "== Metadata Files ==\n")
    (dolist (ws (claude-worktree-list))
      (let ((metadata (claude-metadata-read (car ws) (cdr ws))))
        (insert (format "\n%s:%s\n" (car ws) (cdr ws)))
        (insert (format "  %S\n" metadata))))

    (insert "\n== Doom Workspaces ==\n")
    (dolist (ws (+workspace-list-names))
      (insert (format "  %s\n" ws)))

    (insert "\n== Claude Buffers ==\n")
    (dolist (buf (buffer-list))
      (when (string-prefix-p "*claude:" (buffer-name buf))
        (insert (format "  %s\n" (buffer-name buf)))))

    (insert "\n== Terminal Buffers ==\n")
    (dolist (buf (buffer-list))
      (when (string-prefix-p "*term:" (buffer-name buf))
        (insert (format "  %s\n" (buffer-name buf)))))

    (insert "\n== Attention State ==\n")
    (maphash (lambda (k v)
               (insert (format "  %s: %s\n" k v)))
             claude-monitor--attention-state)

    (pop-to-buffer (current-buffer))))
```
