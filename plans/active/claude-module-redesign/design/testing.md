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
  (member (alist-get 'worktree_path metadata) claude-test--mock-worktrees))

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
        (metadata '((status . "active")
                    (worktree_path . "/tmp/worktrees/repo/branch")
                    (repo_name . "repo")
                    (branch_name . "branch"))))
    (cl-letf (((symbol-function 'claude--check-worktree) #'claude-test--mock-check-worktree)
              ((symbol-function 'claude--check-doom-workspace) #'claude-test--mock-check-doom)
              ((symbol-function 'claude--check-vterm-buffer) #'claude-test--mock-check-vterm))
      (should (eq (claude--reconcile metadata) 'active)))))

(ert-deftest claude-reconcile-test-missing-worktree ()
  (let ((claude-test--mock-worktrees nil)  ; Worktree missing
        (claude-test--mock-workspaces '("repo:branch"))
        (claude-test--mock-buffers '("*claude:repo:branch*"))
        (metadata '((status . "active")
                    (worktree_path . "/tmp/worktrees/repo/branch")
                    (repo_name . "repo")
                    (branch_name . "branch"))))
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
        (metadata '((status . "active")
                    (worktree_path . "/tmp/worktrees/repo/branch")
                    (repo_name . "repo")
                    (branch_name . "branch"))))
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
    (should (equal (alist-get 'status (claude-metadata-read "test-repo" "test-branch"))
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
  (should (equal (alist-get 'status (claude-metadata-read "test-repo" "test-branch"))
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
