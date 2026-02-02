# Emacs Changes

Last updated: 2026-02-01

## claude-cleanup.el

### New Status Buffer Layout

```
Claude Workspace Cleanup

Workspace: dotfiles:feature-auth
Parent: main
Status: 3 commits ahead
Uncommitted: No

[m] Merge & cleanup
[p] Push & create PR
[k] Keep as-is
[d] Discard (lose changes)
[v] View diff in magit

[q] Cancel
```

### New Keybindings

| Key | Current | New |
|-----|---------|-----|
| `m` | Merge & cleanup | Merge & cleanup (unchanged) |
| `p` | - | Push & create PR (NEW) |
| `d` | Delete | Discard (renamed for clarity) |
| `k` | - | Keep as-is (NEW, replaces `c` cancel) |
| `v` | View diff | View diff (unchanged) |
| `q` | - | Cancel (NEW, consistent with other modes) |

### Defensive Git Checks

Before showing options, detect state:

```elisp
(defun claude-cleanup--detect-state (worktree-path branch parent-branch)
  "Detect branch state for cleanup options."
  (list
   :already-merged (claude-git-branch-merged-p worktree-path branch parent-branch)
   :branch-exists (claude-git-branch-exists-p worktree-path branch)
   :commits-ahead (claude-git-commits-ahead worktree-path parent-branch)
   :uncommitted (claude-git-has-uncommitted-changes worktree-path)))
```

Adjust displayed options based on state:
- Already merged → hide `[m]`, show "Already merged, clean up?"
- Branch gone → hide `[m]` and `[p]`, show cleanup only

### PR Creation

```elisp
(defun claude-cleanup-push-and-pr ()
  "Push branch and create PR."
  (interactive)
  (let* ((info claude-cleanup--workspace-info)
         (branch (plist-get info :branch-name))
         (parent (plist-get info :parent-branch)))
    ;; Push with upstream
    (claude-git-push-with-upstream (plist-get info :worktree-path) branch)
    ;; Create PR
    (let ((pr-url (claude-git-create-pr (plist-get info :parent-repo) branch parent)))
      ;; Store in metadata
      (claude-metadata-update (plist-get info :repo-name) branch :pr_url pr-url)
      (message "PR created: %s" pr-url)
      (kill-new pr-url))))
```

## claude-workspace.el

### Treemacs Refresh on Switch

```elisp
(defun claude-workspace-switch (workspace-name)
  "Switch to WORKSPACE-NAME and focus its Claude buffer."
  (+workspace/switch-to workspace-name)
  (let ((parsed (claude--parse-workspace-name workspace-name)))
    (when parsed
      (let* ((workspace-path (claude-workspace-path workspace-name))
             (buffer-name (claude--buffer-name (car parsed) (cdr parsed)))
             (buffer (get-buffer buffer-name)))
        ;; Refresh treemacs for new project
        (when (and workspace-path (fboundp 'treemacs-add-and-display-current-project-exclusively))
          (let ((default-directory workspace-path))
            (treemacs-add-and-display-current-project-exclusively)))
        ;; Focus Claude buffer
        (when (and buffer (buffer-live-p buffer))
          (switch-to-buffer buffer)
          (when (eq major-mode 'vterm-mode)
            (goto-char (point-max))
            (vterm-reset-cursor-point)))))))
```

## claude-worktree.el

### New Git Helper Functions

```elisp
(defun claude-git-branch-merged-p (repo-path branch parent)
  "Return t if BRANCH is fully merged into PARENT."
  ...)

(defun claude-git-branch-exists-p (repo-path branch)
  "Return t if BRANCH exists locally."
  ...)

(defun claude-git-push-with-upstream (repo-path branch)
  "Push BRANCH with -u to set upstream."
  ...)

(defun claude-git-create-pr (repo-path branch parent)
  "Create PR for BRANCH targeting PARENT. Returns PR URL."
  ...)
```

## install.sh

Add symlink for skills:

```bash
# Claude skills
ln -sf "$DOTFILES/claude/skills" "$HOME/.claude/skills"
```
