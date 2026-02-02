# Phase 2 Implementation Tasks

## Prerequisites
- Phase 1 complete (skills in dotfiles, symlinks working)
- Emacs module functional (`SPC C c`, `SPC C x`, dashboard work)

## Part A: Skill Modifications

### Task A1: Update `using-git-worktrees/SKILL.md`

Add new section **before** "Directory Selection Process":

```markdown
## Emacs Worktree Detection

Before any directory selection, check if already in an Emacs-managed worktree:

\`\`\`bash
REPO=$(basename "$(git rev-parse --show-toplevel 2>/dev/null || echo unknown)")
BRANCH=$(git branch --show-current)
METADATA="$HOME/worktrees/metadata/$REPO/$BRANCH.json"

if [[ -f "$METADATA" ]]; then
  echo "Already in Emacs-managed worktree at $(pwd)"
  echo "Skipping worktree creation."
  # Proceed directly to project setup and baseline verification
fi
\`\`\`

If metadata exists, skip to "Run Project Setup" section.
```

### Task A2: Update `worktree-workflow/SKILL.md`

Replace "State File Location" section with:

```markdown
### State File Location

Check for Emacs metadata first:

\`\`\`bash
REPO=$(basename "$(git rev-parse --show-toplevel)")
BRANCH=$(git branch --show-current)
EMACS_METADATA="$HOME/worktrees/metadata/$REPO/$BRANCH.json"

if [[ -f "$EMACS_METADATA" ]]; then
  # Use Emacs metadata - read/write workflow as nested key
  # Read:  jq -r '.workflow // empty' "$EMACS_METADATA"
  # Write: Use Read tool, modify plist, use Write tool
  STATE_LOCATION="emacs"
else
  # Fallback for non-Emacs sessions
  STATE_LOCATION=".workflow-state.json"
fi
\`\`\`

**Emacs metadata workflow key:**
\`\`\`json
{
  "workflow": {
    "plan": "plan-name",
    "phase": "review",
    "started": "2026-02-01T10:00:00Z",
    "history": []
  }
}
\`\`\`

When writing to Emacs metadata, merge the workflow key into existing JSON (don't overwrite other fields).
```

### Task A3: Update `finishing-a-development-branch/SKILL.md`

Replace "Step 4: Execute Choice" with:

```markdown
### Step 4: Check Environment and Report

**If in Emacs-managed worktree** (metadata file exists):

\`\`\`
Branch ready for integration.

Tests: passing
Base: {base-branch}
Ahead: {N} commits
Conflicts: none detected

Use SPC C x in Emacs to complete:
- [m] Merge & cleanup
- [p] Push & create PR
- [d] Discard

Branch is prepared. Awaiting your choice in Emacs.
\`\`\`

**Stop here.** Do not execute merge/push/delete.

**If NOT in Emacs worktree** (no metadata file):

Present the 4 options and execute as before (existing behavior).
```

Remove or gate the existing execution code behind the non-Emacs check.

### Task A4: Verify skill changes

```bash
# Start fresh Claude in an Emacs worktree
cd ~/worktrees/some-repo/some-branch
claude

# Test detection
# Invoke: using-git-worktrees
# Expected: "Already in Emacs-managed worktree"

# Test workflow state location
# Invoke: /worktree-workflow status
# Check: reads from ~/worktrees/metadata/...
```

## Part B: Elisp Modifications

### Task B1: Add workflow phase helper to `claude-state.el`

Add before `(provide 'claude-state)`:

```elisp
(defun claude--workflow-phase (repo-name branch-name)
  "Get workflow phase for REPO-NAME/BRANCH-NAME, or nil if no workflow."
  (when-let* ((metadata (claude-metadata-read repo-name branch-name))
              (workflow (plist-get metadata :workflow)))
    (plist-get workflow :phase)))
```

### Task B2: Update `claude-dashboard.el` to show phase

In `claude-dashboard--get-workspaces`, add phase to result:

```elisp
;; After (is-home (claude--home-workspace-p metadata))
(phase (claude--workflow-phase repo-name branch-name))

;; Add to push result:
:phase phase
```

In `claude-dashboard--render`, add phase display:

```elisp
;; After display-name calculation
(phase-str (or (plist-get ws :phase) "-"))

;; In the insert, add phase
(format "%-12s " phase-str)
```

### Task B3: Update `claude-cleanup.el` with PR option and workflow display

Add `[p]` binding in `claude-cleanup-mode-map`:
```elisp
(define-key map "p" #'claude-cleanup-push-pr)
```

Add function:
```elisp
(defun claude-cleanup-push-pr ()
  "Push branch and create PR."
  (interactive)
  (when-let* ((info claude-cleanup--workspace-info)
              (branch-name (plist-get info :branch-name))
              (worktree-path (plist-get info :worktree-path)))
    (let ((default-directory worktree-path))
      ;; Push
      (shell-command (format "git push -u origin %s" branch-name))
      ;; Create PR
      (shell-command "gh pr create --fill")
      (message "PR created for %s" branch-name)
      ;; Keep worktree (PR workflow needs it)
      (quit-window t))))
```

In `claude-cleanup--show-status`, add workflow phase to display:
```elisp
;; Read workflow phase
(workflow-phase (claude--workflow-phase repo-name branch-name))

;; Add to buffer output (after Parent line)
(when workflow-phase
  (format "Workflow: %s (phase: %s)\n"
          (or (plist-get metadata :workflow-plan) "unknown")
          workflow-phase))

;; Add [p] option line
(propertize "[p]" 'face 'font-lock-keyword-face)
" Push & create PR\n"
```

### Task B4: Add evil binding for `p`

In the evil setup block:
```elisp
(evil-local-set-key 'normal (kbd "p") #'claude-cleanup-push-pr)
```

### Task B5: Add tests

Create `test/claude-cleanup-test.el` with:
- Test PR option exists in keymap
- Test workflow phase display logic

Add to `test/claude-state-test.el`:
- Test `claude--workflow-phase` returns nil when no workflow
- Test `claude--workflow-phase` returns phase when present

### Task B6: Run tests and lint

```bash
./emacs/doom/modules/claude/test/run-tests.sh
./emacs/doom/modules/claude/test/lint.sh
```

### Task B7: Integration test

1. `SPC C c` - create worktree workspace
2. In Claude: `/worktree-workflow start test-plan`
3. Check metadata file has `workflow` key
4. Dashboard shows phase
5. Run through to finish
6. `SPC C x` shows workflow context
7. `[p]` creates PR

### Task B8: Commit

```bash
git add claude/skills emacs/doom/modules/claude
git commit -m "feat: integrate gremlins with Emacs module

- using-git-worktrees detects Emacs-managed worktrees
- worktree-workflow writes state to Emacs metadata
- finishing-a-development-branch hands off to Emacs
- Dashboard shows workflow phase
- Cleanup buffer has PR option"
```
