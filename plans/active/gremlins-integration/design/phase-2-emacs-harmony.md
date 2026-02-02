# Phase 2: Emacs Harmony

Last updated: 2026-02-01

## Goal

Adjust gremlins skills to work with Emacs module conventions. Emacs owns worktree creation and final merge/PR execution. Claude owns planning and implementation.

## Division of Labor

| Phase | Owner | How |
|-------|-------|-----|
| Worktree creation | Emacs | `SPC C c` |
| Planning | Claude | brainstorming, writing-plans |
| Implementation | Claude | executing-plans, worktree-workflow |
| Branch prep | Claude | finishing-a-development-branch |
| Final execution | Emacs | `SPC C x` (merge/PR/discard) |

## Current Conflicts

| Aspect | Gremlins | Emacs | Resolution |
|--------|----------|-------|------------|
| Worktree location | `.worktrees/` or global | `~/worktrees/{repo}/{branch}/` | Use Emacs location |
| Workflow state | `.workflow-state.json` in repo | `~/worktrees/metadata/{repo}/{branch}.json` | Add `workflow` key to Emacs metadata |
| Branch finishing | Claude executes merge/PR | Emacs executes via `SPC C x` | Claude preps, Emacs executes |

## Tasks

### 1. Update `using-git-worktrees` Skill

Add detection for Emacs-managed worktrees. If already in one, skip creation.

**Add to directory selection:**

```markdown
### 0. Check for Emacs-Managed Worktree (Before Other Checks)

\`\`\`bash
REPO=$(basename $(git rev-parse --show-toplevel 2>/dev/null || echo "unknown"))
BRANCH=$(git branch --show-current)
METADATA="$HOME/worktrees/metadata/$REPO/$BRANCH.json"

if [[ -f "$METADATA" ]]; then
  echo "Emacs-managed worktree detected. Skipping creation."
  # Already isolated, proceed with work
fi
\`\`\`

If NOT in Emacs worktree, continue with existing directory selection logic.
```

### 2. Update `worktree-workflow` State Location

Change from `.workflow-state.json` to Emacs metadata.

**State location logic:**

```markdown
### State File Location

Check for Emacs metadata first:

\`\`\`bash
REPO=$(basename $(git rev-parse --show-toplevel))
BRANCH=$(git branch --show-current)
EMACS_METADATA="$HOME/worktrees/metadata/$REPO/$BRANCH.json"

if [[ -f "$EMACS_METADATA" ]]; then
  # Read/write workflow state as nested key in Emacs metadata
  # Use jq to read: jq '.workflow' "$EMACS_METADATA"
  # Use jq to write: jq '.workflow = {...}' "$EMACS_METADATA"
else
  # Fallback to .workflow-state.json for non-Emacs sessions
fi
\`\`\`
```

**Workflow key schema (nested in Emacs metadata):**

```json
{
  "workflow": {
    "plan": "plan-name",
    "phase": "review | implement | audit | reach | complete",
    "started": "2026-02-01T10:00:00Z",
    "history": [...]
  }
}
```

**Ownership:** Claude writes, Emacs reads (for dashboard display).

### 3. Update `finishing-a-development-branch` Skill

Replace the execution steps with a handoff message.

**Change Step 4 (Execute Choice) to:**

```markdown
### Step 4: Report Readiness and Hand Off

After verifying tests pass and determining base branch:

\`\`\`
Branch ready for integration.

Status:
- Tests: passing
- Base branch: {base-branch}
- Commits ahead: {N}
- Conflicts: none detected

Next step: Use SPC C x in Emacs to choose:
- [m] Merge & cleanup
- [p] Push & create PR
- [d] Discard
- [c] Cancel

I've prepared the branch. Emacs will handle the final action.
\`\`\`

**Do NOT execute merge, push, or delete yourself.** The user triggers final action via Emacs keybinding.
```

**Remove:** Steps for actually executing merge/push/delete. Keep only the prep work (verify tests, fetch, check for conflicts).

### 4. Update Emacs `claude-cleanup.el`

Add PR option and workflow context display.

**Changes:**
1. Add `[p]` Push & PR option (calls `gh pr create`)
2. Read workflow phase from metadata and display it
3. Add defensive checks (already merged → skip merge, branch deleted → skip deletion)

**New cleanup buffer format:**

```
Claude Workspace Cleanup

Workspace: dotfiles:feature-auth
Parent: main
Workflow: gremlins-integration (phase: audit)
Status: 3 commits ahead

[m] Merge & cleanup
[p] Push & create PR
[d] Discard
[c] Cancel
```

### 5. Update Emacs `claude-dashboard.el`

Add workflow phase column.

**New format:**

```
  REPO          BRANCH              PHASE        STATUS
  dotfiles      feature-auth        implement    ● needs input
  my-app        fix-login           audit        ○ running
  ⌂ dotfiles    (home)              -            ○ running
```

**Phase values:** `plan`, `implement`, `audit`, `reach`, `complete`, or `-` if no workflow active.

### 6. Update `claude-state.el`

Add helper to read workflow phase from metadata.

```elisp
(defun claude--workflow-phase (repo-name branch-name)
  "Get workflow phase for REPO-NAME/BRANCH-NAME, or nil."
  (let ((metadata (claude-metadata-read repo-name branch-name)))
    (when-let ((workflow (plist-get metadata :workflow)))
      (plist-get workflow :phase))))
```

### 7. Add Elisp Tests

Add test coverage for:
- `claude-cleanup.el`: PR option, workflow context display
- `claude-dashboard.el`: Phase column rendering
- `claude-state.el`: `claude--workflow-phase` helper

```bash
./emacs/doom/modules/claude/test/run-tests.sh
./emacs/doom/modules/claude/test/lint.sh
```

## Verification

**Skill changes:**
1. Start Emacs, `SPC C c` to create worktree
2. In Claude session, invoke `using-git-worktrees` - should detect existing worktree
3. Run `/worktree-workflow start some-plan` - state should go to Emacs metadata
4. Complete implementation, run audit
5. `finishing-a-development-branch` should report readiness, NOT execute merge
6. `SPC C x` should show workflow context and offer merge/PR/discard

**Elisp changes:**
1. Tests pass
2. Dashboard shows phase column
3. Cleanup buffer shows PR option and workflow info

## Notes

- Claude never executes final merge/PR/delete in Emacs-managed worktrees
- Workflow state in Emacs metadata enables dashboard to show progress
- Non-Emacs sessions (terminal Claude) still work via `.workflow-state.json` fallback
