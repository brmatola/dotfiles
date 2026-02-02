# Skill: preparing-branch-for-integration

Last updated: 2026-02-01

## Purpose

Get a feature branch ready for integration. Claude handles the prep work (fetching, rebasing, conflict resolution, testing), then hands off to the user for execution via Emacs.

## Scope

**Skill does:**
- Fetch latest parent branch
- Merge or rebase parent into feature branch
- Help resolve conflicts interactively
- Run tests to verify clean state
- Report readiness

**Skill does NOT:**
- Present merge/PR/discard options
- Execute the final integration
- Touch Emacs workspaces or metadata
- Delete branches or worktrees

## The Flow

```
1. Check current branch and parent
2. Fetch origin
3. Merge parent into current branch
   - If conflict → help resolve
   - If clean → continue
4. Run test suite
   - If fail → help fix
   - If pass → continue
5. Report: "Branch ready. Use SPC C x to merge, create PR, or cleanup."
```

## Skill File

Location: `dotfiles/claude/skills/preparing-branch-for-integration/SKILL.md`

```markdown
---
name: preparing-branch-for-integration
description: Use when ready to integrate a feature branch - fetches latest, rebases, resolves conflicts, runs tests, then hands off to user for merge/PR decision
---

# Preparing Branch for Integration

## Overview

Get a feature branch ready for integration. You handle the prep, user executes.

**Announce at start:** "I'm preparing this branch for integration."

## The Process

### Step 1: Identify Context

\`\`\`bash
git branch --show-current
git rev-parse --abbrev-ref @{upstream} 2>/dev/null || echo "no upstream"
\`\`\`

Determine parent branch (usually main/master, or from worktree metadata).

### Step 2: Fetch Latest

\`\`\`bash
git fetch origin
\`\`\`

### Step 3: Merge Parent

\`\`\`bash
git merge origin/<parent-branch>
\`\`\`

**If conflicts:**
- Show conflicted files
- Help resolve each one
- Run `git add` after each resolution
- Complete merge with `git commit`

**If clean:** Continue.

### Step 4: Run Tests

\`\`\`bash
# Detect and run appropriate test command
npm test || cargo test || pytest || go test ./...
\`\`\`

**If tests fail:**
- Show failures
- Help fix
- Re-run until green

### Step 5: Hand Off

Report:
\`\`\`
Branch is ready for integration:
- Parent: <parent-branch>
- Commits ahead: <N>
- Tests: passing

Use SPC C x to:
- [m] Merge & cleanup
- [p] Push & create PR
- [k] Keep working
- [d] Discard
\`\`\`

## Do NOT

- Execute merge into parent
- Create PRs
- Delete branches
- Modify Emacs workspaces

The user handles execution via Emacs UI.
```

## Integration with Emacs

The skill can detect Claude-managed workspaces by checking for metadata:

```bash
# Check if in Claude workspace
REPO=$(basename $(git rev-parse --show-toplevel))
BRANCH=$(git branch --show-current)
if [[ -f "$HOME/worktrees/metadata/$REPO/$BRANCH.json" ]]; then
  echo "Claude workspace detected"
fi
```

When detected, the handoff message explicitly mentions `SPC C x`.
