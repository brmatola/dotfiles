# Global Claude Instructions

## Principles

1. **Verify before claiming** - Run commands, see output, then claim results
2. **Test-first** - Write failing test before implementation
3. **Understand before fixing** - Find root cause, don't guess

## Skills Available

Skills in `~/.claude/skills/` are loaded automatically when relevant:

- `test-driven-development` - TDD workflow (red-green-refactor)
- `systematic-debugging` - Four-phase debugging framework
- `verification-before-completion` - Evidence before assertions

## Agents Available

Use these via the Task tool for delegation:

- `haiku-general-purpose` - Fast, cheap tasks (research, summarization)
- `sonnet-general-purpose` - Balanced tasks (analysis, implementation)
- `opus-general-purpose` - Complex reasoning tasks

---

## Worktree Development

Sessions run in either the **main repository** or an **isolated worktree**.

### Detecting Your Environment

Check for Emacs-managed worktree metadata:

```bash
REPO=$(basename "$(git rev-parse --show-toplevel)")
BRANCH=$(git branch --show-current)
METADATA="$HOME/worktrees/metadata/$REPO/$BRANCH.json"

if [[ -f "$METADATA" ]]; then
  echo "In Emacs-managed worktree"
else
  echo "In main repository or standalone worktree"
fi
```

### Main Repository (Home)

- Used for planning, coordination, running dev servers
- Safe to push to remote, create PRs
- No automatic cleanup—changes persist

### Emacs-Managed Worktree

- Isolated branch for focused work
- Located at `~/worktrees/{repo}/{branch}/`
- Metadata at `~/worktrees/metadata/{repo}/{branch}.json`
- **Do not merge or push directly**—report readiness and let the user handle integration via Emacs
- Worktree will be cleaned up when user closes the workspace

### Metadata Schema

```json
{
  "version": 1,
  "status": "active",
  "parent_branch": "main",
  "worktree_path": "~/worktrees/repo/feature",
  "created_at": "2026-02-01T10:00:00Z",
  "workflow": {
    "plan": "plan-name",
    "phase": "implement",
    "started": "2026-02-01T10:00:00Z"
  }
}
```

The `workflow` key is optional—present when running a structured workflow.
