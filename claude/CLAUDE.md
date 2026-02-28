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

## Tooling Stack

The user's development tools (all TypeScript, all in `~/repos/twiglylabs/tooling/`):

| Tool | Purpose | CLI |
|------|---------|-----|
| **Canopy** | Electron desktop UI — mission control for workspaces | (GUI app) |
| **Grove** | Config-driven local K8s dev environments | `grove` |
| **Trellis** | Plan management with dependency DAG | `trellis` |
| **SAP** | Session analytics for Claude Code | `sap` |
| **Bark** | Quality gate artifact validation | `bark` |

### Trellis (Plan Tracking)

If the repo has a `.trellis` config, use trellis to check and update plan status:

- `trellis status` — see all plans and their state
- `trellis ready` — what's ready to work on
- `trellis update <id> <status>` — mark progress (in_progress, done)
- `trellis show <id>` — see a plan's details and dependencies
- `trellis graph` — open DAG viewer in browser

Plans live in `plans/active/` at the workspace root.

### SAP (Session Analytics)

SAP hooks are configured to record all Claude Code events. Use `sap-review` skill for analysis.

---

## Workspace Context

You run inside workspaces managed by the user. Don't create, merge, or close
workspaces — focus on the work.

### Branch Safety

- Don't push, merge, or delete branches — the user handles this
- Commit freely on the current branch
- If you need to sync with upstream, ask first
