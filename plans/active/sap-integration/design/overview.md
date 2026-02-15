# sap Integration — Emacs Dashboard

Last updated: 2026-02-14

## What

Rewrite `claude-monitor.el` to consume session state from sap (the Session Awareness Protocol CLI) instead of scraping vterm buffer output. Update `claude-dashboard.el` to leverage richer session data including resume capability.

## Dependency

Requires **sap** (`@twiglylabs/sap`) to be installed and configured with Claude Code hooks. See the sap project at `~/repos/twiglylabs/sap` for the CLI design and hooks configuration.

## Why

The current monitor (`claude-monitor.el`) works by:
1. Hashing the last 15 lines of vterm buffer output every 2 seconds
2. Pattern-matching against regex attention patterns
3. Deriving a three-state model: `working`, `idle`, `nil`

This is unreliable — false positives from regex matching, no session lifecycle awareness, no resume capability, no knowledge of what Claude is actually doing.

With sap, Claude Code hooks emit structured events that sap persists in SQLite. The monitor becomes a thin polling layer that calls `sap status --json` and parses the result.

## What Changes

| Component | Before | After |
|-----------|--------|-------|
| `claude-monitor.el` | vterm buffer scraping, regex patterns | sap CLI polling, JSON parsing |
| `claude-dashboard.el` status | working/idle/nil | active/attention/stopped + last tool |
| `claude-dashboard.el` actions | Open, Jump, Close | + Resume (for stopped sessions) |
| Session awareness | None | Duration, session history, resume |
| Attention detection | Regex on 15 lines | Exact: permission_prompt, idle_prompt events |

## Related Documents

- [approach.md](./approach.md) — implementation details, new API surface
- sap project: `~/repos/twiglylabs/sap/plans/active/sap-core/design/`
