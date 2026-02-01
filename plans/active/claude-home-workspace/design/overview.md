# Claude Home Workspace

Created: 2026-02-01

## Problem

The current multi-Claude workflow only supports worktree-based workspaces. Users need a "home base" workspace in the main repo for:
- Planning and coordination
- Pushing updates to main branch
- Running dev servers and experiments
- Managing worktree branches

## Solution

Add a "home workspace" concept - a Claude session that runs in the main repository (not a worktree). One home per repo, accessible from anywhere with `SPC C h`.

## Key Features

1. **`SPC C h`** - Jump to home workspace (create if needed)
2. **`SPC C t`** - Spawn extra terminals in any workspace
3. **Home-aware cleanup** - Warn on uncommitted changes, no merge flow
4. **Dashboard integration** - Show home workspaces with `⌂` indicator

## Design Documents

- [Commands](./commands.md) - Keybindings and command behavior
- [Implementation](./implementation.md) - File changes and new functions
