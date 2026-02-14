# Grove Workspace Integration: Global Keybindings

Last updated: 2026-02-14

## Global Keybindings (available everywhere)

Under `SPC C` prefix:

| Key | Action | Description |
|-----|--------|-------------|
| `SPC C m` | `claude-goto-main` | Jump to main workspace, focus dashboard |
| `SPC C d` | `claude-dashboard-open` | Open dashboard in current workspace (fallback) |

`SPC C m` is the primary navigation key. It:
1. Switches to the main Doom workspace
2. If dashboard buffer doesn't exist, creates it
3. If dashboard buffer exists, switches to it
4. Triggers a refresh

### Removed / Changed

The current `SPC C` map has workspace-specific bindings (create, close, home, etc.). These move into the dashboard as contextual actions rather than global keybindings. Simplifies the keymap significantly.

| Current | Replacement |
|---------|-------------|
| `SPC C h` (create home workspace) | Dashboard: `RET` on repo |
| `SPC C c` (create worktree) | Dashboard: `c` on repo |
| `SPC C k` (close workspace) | Dashboard: `x` on worktree |
| `SPC C d` (open dashboard) | `SPC C m` (go to main) |

The dashboard becomes the single entry point for all workspace operations. Fewer global keybindings to remember.

## Workspace-Local Keybindings

Inside a worktree or home workspace, minimal keys:

| Key | Action | Description |
|-----|--------|-------------|
| `SPC C m` | `claude-goto-main` | Back to main/dashboard |
| `SPC C t` | `claude-open-terminal` | Open a terminal in this workspace |

Everything else is done from the dashboard.
