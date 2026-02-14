# Grove Workspace Integration: Global Keybindings

Last updated: 2026-02-14

## Global Keybinding

One binding to rule them all:

| Key | Action | Description |
|-----|--------|-------------|
| `SPC =` | `claude-goto-main` | Jump to main workspace, focus dashboard |

`SPC =` is the only global keybinding. It:
1. Switches to the main Doom workspace (`claude:main`)
2. If dashboard buffer doesn't exist, creates it
3. If dashboard buffer exists, switches to it
4. Triggers a refresh

Everything else happens through buffer-local keybindings once you're in the dashboard.

### Removed / Changed

The current `SPC C` map has workspace-specific bindings (create, close, home, etc.). These move into the dashboard as contextual actions. The entire `SPC C` prefix is eliminated.

| Current | Replacement |
|---------|-------------|
| `SPC C h` (create home workspace) | Dashboard: `RET` on repo |
| `SPC C c` (create worktree) | Dashboard: `c` on repo |
| `SPC C k` (close workspace) | Dashboard: `x` on worktree |
| `SPC C d` (open dashboard) | `SPC =` |
| `SPC C m` (go to main) | `SPC =` |

### Why `SPC =`

- `SPC ;` conflicts with Doom's `eval-expression`
- `=` is unbound in Doom's default `SPC` map
- One key to remember: "go to dashboard"
- All workspace operations are contextual within the dashboard (buffer-local keys)
