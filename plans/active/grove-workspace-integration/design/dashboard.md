# Grove Workspace Integration: Dashboard Design

Last updated: 2026-02-14

## Dashboard Layout

The dashboard is a full-window buffer in the main workspace. It's the primary UI for all workspace orchestration.

### Visual Design

```

   Claude Workspaces

   dotfiles                                       Open
   ├  fix-zsh-path        3 commits    waiting    Jump
   └  add-brew-packages   1 commit    ⚡ working    Jump
                                          + New Worktree

   acorn                                          Open
   ├  feature-auth        5 commits    waiting    Jump
   │    acorn             2 commits   ✓ clean
   │    public            3 commits    dirty (2)
   │    cloud             0 commits   ✓ clean
   └  data-model-v2       8 commits   ⚡ working    Jump
   │    acorn             1 commit    ✓ clean
   │    public            5 commits   ✓ clean
   │    cloud             2 commits   ✓ clean
                                          + New Worktree

   trellis                                        Open
   (no active workspaces)
                                          + New Worktree

  ─────────────────────────────────────────────────────────
   c new   s sync   x close   RET jump   ? help
```

### Visual Elements

**Repo headers** — Large text, subtle background band (like magit section headers). Nerd-font icon  for git. Repo name in bold. `[Open]` button on the right — box face with background.

**Worktree entries** — Tree lines using `├ └ │` in a dim face. Branch icon  before branch name. Commit count right-aligned. Status indicator with colored dot:
- `⚡ working` — green face, dim (all good, no action needed)
- ` waiting` — amber/yellow face, bold (needs attention — Claude stopped)
- ` error` — red face, bold (something broke)

**Grouped workspace sub-repos** — Extra indentation under the worktree entry. Dimmer than the worktree line. Shows per-repo status:
- `✓ clean` — very dim green (fine, no action)
- ` dirty (N)` — yellow (has uncommitted changes)

**Buttons** — Text with box face (border + subtle background). Clickable via mouse or RET when point is on them. Show keyboard shortcut in parentheses when focused.

**Footer hint bar** — Dim face, always visible. Shows available keybindings. Updates contextually (different hints when on a repo vs worktree).

**Attention sorting** — Within each repo group, worktrees needing attention (idle/error) sort above working ones.

### Faces (Custom)

```elisp
;; Repo headers
claude-dashboard-repo-face          ; bold, height 1.1, background band
claude-dashboard-repo-dim-face      ; for repos with no workspaces

;; Worktree entries
claude-dashboard-branch-face        ; default weight
claude-dashboard-commits-face       ; dim, right-aligned

;; Status indicators
claude-dashboard-working-face       ; green foreground
claude-dashboard-waiting-face       ; amber foreground, bold
claude-dashboard-error-face         ; red foreground, bold

;; Sub-repo details
claude-dashboard-subrepo-face       ; dim
claude-dashboard-clean-face         ; very dim green
claude-dashboard-dirty-face         ; yellow

;; Buttons
claude-dashboard-button-face        ; box border, subtle background
claude-dashboard-button-hover-face  ; highlighted on focus

;; Structure
claude-dashboard-tree-face          ; dim, for │ ├ └ characters
claude-dashboard-footer-face        ; dim, for hint bar
claude-dashboard-title-face         ; bold, height 1.3
```

All faces inherit from appropriate doom-themes base faces for theme compatibility.

### Icons (nerd-icons)

```elisp
(nerd-icons-devicon  "nf-dev-git_branch")   ;  branch
(nerd-icons-octicon  "nf-oct-repo")          ;  repo
(nerd-icons-faicon   "nf-fa-circle")         ;  status dot (colored per state)
(nerd-icons-codicon  "nf-cod-check")         ;  clean
(nerd-icons-codicon  "nf-cod-warning")       ;  dirty
(nerd-icons-codicon  "nf-cod-add")           ;  new worktree
```

## Interaction

### Keybindings (dashboard buffer local)

| Key | Context | Action |
|-----|---------|--------|
| `RET` | repo header | Open/jump to home workspace |
| `RET` | worktree entry | Jump to worktree workspace |
| `RET` | any button | Activate the button |
| `c` | repo section | Create new worktree (prompts for branch) |
| `s` | worktree entry | Sync workspace (`grove workspace sync`) |
| `x` | worktree entry | Close workspace (prompts merge/discard) |
| `a` | anywhere | Add repo (`grove repo add`, prompts for path) |
| `n` / `p` | anywhere | Next/previous entry (skip decorative lines) |
| `TAB` | repo header | Collapse/expand repo section |
| `q` | anywhere | Bury dashboard buffer |
| `?` | anywhere | Show help popup (transient or which-key) |

### Mouse Support

- Click on any button to activate
- Click on repo header to jump to home workspace
- Click on worktree to jump to it

### Navigation Flow

```
SPC C m  →  Main workspace (dashboard auto-focused)
             │
             ├── RET on repo → Doom workspace for that repo (home)
             │                  └── SPC C m → back to main
             │
             ├── RET on worktree → Doom workspace for that worktree
             │                      └── SPC C m → back to main
             │
             ├── c on repo → minibuffer prompt for branch name
             │                → grove workspace create (async)
             │                → dashboard refreshes, new entry appears
             │
             ├── s on worktree → grove workspace sync (async)
             │                   → status updates in dashboard
             │
             └── x on worktree → prompt: merge or discard?
                                 → grove workspace close (async)
                                 → entry removed from dashboard
```

## Auto-Refresh

**Timer-based polling** while dashboard buffer is visible:
- Poll interval: 5 seconds (configurable via `claude-dashboard-refresh-interval`)
- Only runs when dashboard buffer is in a visible window
- Calls `grove repo list --json` and `claude-workspace-attention` per workspace
- Preserves cursor position across refreshes (by remembering which entry point is on)
- Timer stops when dashboard buffer is killed or buried

**Event-driven refresh** for immediate updates:
- After any grove command completes (create, sync, close)
- After attention state changes (via `claude-attention-change-hook`)

## Buffer Construction

The dashboard is a read-only special-mode buffer (`claude-dashboard-mode`), not derived from tabulated-list-mode. Custom rendering gives full control over layout.

### Rendering Pipeline

```elisp
(defun claude-dashboard-render ()
  "Render the dashboard buffer from grove data."
  ;; 1. Call grove
  (let* ((grove-data (claude-grove-repo-list))
         (attention  (claude-dashboard--collect-attention))
         (inhibit-read-only t))
    ;; 2. Clear and rebuild
    (erase-buffer)
    ;; 3. Title
    (claude-dashboard--insert-title)
    ;; 4. Each repo section
    (dolist (repo grove-data)
      (claude-dashboard--insert-repo-section repo attention))
    ;; 5. Footer
    (claude-dashboard--insert-footer)))
```

Each section renderer inserts propertized text with:
- `claude-dashboard-entry-type` property (`repo` | `worktree` | `subrepo` | `button`)
- `claude-dashboard-entry-data` property (the data struct for that entry)
- Navigation commands use these properties to determine context

### Entry Data Properties

```elisp
;; On a repo header line:
(get-text-property (point) 'claude-dashboard-entry-type)  ; → 'repo
(get-text-property (point) 'claude-dashboard-entry-data)  ; → (:name "acorn" :path "/Users/...")

;; On a worktree line:
(get-text-property (point) 'claude-dashboard-entry-type)  ; → 'worktree
(get-text-property (point) 'claude-dashboard-entry-data)  ; → (:id "acorn-feature-x" :branch "feature-x" ...)

;; On a button:
(get-text-property (point) 'claude-dashboard-entry-type)  ; → 'button
(get-text-property (point) 'claude-dashboard-action)      ; → #'claude-dashboard--create-worktree
```
