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
   c new   s sync   x close   r remove   RET jump   TAB fold   ? help
```

### Visual Elements

**Repo headers** — Large text, subtle background band (like magit section headers). Nerd-font icon  for git. Repo name in bold. `[Open]` button on the right — box face with background.

**Worktree entries** — Tree lines using `├ └ │` in a dim face. Branch icon  before branch name. Commit count right-aligned (for grouped workspaces, this is the **sum** across all repos). Status indicator with colored dot:
- `⚡ working` — green face, dim (all good, no action needed)
- ` waiting` — amber/yellow face, bold (needs attention — Claude stopped)
- ` error` — red face, bold (something broke)
- ` creating…` — blue face, dim (grove workspace create in progress)
- ` closing…` — blue face, dim (grove workspace close in progress)
- `✖ failed` — red face, bold (grove operation failed — retry or discard)

The attention states (working/waiting/error) apply when grove status is `active`. The lifecycle states (creating/closing/failed) come directly from grove and take precedence — no vterm monitoring needed for workspaces that aren't active yet.

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
claude-dashboard-lifecycle-face     ; blue foreground, dim (creating/closing)
claude-dashboard-failed-face        ; red foreground, bold (failed state)

;; Navigation
claude-dashboard-current-face       ; subtle background highlight for current entry

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
| `r` | repo header | Remove repo (`grove repo remove`, confirms first) |
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
SPC ;  →  Main workspace (dashboard auto-focused)
             │
             ├── RET on repo → Doom workspace for that repo (home)
             │                  └── SPC ; → back to main
             │
             ├── RET on worktree → Doom workspace for that worktree
             │                      └── SPC ; → back to main
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
- Calls `grove repo list --json` async (tier 1) — skips if a previous call is still in-flight (debounce)
- Fires `grove workspace status` per active workspace (tier 2) — progressive enrichment
- Calls `claude-workspace-attention` per workspace (local, always fast)
- Preserves cursor position across refreshes (by remembering which entry point is on)
- Timer stops when dashboard buffer is killed or buried

**Event-driven refresh** for immediate updates:
- After any grove command completes (create, sync, close)
- After attention state changes (via `claude-attention-change-hook`)

## Buffer Construction

The dashboard is a read-only special-mode buffer (`claude-dashboard-mode`), not derived from tabulated-list-mode. Custom rendering gives full control over layout.

### Rendering Pipeline

Rendering is split into three parts: tier 1 fetch (async, instant), buffer painting (sync, fast), and tier 2 enrichment (async, progressive).

**Data cache** — A buffer-local variable holds the merged grove data. Both tiers write to the cache; the paint function reads from it. This decouples fetching from rendering.

```elisp
(defvar-local claude-dashboard--grove-cache nil
  "Cached grove data. Updated by tier 1 and tier 2 callbacks.
Structure mirrors `grove repo list --json` output, progressively
enriched with per-workspace git stats from tier 2.")
```

**Refresh flow:**

```elisp
(defun claude-dashboard-refresh ()
  "Fetch fresh data from grove and re-render (two-tier)."
  ;; Tier 1: instant, no git calls
  (claude-grove-repo-list
   (lambda (ok data error-msg)
     (if (not ok)
         (claude-dashboard--render-error error-msg)
       ;; Update cache with tier 1 data, render immediately
       (setq claude-dashboard--grove-cache data)
       (claude-dashboard--paint)
       ;; Tier 2: fire per-workspace status calls in parallel
       (dolist (repo data)
         (dolist (ws (plist-get repo :workspaces))
           (when (equal (plist-get ws :status) "active")
             (claude-grove-workspace-status
              (plist-get ws :branch)
              (lambda (ok status-data _err)
                (when ok
                  (claude-dashboard--merge-workspace-status status-data)
                  (claude-dashboard--paint)))))))))))

(defun claude-dashboard--merge-workspace-status (status-data)
  "Merge tier 2 STATUS-DATA into `claude-dashboard--grove-cache'."
  ;; Find the matching workspace in cache by ID, replace its :repos
  ;; with the detailed per-repo data from tier 2.
  ...)

(defun claude-dashboard--paint ()
  "Paint the dashboard buffer from cache. Fast, no I/O."
  (let* ((data      claude-dashboard--grove-cache)
         (attention (claude-dashboard--collect-attention))
         (collapsed claude-dashboard--collapsed-repos)
         (inhibit-read-only t))
    ;; 1. Clear and rebuild
    (erase-buffer)
    ;; 2. Title
    (claude-dashboard--insert-title)
    ;; 3. Each repo section (skip children if collapsed)
    (dolist (repo data)
      (claude-dashboard--insert-repo-section repo attention collapsed))
    ;; 4. Footer
    (claude-dashboard--insert-footer)
    ;; 5. Clean up orphaned Doom workspaces
    (claude-dashboard--cleanup-orphans data)))

(defun claude-dashboard--render-error (error-msg)
  "Show error state — e.g., grove not installed."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (claude-dashboard--insert-title)
    (insert "\n  " (propertize error-msg 'face 'claude-dashboard-error-face))
    (when (string-match-p "not found" error-msg)
      (insert "\n\n  Install grove: npm install -g @twiglylabs/grove"))
    (claude-dashboard--insert-footer)))
```

**What renders before tier 2 completes:**
- Repo headers — full (from tier 1)
- Worktree entries — branch name, status, attention indicator (from tier 1 + monitor)
- Commit counts — omitted until tier 2 (no placeholder, column just absent)
- Sub-repo details — omitted until tier 2 (section appears when data arrives)

This avoids loading spinners or placeholder text — the dashboard simply shows what it knows, then fills in details as they arrive.

### Collapse/Expand State

```elisp
(defvar-local claude-dashboard--collapsed-repos nil
  "Set of repo names whose sections are collapsed.
Persists across refreshes (buffer-local, not buffer-content).")
```

TAB on a repo header toggles its name in this set. The rendering pipeline checks the set and skips worktree/sub-repo entries for collapsed repos. Since this is buffer-local state (not stored in the buffer text), it survives erase-and-rebuild refreshes.

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
