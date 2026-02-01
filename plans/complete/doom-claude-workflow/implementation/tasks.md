# Implementation Tasks

## Prerequisites

- [x] **P1: Automate Claude CLI installation** *(Already done in install.sh)*

## Phase 0: Validation Spike

- [ ] **0.1: Verify vterm buffer reading in IELM**
  - Start a vterm buffer manually
  - Test `vterm--filter-buffer-substring` on buffer content
  - Verify buffer-local variables persist after switching buffers
  - Test hashing approach for change detection
  - Success criteria:
    - Can extract last 15 lines cleanly
    - Hash changes when new output appears
    - Buffer-local vars survive buffer switches
  - **If this fails:** Investigate alternative approaches before proceeding

## Phase 1: Foundation

- [ ] **1.1: Create module directory structure**
  - Create `emacs/doom/modules/claude/` directory
  - Create stub files: `claude.el`, `claude-worktree.el`, `claude-workspace.el`, `claude-monitor.el`, `claude-dashboard.el`, `claude-cleanup.el`
  - Update `config.el` with Doom-idiomatic loading:
    ```elisp
    (add-load-path! "modules/claude")
    (after! doom-modeline
      (load! "modules/claude/claude"))
    ```
  - Success criteria:
    - `doom sync` completes without errors
    - Emacs starts without errors
    - `M-x describe-variable RET load-path` shows modules/claude

- [ ] **1.2: Implement claude-worktree.el**
  - Metadata read/write functions (JSON encode/decode)
  - Worktree create/remove/list functions
  - Git operations (commits-ahead, merge, delete-branch)
  - Repo name derivation from path
  - Error handling for git worktree:
    - Exit 255: branch already exists
    - Exit 128: non-empty directory exists
    - Return `(success . result-or-error)` cons cells
  - Success criteria:
    - In IELM: `(claude-worktree-create "/path/to/repo" "test-branch" "main")` creates worktree
    - Metadata JSON file exists at correct path with valid content
    - `(claude-worktree-list)` returns the new worktree
    - `(claude-worktree-remove "repo" "test-branch")` cleans up

- [ ] **1.3: Implement claude-workspace.el**
  - Doom workspace via `+workspace/new`, `+workspace/switch`, `+workspace/delete`
  - Query via `+workspace-list-names`, `+workspace-current-name`
  - vterm buffer creation with `vterm-send-string`
  - Buffer naming `*claude:{repo}:{branch}*`
  - Note: Workspace persistence is automatic via persp-mode
  - Success: Can create workspace with vterm from IELM

## Phase 2: Core Commands

- [ ] **2.1: Implement claude-create-workspace command**
  - Detect current repo (project-root)
  - Prompt for branch name
  - Wire together worktree + workspace creation
  - Start vterm, cd, run `claude`
  - Depends on: 1.2, 1.3
  - Success: `SPC C c` creates full workspace

- [ ] **2.2: Implement claude-cleanup.el**
  - Status buffer with merge info
  - View diff action (magit integration)
  - Merge and cleanup action
  - Delete without merge action
  - Depends on: 1.2, 1.3
  - Success: `SPC C x` shows status, can merge/delete

## Phase 3: Monitoring

- [ ] **3.1: Implement claude-monitor.el**
  - Buffer-local state tracking (`claude--last-output-time`, `claude--last-content-hash`, `claude--needs-attention`)
  - Content extraction using verified approach:
    ```elisp
    (defun claude--get-last-n-lines (buffer n)
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-max))
          (forward-line (- n))
          (vterm--filter-buffer-substring
           (buffer-substring (point) (point-max))))))
    ```
  - MD5 hashing for change detection
  - Pattern matching against last 15 lines (updated patterns from verification.md)
  - Timer-based polling via `run-with-timer` (2s interval, 3s staleness threshold)
  - Depends on: 0.1, 1.3
  - Success criteria:
    - Start Claude in vterm, wait for prompt
    - `claude--needs-attention` becomes non-nil after 3s idle
    - Type something, `claude--needs-attention` clears
    - Permission prompt triggers attention immediately after staleness

- [ ] **3.2: Integrate modeline segment**
  - Define `claude-status` segment via `doom-modeline-def-segment`
  - Define custom modeline layout `claude-main` via `doom-modeline-def-modeline`
  - Set as global default with `doom-modeline-set-main-modeline`
  - Click action jumps to buffer needing attention
  - Display: hidden (no sessions), `○ Claude` (idle), `● Claude` (attention)
  - Depends on: 3.1
  - Success: Modeline shows Claude status globally across all buffers

## Phase 4: Dashboard

- [ ] **4.1: Implement claude-dashboard.el**
  - Dashboard buffer with major mode
  - Render workspace list with status
  - Navigation (j/k/RET)
  - Actions (c/x/g/q)
  - Filter by repo (/)
  - Depends on: 1.2, 1.3, 3.1
  - Success: `SPC C d` shows all workspaces with status

## Phase 5: Polish

- [ ] **5.1: Implement remaining commands**
  - `claude-jump-to-buffer` (SPC C j)
  - `claude-magit-status` (SPC C g)
  - `claude-monitor-toggle` (SPC C m)
  - Success: All keybindings work

- [ ] **5.2: Edge case handling**
  - Branch already exists flow
  - Worktree directory exists flow
  - Not in git repo flow
  - Claude CLI not found error
  - Partial failure cleanup (e.g., worktree created but workspace creation fails)
  - Merge conflict handling (abort merge, open magit, inform user)
  - Success: All edge cases handled gracefully

- [ ] **5.3: Update CLAUDE.md documentation**
  - Document workflow purpose
  - Keybinding reference
  - Troubleshooting guide
  - Success: New user can understand system from docs

## Success Criteria (Overall)

1. Fresh `install.sh` run installs Claude CLI
2. `SPC C c` creates isolated worktree + workspace + Claude session
3. Modeline shows Claude status globally (visible in all buffers)
4. `SPC C d` shows all active sessions with status
5. `SPC C x` safely merges or deletes with confirmation
6. Workspaces survive Emacs restart (via persp-mode persistence)
7. All edge cases show helpful prompts, not errors
