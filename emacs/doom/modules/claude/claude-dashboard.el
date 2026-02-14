;;; claude-dashboard.el --- Mission control dashboard -*- lexical-binding: t; -*-

;;; Commentary:
;; Mission control dashboard for all workspace orchestration.
;; Renders from grove CLI data via claude-grove.el.
;; Attention status from claude-monitor.el.
;; Handles Doom workspace and vterm buffer lifecycle.

;;; Code:

(require 'claude-grove)
(require 'claude-monitor)

;; Forward declarations for Doom workspace functions
(declare-function +workspace-exists-p "~/.config/emacs/modules/ui/workspaces/autoload/workspaces")
(declare-function +workspace/new "~/.config/emacs/modules/ui/workspaces/autoload/workspaces")
(declare-function +workspace/switch-to "~/.config/emacs/modules/ui/workspaces/autoload/workspaces")
(declare-function +workspace-kill "~/.config/emacs/modules/ui/workspaces/autoload/workspaces")
(declare-function +workspace-list-names "~/.config/emacs/modules/ui/workspaces/autoload/workspaces")

;; Forward declarations for vterm
(declare-function vterm "vterm")
(declare-function vterm-send-string "vterm")

;; Forward declaration for treemacs
(declare-function treemacs-add-and-display-current-project-exclusively "treemacs")

;; Forward declarations for projectile
(declare-function projectile-remove-known-project "projectile")
(defvar projectile-known-projects)

;;; Customization

(defcustom claude-dashboard-refresh-interval 5
  "Seconds between auto-refresh cycles."
  :type 'integer
  :group 'claude)

;;; Faces

(defface claude-dashboard-title-face
  '((t :weight bold :height 1.3))
  "Face for the dashboard title.")

(defface claude-dashboard-repo-face
  '((t :weight bold :height 1.1 :extend t))
  "Face for repo header names.")

(defface claude-dashboard-repo-dim-face
  '((t :weight normal :height 1.1 :inherit shadow))
  "Face for repos with no workspaces.")

(defface claude-dashboard-branch-face
  '((t :weight normal))
  "Face for branch names.")

(defface claude-dashboard-commits-face
  '((t :inherit shadow))
  "Face for commit counts.")

(defface claude-dashboard-working-face
  '((t :inherit success))
  "Face for working status.")

(defface claude-dashboard-waiting-face
  '((t :inherit warning :weight bold))
  "Face for waiting/idle status.")

(defface claude-dashboard-error-face
  '((t :inherit error :weight bold))
  "Face for error status.")

(defface claude-dashboard-lifecycle-face
  '((t :inherit font-lock-keyword-face))
  "Face for lifecycle states (creating/closing).")

(defface claude-dashboard-failed-face
  '((t :inherit error :weight bold))
  "Face for failed state.")

(defface claude-dashboard-current-face
  '((t :inherit hl-line))
  "Face for the current entry highlight.")

(defface claude-dashboard-subrepo-face
  '((t :inherit shadow))
  "Face for sub-repo details.")

(defface claude-dashboard-clean-face
  '((t :inherit (success shadow)))
  "Face for clean sub-repo status.")

(defface claude-dashboard-dirty-face
  '((t :inherit warning))
  "Face for dirty sub-repo status.")

(defface claude-dashboard-button-face
  '((t :box (:line-width (2 . 8) :style flat-button) :inherit default))
  "Face for action buttons.")

(defface claude-dashboard-button-selected-face
  '((t :inverse-video t :weight bold))
  "Face for the currently selected action button.")

(defface claude-dashboard-tree-face
  '((t :inherit shadow))
  "Face for tree drawing characters.")

(defface claude-dashboard-footer-face
  '((t :inherit shadow))
  "Face for the footer hint bar.")

;;; Dynamic Variables

(defvar claude-dashboard--column-width nil
  "Column width for rendering.
When non-nil, constrains repo section rendering to this width.
Dynamically bound during two-column rendering.")

;;; Buffer-Local State

(defvar-local claude-dashboard--grove-cache nil
  "Cached grove data. Updated by tier 1 and tier 2 callbacks.
Structure mirrors `grove repo list --json' output, progressively
enriched with per-workspace git stats from tier 2.")

(defvar-local claude-dashboard--collapsed-repos nil
  "Set of repo names whose sections are collapsed.")

(defvar-local claude-dashboard--refresh-timer nil
  "Buffer-local refresh timer.")

(defvar-local claude-dashboard--current-entry-overlay nil
  "Overlay for highlighting the current entry.")

(defvar-local claude-dashboard--button-overlay nil
  "Overlay for highlighting the active button in the current entry.")

(defvar-local claude-dashboard--tier2-paint-timer nil
  "Debounce timer for tier 2 repaints.")

;;; Dashboard Mode

(defvar claude-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'claude-dashboard-select)
    (define-key map "c" #'claude-dashboard-create)
    (define-key map "s" #'claude-dashboard-sync)
    (define-key map "x" #'claude-dashboard-close)
    (define-key map "a" #'claude-dashboard-add-repo)
    (define-key map "r" #'claude-dashboard-remove-repo)
    (define-key map "j" #'claude-dashboard-next)
    (define-key map "k" #'claude-dashboard-prev)
    (define-key map "n" #'claude-dashboard-next)
    (define-key map "p" #'claude-dashboard-prev)
    (define-key map (kbd "TAB") #'claude-dashboard-toggle-collapse)
    (define-key map "g" #'claude-dashboard-refresh)
    (define-key map "q" #'claude-dashboard-quit)
    (define-key map "?" #'claude-dashboard-help)
    map)
  "Keymap for Claude dashboard mode.")

(define-derived-mode claude-dashboard-mode special-mode "Claude"
  "Major mode for the Claude mission control dashboard.

\\{claude-dashboard-mode-map}"
  :group 'claude
  (setq buffer-read-only t
        truncate-lines t
        cursor-type nil
        mode-line-format
        (list "  "
              (propertize
               "c:new  s:sync  x:close  a:add  r:remove  RET:jump  TAB:fold  ?:help"
               'face 'claude-dashboard-footer-face)))
  (claude-dashboard--setup-refresh-timer)
  (add-hook 'post-command-hook #'claude-dashboard--post-command nil t)
  (add-hook 'kill-buffer-hook #'claude-dashboard--teardown nil t))

;; Evil bindings
(with-eval-after-load 'evil
  (add-hook 'claude-dashboard-mode-hook
            (lambda ()
              (when (fboundp 'evil-local-set-key)
                (evil-local-set-key 'normal (kbd "RET") #'claude-dashboard-select)
                (evil-local-set-key 'normal "c" #'claude-dashboard-create)
                (evil-local-set-key 'normal "s" #'claude-dashboard-sync)
                (evil-local-set-key 'normal "x" #'claude-dashboard-close)
                (evil-local-set-key 'normal "a" #'claude-dashboard-add-repo)
                (evil-local-set-key 'normal "r" #'claude-dashboard-remove-repo)
                (evil-local-set-key 'normal "j" #'claude-dashboard-next)
                (evil-local-set-key 'normal "k" #'claude-dashboard-prev)
                (evil-local-set-key 'normal "n" #'claude-dashboard-next)
                (evil-local-set-key 'normal "p" #'claude-dashboard-prev)
                (evil-local-set-key 'normal (kbd "TAB") #'claude-dashboard-toggle-collapse)
                (evil-local-set-key 'normal "g" #'claude-dashboard-refresh)
                (evil-local-set-key 'normal "q" #'claude-dashboard-quit)
                (evil-local-set-key 'normal "?" #'claude-dashboard-help)))))

;;; Teardown

(defun claude-dashboard--teardown ()
  "Clean up when dashboard buffer is killed."
  (when claude-dashboard--refresh-timer
    (cancel-timer claude-dashboard--refresh-timer))
  (when claude-dashboard--tier2-paint-timer
    (cancel-timer claude-dashboard--tier2-paint-timer))
  (when claude-dashboard--current-entry-overlay
    (delete-overlay claude-dashboard--current-entry-overlay))
  (when claude-dashboard--button-overlay
    (delete-overlay claude-dashboard--button-overlay)))

;;; Auto-Refresh

(defun claude-dashboard--setup-refresh-timer ()
  "Start the auto-refresh timer."
  (setq claude-dashboard--refresh-timer
        (run-with-timer claude-dashboard-refresh-interval
                        claude-dashboard-refresh-interval
                        #'claude-dashboard--maybe-refresh
                        (current-buffer))))

(defun claude-dashboard--maybe-refresh (buffer)
  "Refresh BUFFER if it's still live and visible."
  (when (and (buffer-live-p buffer)
             (get-buffer-window buffer t))
    (with-current-buffer buffer
      (claude-dashboard-refresh))))

;;; Refresh & Data Pipeline

(defun claude-dashboard--preserve-tier2 (new-data old-data)
  "Copy tier 2 enrichments from OLD-DATA into NEW-DATA.
Prevents commit counts from flashing on each refresh cycle."
  (when (and old-data new-data)
    (let ((old-repos (plist-get old-data :repos))
          (new-repos (plist-get new-data :repos)))
      (when (and old-repos new-repos)
        (let ((old-ws-table (make-hash-table :test 'equal)))
          ;; Index old workspaces by id
          (seq-doseq (repo old-repos)
            (when-let ((workspaces (plist-get repo :workspaces)))
              (seq-doseq (ws (append workspaces nil))
                (when-let ((id (plist-get ws :id)))
                  (puthash id ws old-ws-table)))))
          ;; Merge into new workspaces
          (seq-doseq (repo new-repos)
            (when-let ((workspaces (plist-get repo :workspaces)))
              (dotimes (i (length workspaces))
                (let* ((ws (aref workspaces i))
                       (id (plist-get ws :id))
                       (old-ws (gethash id old-ws-table)))
                  (when old-ws
                    (when-let ((tc (plist-get old-ws :total-commits)))
                      (plist-put ws :total-commits tc))
                    (when-let ((rd (plist-get old-ws :repos)))
                      (plist-put ws :repos rd))))))))))))

;;;###autoload
(defun claude-dashboard-refresh ()
  "Fetch fresh data from grove and re-render."
  (interactive)
  (let ((dashboard-buf (current-buffer)))
    (claude-grove-repo-list
     (lambda (ok data error-msg)
       (when (buffer-live-p dashboard-buf)
         (with-current-buffer dashboard-buf
           (if (not ok)
               (claude-dashboard--render-error error-msg)
             ;; Update cache, preserving tier 2 enrichments
             (claude-dashboard--preserve-tier2 data claude-dashboard--grove-cache)
             (setq claude-dashboard--grove-cache data)
             (claude-dashboard--paint)
             ;; Cleanup orphaned Doom workspaces
             (claude-dashboard--cleanup-orphans data)
             ;; Tier 2: fire per-workspace status calls
             (claude-dashboard--fire-tier2 dashboard-buf data))))))))

(defun claude-dashboard--fire-tier2 (dashboard-buf data)
  "Fire tier 2 workspace status calls for DATA.
DASHBOARD-BUF is the dashboard buffer to update."
  (when-let ((repos (plist-get data :repos)))
    (seq-doseq (repo repos)
      (when-let ((workspaces (plist-get repo :workspaces)))
        (seq-doseq (ws workspaces)
          (when (equal (plist-get ws :status) "active")
            (claude-grove-workspace-status
             (plist-get ws :branch)
             (lambda (ok status-data _err)
               (when (and ok (buffer-live-p dashboard-buf))
                 (with-current-buffer dashboard-buf
                   (claude-dashboard--merge-workspace-status status-data)
                   (claude-dashboard--schedule-paint)))))))))))

(defun claude-dashboard--schedule-paint ()
  "Schedule a debounced repaint.
Batches rapid tier 2 arrivals into a single repaint."
  (when claude-dashboard--tier2-paint-timer
    (cancel-timer claude-dashboard--tier2-paint-timer))
  (let ((buf (current-buffer)))
    (setq claude-dashboard--tier2-paint-timer
          (run-at-time 0.2 nil
                       (lambda ()
                         (when (buffer-live-p buf)
                           (with-current-buffer buf
                             (claude-dashboard--paint))))))))

(defun claude-dashboard--merge-workspace-status (status-data)
  "Merge tier 2 STATUS-DATA into the grove cache."
  (when-let* ((cache claude-dashboard--grove-cache)
              (repos (plist-get cache :repos))
              (ws-id (plist-get status-data :id)))
    (seq-doseq (repo repos)
      (when-let ((workspaces (plist-get repo :workspaces)))
        (dotimes (i (length workspaces))
          (let ((ws (aref workspaces i)))
            (when (equal (plist-get ws :id) ws-id)
              ;; Merge the detailed repos data into the workspace
              (plist-put ws :repos (plist-get status-data :repos))
              ;; Compute total commits from all repos
              (let ((total 0))
                (seq-doseq (r (plist-get status-data :repos))
                  (setq total (+ total (or (plist-get r :commits) 0))))
                (plist-put ws :total-commits total)))))))))

;;; Rendering

(defun claude-dashboard--paint ()
  "Paint the dashboard buffer from cache. Fast, no I/O."
  (let* ((data claude-dashboard--grove-cache)
         (collapsed claude-dashboard--collapsed-repos)
         (inhibit-read-only t)
         (saved-entry (claude-dashboard--entry-at-point))
         (saved-line (line-number-at-pos))
         (win (get-buffer-window (current-buffer)))
         (win-width (if win (window-body-width win) 80))
         (repos (and data (plist-get data :repos)))
         (repo-count (if repos (length repos) 0))
         (use-two-col (and (> repo-count 3) (>= win-width 140))))
    (erase-buffer)
    ;; Title
    (claude-dashboard--insert-title)
    ;; Repo sections
    (if (> repo-count 0)
        (if use-two-col
            (claude-dashboard--paint-two-column repos collapsed win-width)
          (seq-doseq (repo repos)
            (claude-dashboard--insert-repo-section repo collapsed)))
      (insert "\n"
              (propertize "   No repos registered."
                          'face 'claude-dashboard-footer-face)
              "\n"
              (propertize "   Press 'a' to add a repo."
                          'face 'claude-dashboard-footer-face)
              "\n"))
    ;; Restore position
    (claude-dashboard--restore-position saved-entry saved-line)))

(defun claude-dashboard--render-error (error-msg)
  "Show error state with ERROR-MSG."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (claude-dashboard--insert-title)
    (insert "\n   "
            (propertize (or error-msg "Unknown error")
                        'face 'claude-dashboard-error-face))
    (when (and error-msg (string-match-p "not found" error-msg))
      (insert "\n\n   "
              (propertize "Install grove: npm install -g @twiglylabs/grove"
                          'face 'claude-dashboard-footer-face)))
    (insert "\n")))

;;; Section Renderers

(defun claude-dashboard--insert-title ()
  "Insert the dashboard title."
  (insert "\n\n   "
          (propertize "Claude Workspaces"
                      'face 'claude-dashboard-title-face)
          "\n\n"))

(defun claude-dashboard--insert-repo-section (repo collapsed)
  "Insert a repo section for REPO, respecting COLLAPSED set."
  (let* ((name (plist-get repo :name))
         (path (plist-get repo :path))
         (workspaces (plist-get repo :workspaces))
         (ws-list (if workspaces (append workspaces nil) nil))
         (is-collapsed (member name collapsed))
         (has-workspaces (and ws-list (> (length ws-list) 0))))
    ;; Sort workspaces: attention-needing first
    (when has-workspaces
      (setq ws-list (claude-dashboard--sort-workspaces ws-list name)))
    ;; Repo header line
    (insert "\n")
    (let ((line-start (point))
          (home-attention (claude-workspace-attention
                           (format "%s:home" name))))
      (insert "   "
              (propertize name
                          'face (if has-workspaces
                                    'claude-dashboard-repo-face
                                  'claude-dashboard-repo-dim-face)))
      ;; Home session status (when a claude session is active)
      (when home-attention
        (insert "  ")
        (claude-dashboard--insert-status "active" home-attention))
      ;; Right-aligned Open button
      (claude-dashboard--insert-right-aligned
       (propertize " Open "
                   'face 'claude-dashboard-button-face
                   'mouse-face 'highlight
                   'claude-dashboard-action 'open-home
                   'claude-dashboard-entry-data (list :name name :path path))
       line-start)
      (insert "\n")
      ;; Apply text properties to entire line
      (put-text-property line-start (point) 'claude-dashboard-entry-type 'repo)
      (put-text-property line-start (point) 'claude-dashboard-entry-data
                         (list :name name :path path)))
    ;; Worktree entries (if not collapsed)
    (when (and has-workspaces (not is-collapsed))
      (let ((len (length ws-list)))
        (dotimes (i len)
          (let* ((ws (nth i ws-list))
                 (is-last (= i (1- len))))
            (claude-dashboard--insert-worktree-entry ws name path is-last)
            ;; Sub-repo details (tier 2)
            (when-let ((repos-detail (plist-get ws :repos)))
              (when (> (length repos-detail) 1)
                (seq-doseq (sr repos-detail)
                  (claude-dashboard--insert-subrepo-entry sr is-last))))))))
    ;; Collapsed indicator
    (when (and has-workspaces is-collapsed)
      (insert "   "
              (propertize (format "  (%d workspaces)" (length ws-list))
                          'face 'claude-dashboard-footer-face)
              "\n"))
    ;; No workspaces message
    (when (not has-workspaces)
      (insert "   "
              (propertize "  (no active workspaces)"
                          'face 'claude-dashboard-footer-face)
              "\n"))
    ;; New Worktree button
    (let ((btn-start (point)))
      (insert "   ")
      (let ((line-start (point)))
        (claude-dashboard--insert-right-aligned
         (propertize " + New Worktree "
                     'face 'claude-dashboard-button-face
                     'mouse-face 'highlight
                     'claude-dashboard-action 'create-worktree
                     'claude-dashboard-entry-data (list :name name :path path))
         line-start))
      (insert "\n")
      (put-text-property btn-start (point) 'claude-dashboard-entry-type 'button)
      (put-text-property btn-start (point) 'claude-dashboard-entry-data
                         (list :name name :path path)))))

(defun claude-dashboard--insert-worktree-entry (ws repo-name repo-path is-last)
  "Insert a worktree entry for WS under REPO-NAME.
REPO-PATH is the parent repo directory.
IS-LAST indicates if this is the last entry (affects tree drawing)."
  (let* ((branch (plist-get ws :branch))
         (status (plist-get ws :status))
         (total-commits (plist-get ws :total-commits))
         (ws-name (format "%s:%s" repo-name branch))
         (attention (claude-workspace-attention ws-name))
         (tree-char (if is-last "└" "├"))
         (line-start (point)))
    ;; Tree line + branch name
    (insert "   "
            (propertize (format "%s  " tree-char)
                        'face 'claude-dashboard-tree-face)
            (propertize branch 'face 'claude-dashboard-branch-face))
    ;; Commit count (if available from tier 2)
    (when total-commits
      (insert "  "
              (propertize (format "%d commit%s"
                                  total-commits
                                  (if (= total-commits 1) "" "s"))
                          'face 'claude-dashboard-commits-face)))
    ;; Status indicator
    (insert "  ")
    (claude-dashboard--insert-status status attention)
    ;; Right-aligned action buttons (only for active workspaces)
    (when (equal status "active")
      (claude-dashboard--insert-right-aligned
       (concat
        (propertize " Jump "
                    'face 'claude-dashboard-button-face
                    'mouse-face 'highlight
                    'claude-dashboard-action 'jump-to-worktree)
        " "
        (propertize " Close "
                    'face 'claude-dashboard-button-face
                    'mouse-face 'highlight
                    'claude-dashboard-action 'close-worktree))
       line-start))
    (insert "\n")
    ;; Apply text properties
    (put-text-property line-start (point) 'claude-dashboard-entry-type 'worktree)
    (put-text-property line-start (point) 'claude-dashboard-entry-data
                       (list :id (plist-get ws :id)
                             :branch branch
                             :status status
                             :repo-name repo-name
                             :path repo-path
                             :root (plist-get ws :root)))))

(defun claude-dashboard--insert-subrepo-entry (sr is-last-ws)
  "Insert a sub-repo detail line for SR.
IS-LAST-WS indicates if this is under the last worktree entry."
  (let* ((name (plist-get sr :name))
         (dirty (or (plist-get sr :dirty) 0))
         (commits (or (plist-get sr :commits) 0))
         (cont-char (if is-last-ws " " "│"))
         (line-start (point)))
    (insert "   "
            (propertize (format "%s    " cont-char)
                        'face 'claude-dashboard-tree-face)
            (propertize name 'face 'claude-dashboard-subrepo-face))
    ;; Commit count
    (when (> commits 0)
      (insert "  "
              (propertize (format "%d commit%s" commits (if (= commits 1) "" "s"))
                          'face 'claude-dashboard-commits-face)))
    ;; Clean/dirty status
    (insert "  ")
    (if (= dirty 0)
        (insert (propertize "✓ clean" 'face 'claude-dashboard-clean-face))
      (insert (propertize (format "dirty (%d)" dirty)
                          'face 'claude-dashboard-dirty-face)))
    (insert "\n")
    (put-text-property line-start (point) 'claude-dashboard-entry-type 'subrepo)))

(defun claude-dashboard--insert-status (grove-status attention)
  "Insert status indicator for GROVE-STATUS and ATTENTION."
  (cond
   ;; Lifecycle states take precedence
   ((equal grove-status "creating")
    (insert (propertize "◌ creating…" 'face 'claude-dashboard-lifecycle-face)))
   ((equal grove-status "closing")
    (insert (propertize "◌ closing…" 'face 'claude-dashboard-lifecycle-face)))
   ((equal grove-status "failed")
    (insert (propertize "✖ failed" 'face 'claude-dashboard-failed-face)))
   ;; Active — use attention
   ((equal grove-status "active")
    (pcase attention
      ('idle
       (insert (propertize "● waiting" 'face 'claude-dashboard-waiting-face)))
      ('error
       (insert (propertize "✖ error" 'face 'claude-dashboard-error-face)))
      ('working
       (insert (propertize "⚡working" 'face 'claude-dashboard-working-face)))
      (_
       (insert (propertize "⚡working" 'face 'claude-dashboard-working-face)))))
   ;; Unknown
   (t
    (insert (propertize (or grove-status "unknown")
                        'face 'claude-dashboard-footer-face)))))

(defun claude-dashboard--insert-footer ()
  "Insert the footer hint bar, pinned to the bottom of the window."
  (let* ((footer-lines 3)
         (content-lines (count-lines (point-min) (point)))
         (win (get-buffer-window (current-buffer)))
         (win-height (or (when win (window-body-height win)) 40))
         (win-width (or (when win (window-body-width win)) 80))
         (rule-width (min (- win-width 4) 120))
         (padding (max 1 (- win-height content-lines footer-lines))))
    (insert (make-string padding ?\n)
            "  "
            (propertize (make-string rule-width ?─)
                        'face 'claude-dashboard-footer-face)
            "\n   "
            (propertize "c new   s sync   x close   a add   r remove   RET jump   TAB fold   ? help"
                        'face 'claude-dashboard-footer-face)
            "\n")))

;;; Helpers

(defun claude-dashboard--insert-right-aligned (text ref-start)
  "Insert TEXT right-aligned relative to column width.
REF-START is the start of the current line.
Uses `claude-dashboard--column-width' when set, otherwise
sizes to the window or a default of 72."
  (let* ((col-width (or claude-dashboard--column-width
                        (when-let ((win (get-buffer-window (current-buffer))))
                          (min (window-body-width win) 120))
                        72))
         (text-width (string-width text))
         (target-col (max 0 (- col-width text-width 4)))
         (current-col (- (point) ref-start)))
    (when (> target-col current-col)
      (insert (make-string (- target-col current-col) ?\s)))
    (insert text)))

(defun claude-dashboard--render-repo-lines (repo collapsed col-width)
  "Render REPO section as a list of propertized line strings.
COL-WIDTH constrains the rendering width."
  (let ((claude-dashboard--column-width col-width))
    (with-temp-buffer
      (claude-dashboard--insert-repo-section repo collapsed)
      (goto-char (point-min))
      ;; Skip leading blank lines
      (while (and (not (eobp)) (looking-at-p "^$"))
        (forward-line 1))
      (let (lines)
        (while (not (eobp))
          (push (buffer-substring (line-beginning-position) (line-end-position)) lines)
          (forward-line 1))
        ;; Remove trailing blank lines
        (while (and lines (string-empty-p (car lines)))
          (setq lines (cdr lines)))
        (nreverse lines)))))

(defun claude-dashboard--paint-two-column (repos collapsed win-width)
  "Render REPOS in two columns within WIN-WIDTH."
  (let* ((gutter 6)
         (col-width (/ (- win-width gutter) 2))
         (repo-list (append repos nil))
         (len (length repo-list))
         (i 0))
    (while (< i len)
      (let* ((left (nth i repo-list))
             (right (when (< (1+ i) len) (nth (1+ i) repo-list)))
             (left-lines (claude-dashboard--render-repo-lines
                          left collapsed col-width))
             (right-lines (when right
                            (claude-dashboard--render-repo-lines
                             right collapsed col-width)))
             (max-h (max (length left-lines)
                         (if right-lines (length right-lines) 0))))
        (insert "\n")
        (dotimes (j max-h)
          (let ((ll (or (nth j left-lines) ""))
                (rl (when right-lines (or (nth j right-lines) ""))))
            (insert ll)
            (when (and rl (not (string-empty-p rl)))
              (let ((pad (max 1 (- (+ col-width gutter)
                                   (string-width ll)))))
                (insert (make-string pad ?\s))
                (insert rl)))
            (insert "\n")))
        (setq i (+ i 2))))))

(defun claude-dashboard--sort-workspaces (ws-list repo-name)
  "Sort WS-LIST with attention-needing workspaces first.
REPO-NAME is the parent repo name, used for attention lookup."
  (sort (copy-sequence ws-list)
        (lambda (a b)
          (let* ((a-ws (format "%s:%s" repo-name (plist-get a :branch)))
                 (b-ws (format "%s:%s" repo-name (plist-get b :branch)))
                 (a-attention (claude-workspace-attention a-ws))
                 (b-attention (claude-workspace-attention b-ws))
                 (a-needs (memq a-attention '(idle error)))
                 (b-needs (memq b-attention '(idle error))))
            (cond
             ((and a-needs (not b-needs)) t)
             ((and (not a-needs) b-needs) nil)
             (t (string< (plist-get a :branch) (plist-get b :branch))))))))

;;; Position Preservation

(defun claude-dashboard--entry-at-point ()
  "Get the entry data at point for position restoration."
  (let ((type (get-text-property (point) 'claude-dashboard-entry-type))
        (data (get-text-property (point) 'claude-dashboard-entry-data)))
    (when (and type data)
      (cons type data))))

(defun claude-dashboard--restore-position (saved-entry saved-line)
  "Restore cursor to SAVED-ENTRY, falling back to SAVED-LINE."
  (if saved-entry
      ;; Try to find the same entry
      (let ((found nil))
        (goto-char (point-min))
        (while (and (not found) (not (eobp)))
          (let ((type (get-text-property (point) 'claude-dashboard-entry-type))
                (data (get-text-property (point) 'claude-dashboard-entry-data)))
            (when (and type data (equal (car saved-entry) type))
              (cond
               ((and (eq type 'repo)
                     (equal (plist-get data :name)
                            (plist-get (cdr saved-entry) :name)))
                (setq found t))
               ((and (eq type 'worktree)
                     (equal (plist-get data :id)
                            (plist-get (cdr saved-entry) :id)))
                (setq found t)))))
          (unless found (forward-line 1)))
        (unless found
          (goto-char (point-min))
          (forward-line (1- (min saved-line (count-lines (point-min) (point-max)))))))
    ;; No saved entry, go to first actionable line
    (goto-char (point-min))
    (claude-dashboard-next)))

;;; Entry Snapping & Highlight

(defun claude-dashboard--snap-to-entry ()
  "If point is not on an actionable entry, snap to the nearest one.
This ensures the selection highlight is always visible."
  (unless (get-text-property (point) 'claude-dashboard-entry-type)
    (let ((entries (claude-dashboard--collect-entry-positions)))
      (when entries
        (let ((best (car entries))
              (best-dist (abs (- (car entries) (point)))))
          (dolist (pos (cdr entries))
            (let ((dist (abs (- pos (point)))))
              (when (< dist best-dist)
                (setq best pos best-dist dist))))
          (goto-char best))))))

(defun claude-dashboard--update-highlight ()
  "Update the current entry and button highlight overlays.
The entry overlay provides a subtle row/column highlight.
The button overlay highlights the actionable button within the entry."
  (when (eq major-mode 'claude-dashboard-mode)
    (let ((type (get-text-property (point) 'claude-dashboard-entry-type)))
      (if type
          (let ((beg (or (previous-single-property-change
                          (1+ (point)) 'claude-dashboard-entry-data
                          nil (point-min))
                         (point-min)))
                (end (or (next-single-property-change
                          (point) 'claude-dashboard-entry-data
                          nil (point-max))
                         (point-max))))
            ;; Subtle entry highlight (column-scoped, no :extend)
            (if claude-dashboard--current-entry-overlay
                (move-overlay claude-dashboard--current-entry-overlay beg end)
              (setq claude-dashboard--current-entry-overlay
                    (make-overlay beg end))
              (overlay-put claude-dashboard--current-entry-overlay
                           'face 'claude-dashboard-current-face))
            ;; Bright button highlight
            (claude-dashboard--highlight-button beg end))
        ;; No entry — remove all highlights
        (when claude-dashboard--current-entry-overlay
          (delete-overlay claude-dashboard--current-entry-overlay)
          (setq claude-dashboard--current-entry-overlay nil))
        (when claude-dashboard--button-overlay
          (delete-overlay claude-dashboard--button-overlay)
          (setq claude-dashboard--button-overlay nil))))))

(defun claude-dashboard--highlight-button (entry-beg entry-end)
  "Highlight the action button between ENTRY-BEG and ENTRY-END."
  (let ((pos entry-beg)
        (btn-beg nil))
    ;; Find first text with claude-dashboard-action property
    (while (and (< pos entry-end) (not btn-beg))
      (if (get-text-property pos 'claude-dashboard-action)
          (setq btn-beg pos)
        (setq pos (or (next-single-property-change
                        pos 'claude-dashboard-action nil entry-end)
                      entry-end))))
    (if btn-beg
        (let ((btn-end (or (next-single-property-change
                            btn-beg 'claude-dashboard-action nil entry-end)
                           entry-end)))
          (if claude-dashboard--button-overlay
              (move-overlay claude-dashboard--button-overlay btn-beg btn-end)
            (setq claude-dashboard--button-overlay
                  (make-overlay btn-beg btn-end))
            (overlay-put claude-dashboard--button-overlay
                         'face 'claude-dashboard-button-selected-face)
            (overlay-put claude-dashboard--button-overlay
                         'priority 10)))
      ;; No button found — hide overlay
      (when claude-dashboard--button-overlay
        (delete-overlay claude-dashboard--button-overlay)
        (setq claude-dashboard--button-overlay nil)))))

(defun claude-dashboard--post-command ()
  "Post-command handler: snap to nearest entry, then update highlight.
Ensures the selection is always on an actionable entry (Magit-style)."
  (when (eq major-mode 'claude-dashboard-mode)
    (claude-dashboard--snap-to-entry)
    (claude-dashboard--update-highlight)))

;;; Navigation

(defun claude-dashboard--collect-entry-positions ()
  "Return sorted list of buffer positions at the start of actionable entries.
Works for both single-column and two-column layouts."
  (let (positions
        (pos (point-min)))
    (while (< pos (point-max))
      (let ((type (get-text-property pos 'claude-dashboard-entry-type)))
        (if (memq type '(repo worktree button))
            (progn
              (push pos positions)
              (setq pos (or (next-single-property-change
                             pos 'claude-dashboard-entry-type)
                            (point-max))))
          (setq pos (or (next-single-property-change
                         pos 'claude-dashboard-entry-type)
                        (point-max))))))
    (nreverse positions)))

(defun claude-dashboard-next ()
  "Move to next actionable entry."
  (interactive)
  (let ((entries (claude-dashboard--collect-entry-positions))
        (pos (point))
        (found nil))
    (while (and entries (not found))
      (if (> (car entries) pos)
          (setq found (car entries))
        (setq entries (cdr entries))))
    (when found (goto-char found))))

(defun claude-dashboard-prev ()
  "Move to previous actionable entry."
  (interactive)
  (let ((entries (nreverse (claude-dashboard--collect-entry-positions)))
        (pos (point))
        (found nil))
    (while (and entries (not found))
      (if (< (car entries) pos)
          (setq found (car entries))
        (setq entries (cdr entries))))
    (when found (goto-char found))))

(defun claude-dashboard-toggle-collapse ()
  "Toggle collapse/expand of the repo section at point."
  (interactive)
  (when-let ((data (get-text-property (point) 'claude-dashboard-entry-data)))
    (let ((name (plist-get data :name)))
      (when name
        (if (member name claude-dashboard--collapsed-repos)
            (setq claude-dashboard--collapsed-repos
                  (delete name claude-dashboard--collapsed-repos))
          (push name claude-dashboard--collapsed-repos))
        (claude-dashboard--paint)))))

;;; Project Cleanup

(defun claude-dashboard--remove-project (root)
  "Remove ROOT from projectile's known projects."
  (when (and root (fboundp 'projectile-remove-known-project))
    (let ((dir (file-name-as-directory (expand-file-name root))))
      (projectile-remove-known-project dir))))

(defun claude-dashboard--prune-stale-projects ()
  "Remove non-existent directories from projectile's known projects."
  (when (and (fboundp 'projectile-remove-known-project)
             (boundp 'projectile-known-projects))
    (dolist (project projectile-known-projects)
      (unless (file-directory-p project)
        (projectile-remove-known-project project)))))

;;; Orphan Doom Workspace Cleanup

(defun claude-dashboard--cleanup-orphans (grove-data)
  "Clean up Doom workspaces that have no matching grove worktree.
GROVE-DATA is the tier 1 data from grove repo list.
Only considers workspaces whose repo prefix matches a registered
grove repo, to avoid touching unrelated workspaces."
  (when (fboundp '+workspace-list-names)
    (let ((grove-workspace-names (make-hash-table :test 'equal))
          (grove-repo-names (make-hash-table :test 'equal))
          (cleaned-any nil))
      ;; Build set of known grove workspace and repo names
      (when-let ((repos (plist-get grove-data :repos)))
        (seq-doseq (repo repos)
          (let ((repo-name (plist-get repo :name)))
            (puthash repo-name t grove-repo-names)
            ;; Home workspace
            (puthash (format "%s:home" repo-name) t grove-workspace-names)
            ;; Worktree workspaces (include transient — they're not orphans)
            (when-let ((workspaces (plist-get repo :workspaces)))
              (seq-doseq (ws workspaces)
                (puthash (format "%s:%s" repo-name (plist-get ws :branch))
                         t grove-workspace-names))))))
      ;; Check each Doom workspace
      (dolist (ws-name (+workspace-list-names))
        (when (string-match "^\\([^:]+\\):\\([^:]+\\)$" ws-name)
          (let ((repo-prefix (match-string 1 ws-name)))
            ;; Only touch workspaces belonging to registered repos
            (when (gethash repo-prefix grove-repo-names)
              ;; Skip the main dashboard workspace
              (unless (equal ws-name "claude:main")
                (unless (gethash ws-name grove-workspace-names)
                  ;; Orphaned — clean up
                  (claude-dashboard--cleanup-orphan ws-name)
                  (setq cleaned-any t)))))))
      ;; Prune stale projectile entries for deleted worktree dirs
      (when cleaned-any
        (claude-dashboard--prune-stale-projects)))))

(defun claude-dashboard--cleanup-orphan (ws-name)
  "Clean up orphaned Doom workspace WS-NAME."
  (when (string-match "^\\([^:]+\\):\\([^:]+\\)$" ws-name)
    (let ((repo-name (match-string 1 ws-name))
          (branch (match-string 2 ws-name)))
      ;; Kill vterm buffer
      (let ((buf-name (format "*claude:%s:%s*" repo-name branch)))
        (when-let ((buf (get-buffer buf-name)))
          (let ((kill-buffer-query-functions nil))
            (kill-buffer buf))))
      ;; Delete Doom workspace
      (ignore-errors (+workspace-kill ws-name)))))

;;; Commands

(defun claude-dashboard-select ()
  "Select entry at point (RET action)."
  (interactive)
  (let ((type (get-text-property (point) 'claude-dashboard-entry-type))
        (data (get-text-property (point) 'claude-dashboard-entry-data))
        (action (get-text-property (point) 'claude-dashboard-action)))
    (cond
     ;; Button with explicit action
     (action
      (claude-dashboard--dispatch-action action data))
     ;; Repo header → open home
     ((eq type 'repo)
      (claude-dashboard--open-home data))
     ;; Worktree → jump
     ((eq type 'worktree)
      (claude-dashboard--jump-to-worktree data)))))

(defun claude-dashboard--dispatch-action (action data)
  "Dispatch ACTION with DATA."
  (pcase action
    ('open-home (claude-dashboard--open-home data))
    ('jump-to-worktree (claude-dashboard--jump-to-worktree data))
    ('create-worktree (claude-dashboard--create-worktree data))
    ('close-worktree (claude-dashboard--close-worktree data))))

(defun claude-dashboard-create ()
  "Create a new worktree in the repo at point."
  (interactive)
  (when-let ((data (claude-dashboard--repo-at-point)))
    (claude-dashboard--create-worktree data)))

(defun claude-dashboard-sync ()
  "Sync the worktree at point."
  (interactive)
  (when-let ((data (claude-dashboard--worktree-at-point)))
    (claude-dashboard--sync-worktree data)))

(defun claude-dashboard-close ()
  "Close the worktree at point."
  (interactive)
  (when-let ((data (claude-dashboard--worktree-at-point)))
    (claude-dashboard--close-worktree data)))

(defun claude-dashboard-add-repo ()
  "Add a repo to the registry."
  (interactive)
  (claude-dashboard--add-repo))

(defun claude-dashboard-remove-repo ()
  "Remove the repo at point."
  (interactive)
  (when-let ((data (claude-dashboard--repo-at-point)))
    (claude-dashboard--remove-repo data)))

(defun claude-dashboard-quit ()
  "Bury the dashboard buffer."
  (interactive)
  (bury-buffer))

(defun claude-dashboard-help ()
  "Show dashboard help."
  (interactive)
  (message "RET:jump  c:new  s:sync  x:close  a:add  r:remove  n/p:nav  TAB:fold  g:refresh  q:quit"))

;;; Context Helpers

(defun claude-dashboard--repo-at-point ()
  "Get repo data at point, walking up to repo header if on a child entry."
  (let ((type (get-text-property (point) 'claude-dashboard-entry-type))
        (data (get-text-property (point) 'claude-dashboard-entry-data)))
    (cond
     ((eq type 'repo) data)
     ((memq type '(worktree button subrepo))
      ;; Extract repo name from entry data
      (when-let ((name (or (plist-get data :repo-name)
                           (plist-get data :name))))
        (list :name name :path (plist-get data :path))))
     (t nil))))

(defun claude-dashboard--worktree-at-point ()
  "Get worktree data at point."
  (let ((type (get-text-property (point) 'claude-dashboard-entry-type))
        (data (get-text-property (point) 'claude-dashboard-entry-data)))
    (when (eq type 'worktree)
      data)))

;;; Action Implementations

(defun claude-dashboard--open-home (data)
  "Open or switch to the home workspace for repo DATA."
  (let* ((name (plist-get data :name))
         (path (plist-get data :path))
         (ws-name (format "%s:home" name))
         (buf-name (format "*claude:%s:home*" name)))
    ;; First-time setup
    (unless (+workspace-exists-p ws-name)
      (+workspace/new ws-name)
      (when (and path (file-directory-p path))
        (setq default-directory (file-name-as-directory path))
        ;; Treemacs scoped to this project exclusively
        (when (fboundp 'treemacs-add-and-display-current-project-exclusively)
          (treemacs-add-and-display-current-project-exclusively))
        ;; vterm with Claude session
        (claude-dashboard--create-vterm buf-name path)
        ;; Start attention monitor
        (claude-monitor-start)))
    ;; Switch to workspace (handles both new and existing)
    (+workspace/switch-to ws-name)
    ;; Focus vterm if it exists
    (when-let ((buf (get-buffer buf-name)))
      (switch-to-buffer buf))))

(defun claude-dashboard--jump-to-worktree (data)
  "Jump to the worktree workspace for DATA."
  (let* ((repo-name (plist-get data :repo-name))
         (branch (plist-get data :branch))
         (root (plist-get data :root))
         (ws-name (format "%s:%s" repo-name branch))
         (buf-name (format "*claude:%s:%s*" repo-name branch)))
    ;; Create workspace if it doesn't exist
    (unless (+workspace-exists-p ws-name)
      (+workspace/new ws-name)
      ;; Set directory and create vterm if worktree exists
      (when (and root (file-directory-p root))
        (setq default-directory (file-name-as-directory root))
        ;; Treemacs scoped to worktree root
        (when (fboundp 'treemacs-add-and-display-current-project-exclusively)
          (treemacs-add-and-display-current-project-exclusively))
        ;; Create vterm buffer with Claude
        (claude-dashboard--create-vterm buf-name root)))
    (+workspace/switch-to ws-name)
    ;; Focus vterm buffer if it exists
    (when-let ((buf (get-buffer buf-name)))
      (switch-to-buffer buf))))

(defun claude-dashboard--trellis-ready-plans (repo-path)
  "Query trellis for ready plans in REPO-PATH.
Runs trellis from REPO-PATH so it finds the .trellis config.
Returns list of plists (:id ... :title ...) or nil."
  (when (and repo-path (file-directory-p repo-path))
    (condition-case nil
        (let ((buf (generate-new-buffer " *trellis*"))
              (default-directory (file-name-as-directory repo-path)))
          (unwind-protect
              (let ((exit-code
                     (call-process "trellis" nil buf nil
                                   "ready" "--json")))
                (when (= exit-code 0)
                  (with-current-buffer buf
                    (goto-char (point-min))
                    (let ((json-array-type 'list)
                          (json-object-type 'plist)
                          (json-key-type 'keyword))
                      (condition-case nil
                          (let ((plans (json-read)))
                            (when (and (listp plans) plans)
                              plans))
                        (error nil))))))
            (kill-buffer buf)))
      (error nil))))

(defun claude-dashboard--create-worktree (data)
  "Create a new worktree in the repo described by DATA."
  (let* ((name (plist-get data :name))
         (path (plist-get data :path))
         (plans (claude-dashboard--trellis-ready-plans path))
         (branch
          (if plans
              (let* ((candidates
                      (mapcar (lambda (p)
                               (format "%s — %s"
                                       (plist-get p :id)
                                       (or (plist-get p :title) "")))
                             plans))
                     (choice (completing-read
                              (format "Plan or branch (in %s): " name)
                              candidates nil nil)))
                (cond
                 ((string-empty-p choice)
                  (user-error "Branch name cannot be empty"))
                 ;; Matched a plan candidate — extract the ID
                 ((member choice candidates)
                  (car (split-string choice " — ")))
                 ;; Free-form input — use as branch name directly
                 (t choice)))
            (read-string (format "Branch name (in %s): " name)))))
    (when (string-empty-p branch)
      (user-error "Branch name cannot be empty"))
    (message "Grove: creating %s/%s..." name branch)
    (claude-grove-workspace-create
     path branch
     (lambda (ok ws-data error-msg)
       (if (not ok)
           (message "Grove: create failed — %s" error-msg)
         ;; Success: create Doom workspace + vterm
         (let* ((ws-name (format "%s:%s" name branch))
                (buf-name (format "*claude:%s:%s*" name branch))
                (root (plist-get ws-data :root)))
           (+workspace/new ws-name)
           (+workspace/switch-to ws-name)
           (when (and root (file-directory-p root))
             (setq default-directory (file-name-as-directory root))
             ;; Treemacs scoped to worktree root
             (when (fboundp 'treemacs-add-and-display-current-project-exclusively)
               (treemacs-add-and-display-current-project-exclusively))
             (claude-dashboard--create-vterm buf-name root)
             (switch-to-buffer buf-name))
           ;; Start monitor
           (claude-monitor-start)
           (message "Grove: created %s" ws-name)))
       ;; Refresh dashboard
       (when-let ((buf (get-buffer "*claude:dashboard*")))
         (with-current-buffer buf
           (claude-dashboard-refresh)))))))

(defun claude-dashboard--sync-worktree (data)
  "Sync the worktree described by DATA."
  (let ((branch (plist-get data :branch))
        (repo-name (plist-get data :repo-name)))
    (message "Grove: syncing %s/%s..." repo-name branch)
    (claude-grove-workspace-sync
     branch
     (lambda (ok _data error-msg)
       (if ok
           (message "Grove: synced %s/%s" repo-name branch)
         (message "Grove: sync failed — %s" error-msg))
       ;; Refresh dashboard
       (when-let ((buf (get-buffer "*claude:dashboard*")))
         (with-current-buffer buf
           (claude-dashboard-refresh)))))))

(defun claude-dashboard--close-worktree (data)
  "Close the worktree described by DATA."
  (let ((branch (plist-get data :branch))
        (repo-name (plist-get data :repo-name))
        (root (plist-get data :root)))
    (let ((mode (read-char-choice
                 (format "Close %s/%s — [m]erge or [d]iscard? " repo-name branch)
                 '(?m ?d))))
      (let ((close-mode (if (= mode ?m) 'merge 'discard)))
        (message "Grove: closing %s/%s (%s)..." repo-name branch close-mode)
        (claude-grove-workspace-close
         branch close-mode
         (lambda (ok _data error-msg)
           (if (not ok)
               (message "Grove: close failed — %s" error-msg)
             ;; Success: kill vterm + Doom workspace
             (let ((ws-name (format "%s:%s" repo-name branch))
                   (buf-name (format "*claude:%s:%s*" repo-name branch)))
               ;; Kill vterm buffer
               (when-let ((buf (get-buffer buf-name)))
                 (let ((kill-buffer-query-functions nil))
                   (kill-buffer buf)))
               ;; Delete Doom workspace
               (when (+workspace-exists-p ws-name)
                 (+workspace-kill ws-name))
               ;; Remove from projectile known projects
               (when root
                 (claude-dashboard--remove-project root))
               (message "Grove: closed %s/%s" repo-name branch)))
           ;; Refresh dashboard
           (when-let ((buf (get-buffer "*claude:dashboard*")))
             (with-current-buffer buf
               (claude-dashboard-refresh)))))))))

(defun claude-dashboard--add-repo ()
  "Prompt for a directory and add it to the repo registry."
  (let ((path (read-directory-name "Add repo: ")))
    (when (and path (not (string-empty-p path)))
      (message "Grove: adding %s..." path)
      (claude-grove-repo-add
       (expand-file-name path)
       (lambda (ok data error-msg)
         (if ok
             (message "Grove: added %s" (plist-get data :name))
           (message "Grove: add failed — %s" error-msg))
         ;; Refresh dashboard
         (when-let ((buf (get-buffer "*claude:dashboard*")))
           (with-current-buffer buf
             (claude-dashboard-refresh))))))))

(defun claude-dashboard--remove-repo (data)
  "Remove the repo described by DATA from the registry."
  (let ((name (plist-get data :name)))
    (when (y-or-n-p (format "Remove %s from registry? " name))
      (message "Grove: removing %s..." name)
      (claude-grove-repo-remove
       name
       (lambda (ok _data error-msg)
         (if ok
             (message "Grove: removed %s" name)
           (message "Grove: remove failed — %s" error-msg))
         ;; Refresh dashboard
         (when-let ((buf (get-buffer "*claude:dashboard*")))
           (with-current-buffer buf
             (claude-dashboard-refresh))))))))

;;; vterm Helper

(defun claude-dashboard--create-vterm (buffer-name dir)
  "Create a vterm buffer named BUFFER-NAME in directory DIR and start Claude."
  (let ((default-directory (file-name-as-directory (expand-file-name dir))))
    (with-current-buffer (vterm buffer-name)
      (vterm-send-string (format "cd %s && clear\n" (shell-quote-argument dir)))
      (vterm-send-string "claude\n")
      (current-buffer))))

;;; Mouse Support

(defun claude-dashboard-mouse-select (event)
  "Handle mouse click EVENT in dashboard."
  (interactive "e")
  (mouse-set-point event)
  (claude-dashboard-select))

(define-key claude-dashboard-mode-map [mouse-1] #'claude-dashboard-mouse-select)

;;; Attention Change Handler

(defun claude-dashboard--on-attention-change (_ws-name _new-status)
  "Refresh dashboard when attention status changes."
  (when-let ((buf (get-buffer "*claude:dashboard*")))
    (with-current-buffer buf
      (when (eq major-mode 'claude-dashboard-mode)
        (claude-dashboard--paint)))))

(add-hook 'claude-attention-change-hook #'claude-dashboard--on-attention-change)

(provide 'claude-dashboard)
;;; claude-dashboard.el ends here
