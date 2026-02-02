;;; claude-dashboard.el --- Dashboard UI -*- lexical-binding: t; -*-

;;; Commentary:
;; Dashboard buffer for viewing and managing all Claude workspaces.
;; Now state-aware: shows all states, auto-refreshes on state changes.

;;; Code:

(require 'claude-state)

;; Forward declarations
(declare-function claude-workspace-switch "claude-workspace")
(declare-function claude-workspace-list "claude-workspace")
(declare-function claude-close-workspace "claude-cleanup")
(declare-function claude-force-cleanup "claude-cleanup")
(declare-function claude-create-workspace "claude-workspace")

;; Buffer-local variable from claude-monitor
(defvar claude--needs-attention)

;; Faces from claude-monitor
(defvar claude-attention-face)
(defvar claude-idle-face)

(defvar claude-dashboard-buffer-name "*Claude Dashboard*"
  "Name of the Claude dashboard buffer.")

(defvar claude-dashboard-filter nil
  "Current repo filter, or nil for all repos.")

;;; State Symbols for Display

(defconst claude-dashboard--state-symbols
  '((creating . "○")    ; Empty circle - in progress
    (active . "●")      ; Filled circle - ready
    (closing . "◐")     ; Half circle - closing
    (failed . "✗")      ; X mark - failed
    (broken . "⚠")      ; Warning - broken
    (stuck . "⊗"))      ; Circled X - stuck
  "Symbols for workspace states.")

(defconst claude-dashboard--state-faces
  '((creating . font-lock-comment-face)
    (active . default)
    (closing . font-lock-comment-face)
    (failed . error)
    (broken . warning)
    (stuck . error))
  "Faces for workspace states.")

;;; Dashboard Mode

(defvar claude-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "j" #'claude-dashboard-next)
    (define-key map "k" #'claude-dashboard-prev)
    (define-key map (kbd "RET") #'claude-dashboard-select)
    (define-key map "c" #'claude-create-workspace)
    (define-key map "x" #'claude-dashboard-close)
    (define-key map "r" #'claude-dashboard-repair)
    (define-key map "g" #'claude-dashboard-refresh)
    (define-key map "/" #'claude-dashboard-filter)
    (define-key map "q" #'quit-window)
    map)
  "Keymap for Claude dashboard mode.")

(define-derived-mode claude-dashboard-mode special-mode "Claude"
  "Major mode for Claude workspace dashboard.

\\{claude-dashboard-mode-map}")

;; Evil bindings for dashboard
(with-eval-after-load 'evil
  (add-hook 'claude-dashboard-mode-hook
            (lambda ()
              (when (fboundp 'evil-local-set-key)
                (evil-local-set-key 'normal (kbd "j") #'claude-dashboard-next)
                (evil-local-set-key 'normal (kbd "k") #'claude-dashboard-prev)
                (evil-local-set-key 'normal (kbd "RET") #'claude-dashboard-select)
                (evil-local-set-key 'normal (kbd "c") #'claude-create-workspace)
                (evil-local-set-key 'normal (kbd "x") #'claude-dashboard-close)
                (evil-local-set-key 'normal (kbd "r") #'claude-dashboard-repair)
                (evil-local-set-key 'normal (kbd "g") #'claude-dashboard-refresh)
                (evil-local-set-key 'normal (kbd "/") #'claude-dashboard-filter)
                (evil-local-set-key 'normal (kbd "q") #'quit-window)))))

;;; Workspace Data Collection

(defun claude-dashboard--get-workspaces ()
  "Get list of workspace data for display.
Returns list of plists with :name :repo :branch :status :attention :is-home :phase."
  (let ((result nil))
    (dolist (ws (claude--list-all-workspaces))
      (let* ((repo-name (car ws))
             (branch-name (cdr ws))
             (metadata (claude-metadata-read repo-name branch-name))
             (status (or (plist-get metadata :status) "active"))
             (ws-name (claude--workspace-name repo-name branch-name))
             (buffer-name (claude--buffer-name repo-name branch-name))
             (buffer (get-buffer buffer-name))
             (attention (and buffer
                             (buffer-local-value 'claude--needs-attention buffer)))
             (is-home (claude--home-workspace-p metadata))
             (phase (claude--workflow-phase repo-name branch-name)))
        ;; Apply filter if set
        (when (or (null claude-dashboard-filter)
                  (string= claude-dashboard-filter repo-name))
          (push (list :name ws-name
                      :repo repo-name
                      :branch branch-name
                      :status (intern status)
                      :attention attention
                      :is-home is-home
                      :phase phase)
                result))))
    ;; Sort: home workspaces first, then by name
    (sort result
          (lambda (a b)
            (let ((a-home (plist-get a :is-home))
                  (b-home (plist-get b :is-home)))
              (cond
               ((and a-home (not b-home)) t)
               ((and (not a-home) b-home) nil)
               (t (string< (plist-get a :name) (plist-get b :name)))))))))

;;; Rendering

(defun claude-dashboard--render ()
  "Render dashboard buffer contents."
  (let ((inhibit-read-only t)
        (workspaces (claude-dashboard--get-workspaces)))
    (erase-buffer)
    (insert (propertize "Claude Workspaces"
                        'face 'doom-modeline-buffer-major-mode)
            "\n")
    (when claude-dashboard-filter
      (insert (propertize (format "Filtered: %s" claude-dashboard-filter)
                          'face 'font-lock-comment-face)
              "\n"))
    (insert "\n")
    (if workspaces
        (dolist (ws workspaces)
          (let* ((status (plist-get ws :status))
                 (attention (plist-get ws :attention))
                 (is-home (plist-get ws :is-home))
                 (phase (plist-get ws :phase))
                 (phase-str (if phase (format " [%s]" phase) ""))
                 (symbol (cdr (assq status claude-dashboard--state-symbols)))
                 (face (cdr (assq status claude-dashboard--state-faces)))
                 ;; Override face for active with attention
                 (face (if (and (eq status 'active) attention)
                           'claude-attention-face
                         face))
                 ;; Override symbol for active with attention
                 (symbol (if (and (eq status 'active) attention)
                             "●"
                           (or symbol "?")))
                 ;; Display name
                 (display-name (if is-home
                                   (format "⌂ %s" (plist-get ws :repo))
                                 (format "  %s" (plist-get ws :name)))))
            (insert (propertize (format "%s " symbol)
                                'face (if (and (eq status 'active) attention)
                                          'claude-attention-face
                                        (or face 'claude-idle-face)))
                    (propertize display-name 'face face
                                'claude-workspace (plist-get ws :name))
                    ;; Show phase if present
                    (propertize phase-str 'face 'font-lock-type-face)
                    ;; Show status for non-active
                    (if (eq status 'active)
                        ""
                      (propertize (format " [%s]" status)
                                  'face 'font-lock-comment-face))
                    "\n")))
      (insert (propertize "No Claude workspaces active.\n"
                          'face 'font-lock-comment-face)
              "\nPress 'c' to create a new workspace."))
    (goto-char (point-min))
    (forward-line 2)))

;;; Commands

;;;###autoload
(defun claude-dashboard ()
  "Open Claude workspace dashboard."
  (interactive)
  (let ((buffer (get-buffer-create claude-dashboard-buffer-name)))
    (with-current-buffer buffer
      (claude-dashboard-mode)
      (claude-dashboard--render))
    (switch-to-buffer buffer)))

(defun claude-dashboard-refresh ()
  "Refresh dashboard contents."
  (interactive)
  (when (eq major-mode 'claude-dashboard-mode)
    (claude-dashboard--render)))

(defun claude-dashboard--current-workspace ()
  "Get workspace name at point."
  (get-text-property (point) 'claude-workspace))

(defun claude-dashboard-next ()
  "Move to next entry."
  (interactive)
  (forward-line 1)
  (while (and (not (eobp))
              (not (claude-dashboard--current-workspace)))
    (forward-line 1)))

(defun claude-dashboard-prev ()
  "Move to previous entry."
  (interactive)
  (forward-line -1)
  (while (and (not (bobp))
              (not (claude-dashboard--current-workspace)))
    (forward-line -1)))

(defun claude-dashboard-select ()
  "Jump to workspace under cursor."
  (interactive)
  (when-let ((ws (claude-dashboard--current-workspace)))
    (let* ((parsed (claude--parse-workspace-name ws))
           (metadata (and parsed (claude-metadata-read (car parsed) (cdr parsed))))
           (status (and metadata (plist-get metadata :status))))
      (if (equal status "active")
          (progn
            (claude-workspace-switch ws)
            (quit-window))
        (message "Cannot switch to %s workspace. Status: %s" ws status)))))

(defun claude-dashboard-close ()
  "Close workspace under cursor."
  (interactive)
  (when-let ((ws (claude-dashboard--current-workspace)))
    (let* ((parsed (claude--parse-workspace-name ws))
           (metadata (and parsed (claude-metadata-read (car parsed) (cdr parsed))))
           (status (and metadata (intern (plist-get metadata :status)))))
      (cond
       ((memq status '(broken stuck failed))
        ;; Use force cleanup for broken/stuck/failed
        (claude-force-cleanup ws)
        (claude-dashboard-refresh))
       ((eq status 'active)
        (claude-close-workspace ws)
        (claude-dashboard-refresh))
       (t
        (message "Cannot close workspace in %s state" status))))))

(defun claude-dashboard-repair ()
  "Repair workspace under cursor."
  (interactive)
  (when-let ((ws (claude-dashboard--current-workspace)))
    (let* ((parsed (claude--parse-workspace-name ws))
           (metadata (and parsed (claude-metadata-read (car parsed) (cdr parsed))))
           (status (and metadata (plist-get metadata :status))))
      (if (equal status "broken")
          (progn
            ;; Try to reconcile
            (require 'claude-reconcile)
            (let ((result (claude--reconcile-safe metadata)))
              (if (eq result 'active)
                  (message "Workspace %s repaired" ws)
                (message "Could not repair %s (result: %s)" ws result)))
            (claude-dashboard-refresh))
        (message "Only broken workspaces can be repaired (current: %s)" status)))))

(defun claude-dashboard-filter ()
  "Toggle filter by repo."
  (interactive)
  (if claude-dashboard-filter
      (setq claude-dashboard-filter nil)
    (let* ((workspaces (claude--list-all-workspaces))
           (repos (delete-dups (mapcar #'car workspaces))))
      (when repos
        (setq claude-dashboard-filter
              (completing-read "Filter by repo: " repos)))))
  (claude-dashboard-refresh))

;;; State Change Handler for Auto-Refresh

(defun claude-dashboard--on-state-change (_workspace-name _old-status _new-status)
  "Auto-refresh dashboard when state changes."
  (when-let ((buffer (get-buffer claude-dashboard-buffer-name)))
    (with-current-buffer buffer
      (when (eq major-mode 'claude-dashboard-mode)
        (claude-dashboard--render)))))

;; Subscribe to state changes
(add-hook 'claude-state-change-hook #'claude-dashboard--on-state-change)

(provide 'claude-dashboard)
;;; claude-dashboard.el ends here
