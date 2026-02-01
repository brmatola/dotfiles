;;; claude-dashboard.el --- Dashboard UI -*- lexical-binding: t; -*-

;;; Commentary:
;; Dashboard buffer for viewing and managing all Claude workspaces.

;;; Code:

;; Forward declarations
(declare-function claude-workspace-list "claude-workspace")
(declare-function claude-workspace-switch "claude-workspace")
(declare-function claude-buffer-name "claude-workspace")
(declare-function claude-parse-workspace-name "claude-workspace")
(declare-function claude-home-workspace-p "claude-workspace")
(declare-function claude-create-workspace "claude-workspace")
(declare-function claude-close-workspace "claude-cleanup")

;; Buffer-local variable from claude-monitor
(defvar claude--needs-attention)

;; Faces from claude-monitor
(defvar claude-attention-face)
(defvar claude-idle-face)

(defvar claude-dashboard-buffer-name "*Claude Dashboard*"
  "Name of the Claude dashboard buffer.")

(defvar claude-dashboard-filter nil
  "Current repo filter, or nil for all repos.")

(defvar claude-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "j" #'claude-dashboard-next)
    (define-key map "k" #'claude-dashboard-prev)
    (define-key map (kbd "RET") #'claude-dashboard-select)
    (define-key map "c" #'claude-create-workspace)
    (define-key map "x" #'claude-dashboard-close)
    (define-key map "g" #'claude-dashboard-refresh)
    (define-key map "/" #'claude-dashboard-filter)
    (define-key map "q" #'quit-window)
    map)
  "Keymap for Claude dashboard mode.")

(define-derived-mode claude-dashboard-mode special-mode "Claude"
  "Major mode for Claude workspace dashboard.

\\{claude-dashboard-mode-map}")

(defun claude-dashboard--get-workspaces ()
  "Get list of workspaces, optionally filtered.
Home workspaces are sorted to the top."
  (let ((workspaces (claude-workspace-list)))
    ;; Apply filter if set
    (when claude-dashboard-filter
      (setq workspaces
            (seq-filter (lambda (ws)
                          (string-prefix-p (concat claude-dashboard-filter ":")
                                           ws))
                        workspaces)))
    ;; Sort: home workspaces first, then alphabetically within each group
    (sort workspaces
          (lambda (a b)
            (let ((a-home (claude-home-workspace-p a))
                  (b-home (claude-home-workspace-p b)))
              (cond
               ;; Both home or both not home: sort alphabetically
               ((eq (not a-home) (not b-home))
                (string< a b))
               ;; a is home, b is not: a comes first
               (a-home t)
               ;; b is home, a is not: b comes first
               (t nil)))))))

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
          (let* ((parsed (claude-parse-workspace-name ws))
                 (repo-name (car parsed))
                 (branch-name (cdr parsed))
                 (buffer-name (claude-buffer-name repo-name branch-name))
                 (buffer (get-buffer buffer-name))
                 (needs-attention (and buffer
                                       (buffer-local-value 'claude--needs-attention buffer)))
                 (is-home (claude-home-workspace-p ws))
                 ;; Display name: "⌂ repo" for home, "  repo:branch" for worktree
                 (display-name (if is-home
                                   (format "⌂ %s" repo-name)
                                 (format "  %s" ws))))
            (insert (propertize (if needs-attention "● " "○ ")
                                'face (if needs-attention
                                          'claude-attention-face
                                        'claude-idle-face))
                    (propertize display-name 'face 'default
                                'claude-workspace ws)
                    "\n")))
      (insert (propertize "No Claude workspaces active.\n"
                          'face 'font-lock-comment-face)
              "\nPress 'c' to create a new workspace."))
    (goto-char (point-min))
    (forward-line 2)))

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
    (claude-workspace-switch ws)
    (quit-window)))

(defun claude-dashboard-close ()
  "Close workspace under cursor."
  (interactive)
  (when-let ((ws (claude-dashboard--current-workspace)))
    (claude-close-workspace ws)
    (claude-dashboard-refresh)))

(defun claude-dashboard-filter ()
  "Toggle filter by repo."
  (interactive)
  (if claude-dashboard-filter
      (setq claude-dashboard-filter nil)
    (let* ((workspaces (claude-workspace-list))
           (repos (delete-dups
                   (mapcar (lambda (ws)
                             (car (claude-parse-workspace-name ws)))
                           workspaces))))
      (when repos
        (setq claude-dashboard-filter
              (completing-read "Filter by repo: " repos)))))
  (claude-dashboard-refresh))

(provide 'claude-dashboard)
;;; claude-dashboard.el ends here
