;;; claude-dashboard-test.el --- Tests for dashboard rendering -*- lexical-binding: t; -*-

(require 'ert)
(add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name)))

;; Stub Doom workspace functions before loading dashboard
(unless (fboundp '+workspace-exists-p)
  (defun +workspace-exists-p (_name) nil))
(unless (fboundp '+workspace/new)
  (defun +workspace/new (_name) nil))
(unless (fboundp '+workspace/switch-to)
  (defun +workspace/switch-to (_name) nil))
(unless (fboundp '+workspace-kill)
  (defun +workspace-kill (_name) nil))
(unless (fboundp '+workspace-list-names)
  (defun +workspace-list-names () nil))

;; Stub vterm
(unless (fboundp 'vterm)
  (defun vterm (_name) (current-buffer)))
(unless (fboundp 'vterm-send-string)
  (defun vterm-send-string (_str) nil))

(require 'claude-grove)
(require 'claude-monitor)
(require 'claude-dashboard)

;;; Test Helpers

(defun claude-dashboard-test--make-grove-data (&rest repos)
  "Create mock grove data with REPOS."
  (list :repos (vconcat repos)))

(defun claude-dashboard-test--make-repo (name path &rest workspaces)
  "Create a mock repo entry."
  (list :name name
        :path path
        :exists t
        :workspaces (when workspaces (vconcat workspaces))))

(defun claude-dashboard-test--make-workspace (id branch status &optional root)
  "Create a mock workspace entry."
  (list :id id
        :branch branch
        :status status
        :root (or root (format "/tmp/worktrees/%s" branch))
        :repoCount 1))

;;; Rendering Tests

(ert-deftest claude-dashboard-test-paint-empty ()
  "Test rendering with no repos."
  (with-temp-buffer
    (claude-dashboard-mode)
    (setq claude-dashboard--grove-cache (list :repos []))
    (claude-dashboard--paint)
    (should (string-match-p "Claude Workspaces" (buffer-string)))
    (should (string-match-p "No repos registered" (buffer-string)))))

(ert-deftest claude-dashboard-test-paint-repo-no-workspaces ()
  "Test rendering a repo with no active workspaces."
  (with-temp-buffer
    (claude-dashboard-mode)
    (setq claude-dashboard--grove-cache
          (claude-dashboard-test--make-grove-data
           (claude-dashboard-test--make-repo "dotfiles" "/tmp/dotfiles")))
    (claude-dashboard--paint)
    (should (string-match-p "dotfiles" (buffer-string)))
    (should (string-match-p "no active workspaces" (buffer-string)))
    (should (string-match-p "New Worktree" (buffer-string)))))

(ert-deftest claude-dashboard-test-home-attention-indicator ()
  "Test that repo header shows attention status for home session."
  (with-temp-buffer
    (claude-dashboard-mode)
    (let ((claude--workspace-states (make-hash-table :test 'equal)))
      (setq claude-dashboard--grove-cache
            (claude-dashboard-test--make-grove-data
             (claude-dashboard-test--make-repo "myrepo" "/tmp/myrepo")))
      ;; Stub repo-branch to return "main"
      (cl-letf (((symbol-function 'claude-dashboard--repo-branch)
                 (lambda (_path) "main")))
        ;; No SAP session — no status indicator
        (claude-dashboard--paint)
        (should-not (string-match-p "working\\|waiting\\|ready" (buffer-string)))
        ;; Active session → working
        (claude--set-workspace-state "myrepo:main"
                                     (list :workspace "myrepo:main" :state "active")
                                     'active)
        (claude-dashboard--paint)
        (should (string-match-p "working" (buffer-string)))
        ;; Attention session → waiting
        (claude--set-workspace-state "myrepo:main"
                                     (list :workspace "myrepo:main" :state "attention")
                                     'attention)
        (claude-dashboard--paint)
        (should (string-match-p "waiting" (buffer-string)))))))

(ert-deftest claude-dashboard-test-paint-repo-with-workspaces ()
  "Test rendering a repo with active workspaces."
  (with-temp-buffer
    (claude-dashboard-mode)
    (let ((claude--workspace-states (make-hash-table :test 'equal)))
      (setq claude-dashboard--grove-cache
            (claude-dashboard-test--make-grove-data
             (claude-dashboard-test--make-repo
              "dotfiles" "/tmp/dotfiles"
              (claude-dashboard-test--make-workspace "dotfiles-fix-zsh" "fix-zsh" "active")
              (claude-dashboard-test--make-workspace "dotfiles-add-pkg" "add-pkg" "active"))))
      ;; Give one workspace an active SAP session
      (claude--set-workspace-state "dotfiles:fix-zsh"
                                   (list :workspace "dotfiles:fix-zsh" :state "active")
                                   'active)
      (claude-dashboard--paint)
      (should (string-match-p "dotfiles" (buffer-string)))
      (should (string-match-p "fix-zsh" (buffer-string)))
      (should (string-match-p "add-pkg" (buffer-string)))
      (should (string-match-p "working" (buffer-string))))))

(ert-deftest claude-dashboard-test-paint-lifecycle-status ()
  "Test rendering workspaces in lifecycle states."
  (with-temp-buffer
    (claude-dashboard-mode)
    (setq claude-dashboard--grove-cache
          (claude-dashboard-test--make-grove-data
           (claude-dashboard-test--make-repo
            "myrepo" "/tmp/myrepo"
            (claude-dashboard-test--make-workspace "myrepo-feat" "feat" "creating")
            (claude-dashboard-test--make-workspace "myrepo-old" "old" "failed"))))
    (claude-dashboard--paint)
    (should (string-match-p "creating" (buffer-string)))
    (should (string-match-p "failed" (buffer-string)))))

(ert-deftest claude-dashboard-test-error-rendering ()
  "Test error state rendering."
  (with-temp-buffer
    (claude-dashboard-mode)
    (claude-dashboard--render-error "grove not found")
    (should (string-match-p "grove not found" (buffer-string)))
    (should (string-match-p "Install grove" (buffer-string)))))

(ert-deftest claude-dashboard-test-text-properties ()
  "Test that text properties are set correctly on entries."
  (with-temp-buffer
    (claude-dashboard-mode)
    (setq claude-dashboard--grove-cache
          (claude-dashboard-test--make-grove-data
           (claude-dashboard-test--make-repo
            "dotfiles" "/tmp/dotfiles"
            (claude-dashboard-test--make-workspace "dotfiles-fix" "fix" "active"))))
    (claude-dashboard--paint)
    ;; Find the repo header
    (goto-char (point-min))
    (search-forward "dotfiles")
    (should (eq (get-text-property (point) 'claude-dashboard-entry-type) 'repo))
    ;; Find the worktree entry
    (search-forward "fix")
    (should (eq (get-text-property (point) 'claude-dashboard-entry-type) 'worktree))
    ;; Check entry data
    (let ((data (get-text-property (point) 'claude-dashboard-entry-data)))
      (should (equal (plist-get data :branch) "fix"))
      (should (equal (plist-get data :repo-name) "dotfiles")))))

(ert-deftest claude-dashboard-test-collapse ()
  "Test collapse/expand state."
  (with-temp-buffer
    (claude-dashboard-mode)
    (setq claude-dashboard--grove-cache
          (claude-dashboard-test--make-grove-data
           (claude-dashboard-test--make-repo
            "dotfiles" "/tmp/dotfiles"
            (claude-dashboard-test--make-workspace "dotfiles-fix" "fix" "active"))))
    ;; Not collapsed — should show branch
    (claude-dashboard--paint)
    (should (string-match-p "fix" (buffer-string)))
    ;; Collapse
    (push "dotfiles" claude-dashboard--collapsed-repos)
    (claude-dashboard--paint)
    ;; Should show collapsed indicator instead of branch
    (should (string-match-p "1 workspaces" (buffer-string)))))

(ert-deftest claude-dashboard-test-footer ()
  "Test footer hint bar is in mode-line."
  (with-temp-buffer
    (claude-dashboard-mode)
    ;; mode-line-format is a list of strings; join and check
    (let ((ml-str (mapconcat #'identity mode-line-format "")))
      (should (string-match-p "c:new" ml-str))
      (should (string-match-p "TAB" ml-str)))))

(ert-deftest claude-dashboard-test-multiple-repos ()
  "Test rendering multiple repos."
  (with-temp-buffer
    (claude-dashboard-mode)
    (setq claude-dashboard--grove-cache
          (claude-dashboard-test--make-grove-data
           (claude-dashboard-test--make-repo "dotfiles" "/tmp/dotfiles")
           (claude-dashboard-test--make-repo
            "acorn" "/tmp/acorn"
            (claude-dashboard-test--make-workspace "acorn-auth" "auth" "active"))))
    (claude-dashboard--paint)
    (should (string-match-p "dotfiles" (buffer-string)))
    (should (string-match-p "acorn" (buffer-string)))
    (should (string-match-p "auth" (buffer-string)))))

;;; Navigation Tests

(ert-deftest claude-dashboard-test-navigation ()
  "Test n/p navigation between entries."
  (with-temp-buffer
    (claude-dashboard-mode)
    (setq claude-dashboard--grove-cache
          (claude-dashboard-test--make-grove-data
           (claude-dashboard-test--make-repo
            "dotfiles" "/tmp/dotfiles"
            (claude-dashboard-test--make-workspace "dotfiles-fix" "fix" "active"))))
    (claude-dashboard--paint)
    (goto-char (point-min))
    ;; First next should land on repo header
    (claude-dashboard-next)
    (should (eq (get-text-property (point) 'claude-dashboard-entry-type) 'repo))
    ;; Next should land on worktree
    (claude-dashboard-next)
    (should (eq (get-text-property (point) 'claude-dashboard-entry-type) 'worktree))
    ;; Prev should go back to repo
    (claude-dashboard-prev)
    (should (eq (get-text-property (point) 'claude-dashboard-entry-type) 'repo))))

;;; Merge Workspace Status Tests

(ert-deftest claude-dashboard-test-merge-status ()
  "Test merging tier 2 status data into cache."
  (with-temp-buffer
    (claude-dashboard-mode)
    (setq claude-dashboard--grove-cache
          (claude-dashboard-test--make-grove-data
           (claude-dashboard-test--make-repo
            "acorn" "/tmp/acorn"
            (claude-dashboard-test--make-workspace "acorn-auth" "auth" "active"))))
    ;; Merge tier 2 data
    (claude-dashboard--merge-workspace-status
     (list :id "acorn-auth"
           :status "active"
           :branch "auth"
           :repos [((:name "acorn" :role "parent" :dirty 0 :commits 3)
                    (:name "public" :role "child" :dirty 2 :commits 1))]))
    ;; Check the workspace was enriched
    (let* ((repos (plist-get claude-dashboard--grove-cache :repos))
           (ws (aref (plist-get (aref repos 0) :workspaces) 0)))
      (should (plist-get ws :total-commits)))))

;;; Orphan Cleanup Tests

(ert-deftest claude-dashboard-test-cleanup-orphans ()
  "Test that orphaned Doom workspaces are detected."
  (let ((killed-workspaces nil))
    ;; Stub +workspace-list-names to return some workspaces
    (cl-letf (((symbol-function '+workspace-list-names)
               (lambda () '("dotfiles:home" "dotfiles:fix-zsh" "dotfiles:old-branch" "acorn:home")))
              ((symbol-function '+workspace-kill)
               (lambda (name) (push name killed-workspaces)))
              ((symbol-function 'kill-buffer)
               (lambda (&rest _) nil)))
      (with-temp-buffer
        (claude-dashboard-mode)
        ;; Grove knows about dotfiles with fix-zsh, and acorn with no workspaces
        (let ((grove-data
               (claude-dashboard-test--make-grove-data
                (claude-dashboard-test--make-repo
                 "dotfiles" "/tmp/dotfiles"
                 (claude-dashboard-test--make-workspace "dotfiles-fix-zsh" "fix-zsh" "active"))
                (claude-dashboard-test--make-repo "acorn" "/tmp/acorn"))))
          (claude-dashboard--cleanup-orphans grove-data)
          ;; dotfiles:old-branch should be cleaned up (orphaned)
          (should (member "dotfiles:old-branch" killed-workspaces))
          ;; dotfiles:home and dotfiles:fix-zsh should NOT be cleaned up
          (should (not (member "dotfiles:home" killed-workspaces)))
          (should (not (member "dotfiles:fix-zsh" killed-workspaces)))
          ;; acorn:home should NOT be cleaned up
          (should (not (member "acorn:home" killed-workspaces))))))))

(ert-deftest claude-dashboard-test-cleanup-skips-transient ()
  "Test that workspaces in transient states are not cleaned up."
  (let ((killed-workspaces nil))
    (cl-letf (((symbol-function '+workspace-list-names)
               (lambda () '("dotfiles:new-feat")))
              ((symbol-function '+workspace-kill)
               (lambda (name) (push name killed-workspaces))))
      (with-temp-buffer
        (claude-dashboard-mode)
        ;; Grove has the workspace in "creating" state
        (let ((grove-data
               (claude-dashboard-test--make-grove-data
                (claude-dashboard-test--make-repo
                 "dotfiles" "/tmp/dotfiles"
                 (claude-dashboard-test--make-workspace "dotfiles-new-feat" "new-feat" "creating")))))
          (claude-dashboard--cleanup-orphans grove-data)
          ;; Should NOT be cleaned up — it's still creating
          (should (null killed-workspaces)))))))

(ert-deftest claude-dashboard-test-cleanup-namespace-guard ()
  "Test that cleanup only touches workspaces for registered repos."
  (let ((killed-workspaces nil))
    (cl-letf (((symbol-function '+workspace-list-names)
               (lambda () '("work:notes" "random:thing" "dotfiles:orphan")))
              ((symbol-function '+workspace-kill)
               (lambda (name) (push name killed-workspaces)))
              ((symbol-function 'kill-buffer)
               (lambda (&rest _) nil)))
      (with-temp-buffer
        (claude-dashboard-mode)
        ;; Only dotfiles is registered in grove
        (let ((grove-data
               (claude-dashboard-test--make-grove-data
                (claude-dashboard-test--make-repo "dotfiles" "/tmp/dotfiles"))))
          (claude-dashboard--cleanup-orphans grove-data)
          ;; dotfiles:orphan should be cleaned up
          (should (member "dotfiles:orphan" killed-workspaces))
          ;; work:notes and random:thing should NOT be touched
          (should (not (member "work:notes" killed-workspaces)))
          (should (not (member "random:thing" killed-workspaces))))))))

;;; Sort Tests

(ert-deftest claude-dashboard-test-sort-attention-first ()
  "Test that workspaces needing attention sort first."
  (let ((ws-a (list :branch "alpha" :status "active"))
        (ws-b (list :branch "beta" :status "active"))
        (claude--workspace-states (make-hash-table :test 'equal)))
    ;; Give beta an attention state
    (claude--set-workspace-state "myrepo:beta"
                                 (list :workspace "myrepo:beta" :state "attention")
                                 'attention)
    (let ((sorted (claude-dashboard--sort-workspaces
                   (list ws-a ws-b) "myrepo")))
      ;; beta (attention) should come first
      (should (equal (plist-get (car sorted) :branch) "beta"))
      (should (equal (plist-get (cadr sorted) :branch) "alpha")))))

(ert-deftest claude-dashboard-test-sort-alphabetical-tiebreak ()
  "Test that workspaces with same attention sort alphabetically."
  (let ((ws-z (list :branch "zulu" :status "active"))
        (ws-a (list :branch "alpha" :status "active")))
    (let ((sorted (claude-dashboard--sort-workspaces
                   (list ws-z ws-a) "myrepo")))
      ;; No attention on either, so alphabetical: alpha then zulu
      (should (equal (plist-get (car sorted) :branch) "alpha"))
      (should (equal (plist-get (cadr sorted) :branch) "zulu")))))

;;; Position Preservation Tests

(ert-deftest claude-dashboard-test-position-preserved ()
  "Test that cursor position is preserved across repaints."
  (with-temp-buffer
    (claude-dashboard-mode)
    (setq claude-dashboard--grove-cache
          (claude-dashboard-test--make-grove-data
           (claude-dashboard-test--make-repo
            "dotfiles" "/tmp/dotfiles"
            (claude-dashboard-test--make-workspace "dotfiles-fix" "fix" "active")
            (claude-dashboard-test--make-workspace "dotfiles-add" "add" "active"))))
    (claude-dashboard--paint)
    ;; Navigate to the second worktree
    (goto-char (point-min))
    (search-forward "add")
    (should (eq (get-text-property (point) 'claude-dashboard-entry-type) 'worktree))
    ;; Repaint should restore to same entry
    (claude-dashboard--paint)
    (should (eq (get-text-property (point) 'claude-dashboard-entry-type) 'worktree))
    (let ((data (get-text-property (point) 'claude-dashboard-entry-data)))
      (should (equal (plist-get data :branch) "add")))))

;;; Worktree Entry Data Tests

(ert-deftest claude-dashboard-test-worktree-has-repo-path ()
  "Test that worktree entry data includes the parent repo path."
  (with-temp-buffer
    (claude-dashboard-mode)
    (setq claude-dashboard--grove-cache
          (claude-dashboard-test--make-grove-data
           (claude-dashboard-test--make-repo
            "dotfiles" "/tmp/dotfiles"
            (claude-dashboard-test--make-workspace "dotfiles-fix" "fix" "active"))))
    (claude-dashboard--paint)
    ;; Find the worktree entry
    (goto-char (point-min))
    (search-forward "fix")
    (let ((data (get-text-property (point) 'claude-dashboard-entry-data)))
      ;; Should have both repo path and worktree root
      (should (equal (plist-get data :path) "/tmp/dotfiles"))
      (should (equal (plist-get data :root) "/tmp/worktrees/fix"))
      (should (equal (plist-get data :repo-name) "dotfiles")))))

;;; Status Icon Tests

(ert-deftest claude-dashboard-test-status-icons ()
  "Test that status indicators include icons."
  (with-temp-buffer
    (claude-dashboard-mode)
    (let ((claude--workspace-states (make-hash-table :test 'equal)))
      (setq claude-dashboard--grove-cache
            (claude-dashboard-test--make-grove-data
             (claude-dashboard-test--make-repo
              "myrepo" "/tmp/myrepo"
              (claude-dashboard-test--make-workspace "myrepo-feat" "feat" "active")
              (claude-dashboard-test--make-workspace "myrepo-new" "new" "creating")
              (claude-dashboard-test--make-workspace "myrepo-bad" "bad" "failed"))))
      ;; Give feat an active SAP session
      (claude--set-workspace-state "myrepo:feat"
                                   (list :workspace "myrepo:feat" :state "active")
                                   'active)
      (claude-dashboard--paint)
      (let ((content (buffer-string)))
        (should (string-match-p "⚡ " content))
        (should (string-match-p "◌" content))
        (should (string-match-p "✖" content))))))

;;; Trellis Ready Plans Tests

(ert-deftest claude-dashboard-test-trellis-ready-valid-json ()
  "Test that valid JSON from trellis returns parsed plans."
  (cl-letf (((symbol-function 'call-process)
             (lambda (_program _infile buffer &rest _args)
               (with-current-buffer buffer
                 (insert "[{\"id\":\"auth\",\"title\":\"Add auth\"},{\"id\":\"cache\",\"title\":\"Add caching\"}]"))
               0)))
    (let ((plans (claude-dashboard--trellis-ready-plans "/tmp")))
      (should (= (length plans) 2))
      (should (equal (plist-get (car plans) :id) "auth"))
      (should (equal (plist-get (car plans) :title) "Add auth"))
      (should (equal (plist-get (cadr plans) :id) "cache")))))

(ert-deftest claude-dashboard-test-trellis-ready-not-found ()
  "Test that trellis not found returns nil."
  (cl-letf (((symbol-function 'call-process)
             (lambda (_program _infile _buffer &rest _args)
               (signal 'file-missing (list "trellis" "No such file or directory")))))
    (should (null (claude-dashboard--trellis-ready-plans "/tmp")))))

(ert-deftest claude-dashboard-test-trellis-ready-empty ()
  "Test that empty results returns nil."
  (cl-letf (((symbol-function 'call-process)
             (lambda (_program _infile buffer &rest _args)
               (with-current-buffer buffer
                 (insert "[]"))
               0)))
    (let ((plans (claude-dashboard--trellis-ready-plans "/tmp")))
      ;; Empty list is still a valid list, but has no items
      (should (null plans)))))

(ert-deftest claude-dashboard-test-trellis-ready-nonzero-exit ()
  "Test that non-zero exit code returns nil."
  (cl-letf (((symbol-function 'call-process)
             (lambda (_program _infile buffer &rest _args)
               (with-current-buffer buffer
                 (insert "Error: no .trellis config"))
               1)))
    (should (null (claude-dashboard--trellis-ready-plans "/tmp")))))

;;; Open-Home Tests

(ert-deftest claude-dashboard-test-open-home-creates-vterm ()
  "Test that open-home creates vterm on first visit."
  (let ((created-vterms nil))
    (cl-letf (((symbol-function '+workspace-exists-p)
               (lambda (_) nil))
              ((symbol-function '+workspace/new) (lambda (_) nil))
              ((symbol-function '+workspace/switch-to) (lambda (_) nil))
              ((symbol-function 'file-directory-p) (lambda (_) t))
              ((symbol-function 'claude-dashboard--create-vterm)
               (lambda (name dir) (push (list name dir) created-vterms)))
              ((symbol-function 'claude-monitor-start) (lambda () nil)))
      (claude-dashboard--open-home (list :name "dotfiles" :path "/tmp/dotfiles"))
      (should (= (length created-vterms) 1))
      (should (equal (caar created-vterms) "*claude:dotfiles:home*")))))

(ert-deftest claude-dashboard-test-open-home-skips-setup-on-revisit ()
  "Test that open-home skips vterm when workspace exists."
  (let ((created-vterms nil))
    (cl-letf (((symbol-function '+workspace-exists-p)
               (lambda (_) t))
              ((symbol-function '+workspace/switch-to) (lambda (_) nil))
              ((symbol-function 'claude-dashboard--create-vterm)
               (lambda (name dir) (push (list name dir) created-vterms)))
              ((symbol-function 'claude-monitor-start) (lambda () nil)))
      (claude-dashboard--open-home (list :name "dotfiles" :path "/tmp/dotfiles"))
      (should (null created-vterms)))))

;;; Snap-to-Entry Tests

(ert-deftest claude-dashboard-test-snap-to-nearest-entry ()
  "Test that snap moves point to the nearest entry when on a blank line."
  (with-temp-buffer
    (claude-dashboard-mode)
    (setq claude-dashboard--grove-cache
          (claude-dashboard-test--make-grove-data
           (claude-dashboard-test--make-repo
            "dotfiles" "/tmp/dotfiles"
            (claude-dashboard-test--make-workspace "dotfiles-fix" "fix" "active"))))
    (claude-dashboard--paint)
    ;; Move to beginning where there's no entry (title area)
    (goto-char (point-min))
    (should-not (get-text-property (point) 'claude-dashboard-entry-type))
    ;; Snap should move us to the nearest entry
    (claude-dashboard--snap-to-entry)
    (should (get-text-property (point) 'claude-dashboard-entry-type))))

(ert-deftest claude-dashboard-test-snap-noop-on-entry ()
  "Test that snap is a no-op when already on an entry."
  (with-temp-buffer
    (claude-dashboard-mode)
    (setq claude-dashboard--grove-cache
          (claude-dashboard-test--make-grove-data
           (claude-dashboard-test--make-repo
            "dotfiles" "/tmp/dotfiles"
            (claude-dashboard-test--make-workspace "dotfiles-fix" "fix" "active"))))
    (claude-dashboard--paint)
    ;; Navigate to a known entry
    (goto-char (point-min))
    (claude-dashboard-next)
    (let ((pos (point)))
      (should (get-text-property pos 'claude-dashboard-entry-type))
      ;; Snap should not move
      (claude-dashboard--snap-to-entry)
      (should (= (point) pos)))))

;;; Two-Column Layout Tests

(ert-deftest claude-dashboard-test-render-repo-lines ()
  "Test that render-repo-lines returns propertized line strings."
  (with-temp-buffer
    (claude-dashboard-mode)
    (let* ((repo (claude-dashboard-test--make-repo "dotfiles" "/tmp/dotfiles"))
           (lines (claude-dashboard--render-repo-lines repo nil 60)))
      ;; Should return a non-empty list
      (should (listp lines))
      (should (> (length lines) 0))
      ;; Should contain repo name
      (should (cl-some (lambda (l) (string-match-p "dotfiles" l)) lines))
      ;; Should contain New Worktree button
      (should (cl-some (lambda (l) (string-match-p "New Worktree" l)) lines)))))

(ert-deftest claude-dashboard-test-render-repo-lines-with-workspaces ()
  "Test render-repo-lines includes worktree entries."
  (with-temp-buffer
    (claude-dashboard-mode)
    (let* ((repo (claude-dashboard-test--make-repo
                  "acorn" "/tmp/acorn"
                  (claude-dashboard-test--make-workspace "acorn-feat" "feat" "active")))
           (lines (claude-dashboard--render-repo-lines repo nil 60)))
      ;; Should contain branch name
      (should (cl-some (lambda (l) (string-match-p "feat" l)) lines))
      ;; Lines should have text properties
      (let ((repo-line (cl-find-if
                        (lambda (l) (string-match-p "acorn" l))
                        lines)))
        (should repo-line)
        (should (get-text-property 0 'claude-dashboard-entry-type repo-line))))))

(ert-deftest claude-dashboard-test-two-column-rendering ()
  "Test that two-column paint produces both repos side by side."
  (with-temp-buffer
    (claude-dashboard-mode)
    (let ((repos (vconcat
                  (list (claude-dashboard-test--make-repo "alpha" "/tmp/alpha")
                        (claude-dashboard-test--make-repo "beta" "/tmp/beta")))))
      (let ((inhibit-read-only t))
        (claude-dashboard--paint-two-column repos nil 140))
      (let ((content (buffer-string)))
        ;; Both repos should appear
        (should (string-match-p "alpha" content))
        (should (string-match-p "beta" content))
        ;; They should be on the same line (side by side)
        (goto-char (point-min))
        (let ((found-same-line nil))
          (while (and (not found-same-line) (not (eobp)))
            (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
              (when (and (string-match-p "alpha" line)
                         (string-match-p "beta" line))
                (setq found-same-line t)))
            (forward-line 1))
          ;; Repos should appear on the same line in two-column mode
          (should found-same-line))))))

(ert-deftest claude-dashboard-test-collect-entry-positions ()
  "Test that entry positions are correctly collected."
  (with-temp-buffer
    (claude-dashboard-mode)
    (setq claude-dashboard--grove-cache
          (claude-dashboard-test--make-grove-data
           (claude-dashboard-test--make-repo
            "dotfiles" "/tmp/dotfiles"
            (claude-dashboard-test--make-workspace "dotfiles-fix" "fix" "active"))))
    (claude-dashboard--paint)
    (let ((positions (claude-dashboard--collect-entry-positions)))
      ;; Should have at least: repo header, worktree
      (should (>= (length positions) 2))
      ;; All positions should have entry-type properties
      (dolist (pos positions)
        (should (get-text-property pos 'claude-dashboard-entry-type))))))

(ert-deftest claude-dashboard-test-navigation-position-based ()
  "Test that position-based navigation works across entries."
  (with-temp-buffer
    (claude-dashboard-mode)
    (setq claude-dashboard--grove-cache
          (claude-dashboard-test--make-grove-data
           (claude-dashboard-test--make-repo
            "dotfiles" "/tmp/dotfiles"
            (claude-dashboard-test--make-workspace "dotfiles-fix" "fix" "active"))
           (claude-dashboard-test--make-repo "acorn" "/tmp/acorn")))
    (claude-dashboard--paint)
    (goto-char (point-min))
    ;; Collect all entry types we visit
    (let (visited)
      (claude-dashboard-next)
      (push (get-text-property (point) 'claude-dashboard-entry-type) visited)
      (claude-dashboard-next)
      (push (get-text-property (point) 'claude-dashboard-entry-type) visited)
      ;; Should have visited repo and worktree (or button)
      (should (memq 'repo visited))
      ;; Going prev should go back
      (let ((pos-before (point)))
        (claude-dashboard-prev)
        (should (< (point) pos-before))))))

;;; SAP Status Rendering Tests

(ert-deftest claude-dashboard-test-nil-attention-no-indicator ()
  "Test that nil attention shows no status text."
  (with-temp-buffer
    (claude-dashboard-mode)
    (let ((claude--workspace-states (make-hash-table :test 'equal)))
      (setq claude-dashboard--grove-cache
            (claude-dashboard-test--make-grove-data
             (claude-dashboard-test--make-repo
              "myrepo" "/tmp/myrepo"
              (claude-dashboard-test--make-workspace "myrepo-feat" "feat" "active"))))
      ;; No SAP session for this workspace
      (claude-dashboard--paint)
      (should-not (string-match-p "working\\|ready\\|waiting\\|stopped"
                                  (buffer-string))))))

(ert-deftest claude-dashboard-test-idle-renders-ready ()
  "Test that idle state renders ◇ ready."
  (with-temp-buffer
    (claude-dashboard-mode)
    (let ((claude--workspace-states (make-hash-table :test 'equal)))
      (setq claude-dashboard--grove-cache
            (claude-dashboard-test--make-grove-data
             (claude-dashboard-test--make-repo
              "myrepo" "/tmp/myrepo"
              (claude-dashboard-test--make-workspace "myrepo-feat" "feat" "active"))))
      (claude--set-workspace-state "myrepo:feat"
                                   (list :workspace "myrepo:feat" :state "idle")
                                   'idle)
      (claude-dashboard--paint)
      (let ((content (buffer-string)))
        (should (string-match-p "◇" content))
        (should (string-match-p "ready" content))))))

(ert-deftest claude-dashboard-test-attention-renders-waiting ()
  "Test that attention state renders ● waiting."
  (with-temp-buffer
    (claude-dashboard-mode)
    (let ((claude--workspace-states (make-hash-table :test 'equal)))
      (setq claude-dashboard--grove-cache
            (claude-dashboard-test--make-grove-data
             (claude-dashboard-test--make-repo
              "myrepo" "/tmp/myrepo"
              (claude-dashboard-test--make-workspace "myrepo-feat" "feat" "active"))))
      (claude--set-workspace-state "myrepo:feat"
                                   (list :workspace "myrepo:feat" :state "attention")
                                   'attention)
      (claude-dashboard--paint)
      (let ((content (buffer-string)))
        (should (string-match-p "●" content))
        (should (string-match-p "waiting" content))))))

(ert-deftest claude-dashboard-test-stopped-renders-stopped ()
  "Test that stopped state renders ◌ stopped."
  (with-temp-buffer
    (claude-dashboard-mode)
    (let ((claude--workspace-states (make-hash-table :test 'equal)))
      (setq claude-dashboard--grove-cache
            (claude-dashboard-test--make-grove-data
             (claude-dashboard-test--make-repo
              "myrepo" "/tmp/myrepo"
              (claude-dashboard-test--make-workspace "myrepo-feat" "feat" "active"))))
      (claude--set-workspace-state "myrepo:feat"
                                   (list :workspace "myrepo:feat" :state "stopped")
                                   'stopped)
      (claude-dashboard--paint)
      (let ((content (buffer-string)))
        (should (string-match-p "◌" content))
        (should (string-match-p "stopped" content))))))

(ert-deftest claude-dashboard-test-home-attention-uses-branch ()
  "Test that home attention uses resolved branch, not 'home'."
  (with-temp-buffer
    (claude-dashboard-mode)
    (let ((claude--workspace-states (make-hash-table :test 'equal))
          (queried-ws nil))
      (setq claude-dashboard--grove-cache
            (claude-dashboard-test--make-grove-data
             (claude-dashboard-test--make-repo "myrepo" "/tmp/myrepo")))
      ;; Stub repo-branch to return "main"
      (cl-letf (((symbol-function 'claude-dashboard--repo-branch)
                 (lambda (_path) "main"))
                ((symbol-function 'claude-workspace-attention)
                 (lambda (ws)
                   (push ws queried-ws)
                   nil)))
        (claude-dashboard--paint)
        ;; Should have queried "myrepo:main", not "myrepo:home"
        (should (member "myrepo:main" queried-ws))
        (should-not (member "myrepo:home" queried-ws))))))

(ert-deftest claude-dashboard-test-sort-priority ()
  "Test sort priority: attention > idle > active."
  (let ((ws-active (list :branch "alpha" :status "active"))
        (ws-idle (list :branch "beta" :status "active"))
        (ws-attention (list :branch "gamma" :status "active"))
        (claude--workspace-states (make-hash-table :test 'equal)))
    (claude--set-workspace-state "myrepo:alpha"
                                 (list :workspace "myrepo:alpha") 'active)
    (claude--set-workspace-state "myrepo:beta"
                                 (list :workspace "myrepo:beta") 'idle)
    (claude--set-workspace-state "myrepo:gamma"
                                 (list :workspace "myrepo:gamma") 'attention)
    (let ((sorted (claude-dashboard--sort-workspaces
                   (list ws-active ws-idle ws-attention) "myrepo")))
      ;; attention first, then idle, then active
      (should (equal (plist-get (nth 0 sorted) :branch) "gamma"))
      (should (equal (plist-get (nth 1 sorted) :branch) "beta"))
      (should (equal (plist-get (nth 2 sorted) :branch) "alpha")))))

(ert-deftest claude-dashboard-test-resume-button-on-stopped ()
  "Test that stopped workspace shows Resume button."
  (with-temp-buffer
    (claude-dashboard-mode)
    (let ((claude--workspace-states (make-hash-table :test 'equal)))
      (setq claude-dashboard--grove-cache
            (claude-dashboard-test--make-grove-data
             (claude-dashboard-test--make-repo
              "myrepo" "/tmp/myrepo"
              (claude-dashboard-test--make-workspace "myrepo-feat" "feat" "active"))))
      (claude--set-workspace-state "myrepo:feat"
                                   (list :workspace "myrepo:feat" :state "stopped")
                                   'stopped)
      (claude-dashboard--paint)
      (let ((content (buffer-string)))
        (should (string-match-p "Resume" content))
        (should-not (string-match-p " Jump " content))))))

(ert-deftest claude-dashboard-test-home-resume-on-stopped ()
  "Test that repo header shows Resume when home session is stopped."
  (with-temp-buffer
    (claude-dashboard-mode)
    (let ((claude--workspace-states (make-hash-table :test 'equal)))
      (setq claude-dashboard--grove-cache
            (claude-dashboard-test--make-grove-data
             (claude-dashboard-test--make-repo "myrepo" "/tmp/myrepo")))
      (cl-letf (((symbol-function 'claude-dashboard--repo-branch)
                 (lambda (_path) "main")))
        ;; Stopped home session → Resume button
        (claude--set-workspace-state "myrepo:main"
                                     (list :workspace "myrepo:main" :state "stopped")
                                     'stopped)
        (claude-dashboard--paint)
        (let ((content (buffer-string)))
          (should (string-match-p "Resume" content))
          (should-not (string-match-p " Open " content)))))))

(ert-deftest claude-dashboard-test-home-open-when-active ()
  "Test that repo header shows Open when home session is active."
  (with-temp-buffer
    (claude-dashboard-mode)
    (let ((claude--workspace-states (make-hash-table :test 'equal)))
      (setq claude-dashboard--grove-cache
            (claude-dashboard-test--make-grove-data
             (claude-dashboard-test--make-repo "myrepo" "/tmp/myrepo")))
      (cl-letf (((symbol-function 'claude-dashboard--repo-branch)
                 (lambda (_path) "main")))
        ;; Active home session → Open button (not Resume)
        (claude--set-workspace-state "myrepo:main"
                                     (list :workspace "myrepo:main" :state "active")
                                     'active)
        (claude-dashboard--paint)
        (let ((content (buffer-string)))
          (should (string-match-p " Open " content))
          (should-not (string-match-p "Resume" content)))))))

(ert-deftest claude-dashboard-test-jump-button-on-active ()
  "Test that active workspace shows Jump button, not Resume."
  (with-temp-buffer
    (claude-dashboard-mode)
    (let ((claude--workspace-states (make-hash-table :test 'equal)))
      (setq claude-dashboard--grove-cache
            (claude-dashboard-test--make-grove-data
             (claude-dashboard-test--make-repo
              "myrepo" "/tmp/myrepo"
              (claude-dashboard-test--make-workspace "myrepo-feat" "feat" "active"))))
      (claude--set-workspace-state "myrepo:feat"
                                   (list :workspace "myrepo:feat" :state "active")
                                   'active)
      (claude-dashboard--paint)
      (let ((content (buffer-string)))
        (should (string-match-p "Jump" content))
        (should-not (string-match-p "Resume" content))))))

(ert-deftest claude-dashboard-test-last-tool-display ()
  "Test that active session with last_tool shows tool context."
  (with-temp-buffer
    (claude-dashboard-mode)
    (let ((claude--workspace-states (make-hash-table :test 'equal)))
      (setq claude-dashboard--grove-cache
            (claude-dashboard-test--make-grove-data
             (claude-dashboard-test--make-repo
              "myrepo" "/tmp/myrepo"
              (claude-dashboard-test--make-workspace "myrepo-feat" "feat" "active"))))
      (claude--set-workspace-state "myrepo:feat"
                                   (list :workspace "myrepo:feat"
                                         :state "active"
                                         :last_tool "Edit"
                                         :last_tool_detail "config.el"
                                         :started_at (* (float-time) 1000))
                                   'active)
      (claude-dashboard--paint)
      (let ((content (buffer-string)))
        ;; Verify space between icon and tool context
        (should (string-match-p "⚡ editing config\\.el" content))))))

(ert-deftest claude-dashboard-test-duration-display ()
  "Test duration formatting helper."
  ;; Recent timestamp → seconds
  (let ((recent (* (- (float-time) 30) 1000)))
    (should (string-match-p "^[0-9]+s$"
                            (claude-dashboard--format-duration recent))))
  ;; Older timestamp → minutes
  (let ((older (* (- (float-time) 300) 1000)))
    (should (string-match-p "^[0-9]+m$"
                            (claude-dashboard--format-duration older))))
  ;; Much older → hours
  (let ((old (* (- (float-time) 7200) 1000)))
    (should (string-match-p "^[0-9]+h$"
                            (claude-dashboard--format-duration old))))
  ;; nil → nil
  (should (null (claude-dashboard--format-duration nil))))

(provide 'claude-dashboard-test)
;;; claude-dashboard-test.el ends here
