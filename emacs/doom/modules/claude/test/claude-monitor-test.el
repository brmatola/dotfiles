;;; claude-monitor-test.el --- Tests for SAP-based monitor -*- lexical-binding: t; -*-

(require 'ert)
(add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name)))
(require 'claude-monitor)

;;; Test Helpers

(defmacro claude-monitor-test--with-clean-state (&rest body)
  "Run BODY with a fresh monitor state."
  `(let ((claude--workspace-states (make-hash-table :test 'equal))
         (claude-sap--pending (make-hash-table :test 'equal))
         (hook-calls nil))
     (cl-letf (((symbol-function 'claude-sap-status)
                (lambda (callback)
                  (funcall callback t claude-monitor-test--sap-response nil))))
       ,@body)))

(defvar claude-monitor-test--sap-response nil
  "Canned SAP status response for testing.")

(defun claude-monitor-test--make-session (workspace state &optional extras)
  "Create a mock SAP session plist.
EXTRAS is an optional plist of additional fields."
  (append (list :workspace workspace
                :state state
                :session_id (format "sess-%s" workspace)
                :started_at (* (float-time) 1000)
                :last_event_at (* (float-time) 1000))
          extras))

(defun claude-monitor-test--make-response (&rest sessions)
  "Create a mock SAP status response with SESSIONS."
  (list :sessions (vconcat sessions)))

;;; Buffer Name Matching Tests

(ert-deftest claude-monitor-test-workspace-for-buffer ()
  "Test buffer name to workspace name extraction."
  (with-temp-buffer
    (rename-buffer "*claude:dotfiles:fix-zsh*" t)
    (should (equal (claude-monitor--workspace-for-buffer (current-buffer))
                   "dotfiles:fix-zsh")))
  ;; Non-claude buffer
  (with-temp-buffer
    (should (null (claude-monitor--workspace-for-buffer (current-buffer))))))

(ert-deftest claude-monitor-test-workspace-for-buffer-home ()
  "Test buffer name extraction for home workspace."
  (with-temp-buffer
    (rename-buffer "*claude:dotfiles:home*" t)
    (should (equal (claude-monitor--workspace-for-buffer (current-buffer))
                   "dotfiles:home"))))

(ert-deftest claude-monitor-test-claude-buffers ()
  "Test finding claude buffers."
  (let ((buf1 (generate-new-buffer "*claude:repo1:branch1*"))
        (buf2 (generate-new-buffer "*claude:repo2:branch2*"))
        (buf3 (generate-new-buffer "*not-claude*")))
    (unwind-protect
        (let ((claude-bufs (claude-monitor--claude-buffers)))
          (should (memq buf1 claude-bufs))
          (should (memq buf2 claude-bufs))
          (should (not (memq buf3 claude-bufs))))
      (kill-buffer buf1)
      (kill-buffer buf2)
      (kill-buffer buf3))))

;;; State Transition Tests

(ert-deftest claude-monitor-test-active-state ()
  "Test that active session reports active state."
  (claude-monitor-test--with-clean-state
   (let ((claude-monitor-test--sap-response
          (claude-monitor-test--make-response
           (claude-monitor-test--make-session "dotfiles:main" "active"))))
     (claude--poll-sap)
     (should (eq (claude-workspace-attention "dotfiles:main") 'active)))))

(ert-deftest claude-monitor-test-idle-state ()
  "Test that idle session reports idle state."
  (claude-monitor-test--with-clean-state
   (let ((claude-monitor-test--sap-response
          (claude-monitor-test--make-response
           (claude-monitor-test--make-session "dotfiles:main" "idle"))))
     (claude--poll-sap)
     (should (eq (claude-workspace-attention "dotfiles:main") 'idle)))))

(ert-deftest claude-monitor-test-attention-state ()
  "Test that attention session reports attention state."
  (claude-monitor-test--with-clean-state
   (let ((claude-monitor-test--sap-response
          (claude-monitor-test--make-response
           (claude-monitor-test--make-session "dotfiles:main" "attention"))))
     (claude--poll-sap)
     (should (eq (claude-workspace-attention "dotfiles:main") 'attention)))))

(ert-deftest claude-monitor-test-state-transitions ()
  "Test full state transition cycle: active → idle → attention → active."
  (claude-monitor-test--with-clean-state
   (let ((hook-calls nil)
         (claude-attention-change-hook nil))
     (add-hook 'claude-attention-change-hook
               (lambda (ws state) (push (list ws state) hook-calls)))
     ;; active
     (let ((claude-monitor-test--sap-response
            (claude-monitor-test--make-response
             (claude-monitor-test--make-session "dotfiles:main" "active"))))
       (claude--poll-sap)
       (should (eq (claude-workspace-attention "dotfiles:main") 'active)))
     ;; → idle
     (let ((claude-monitor-test--sap-response
            (claude-monitor-test--make-response
             (claude-monitor-test--make-session "dotfiles:main" "idle"))))
       (claude--poll-sap)
       (should (eq (claude-workspace-attention "dotfiles:main") 'idle)))
     ;; → attention
     (let ((claude-monitor-test--sap-response
            (claude-monitor-test--make-response
             (claude-monitor-test--make-session "dotfiles:main" "attention"))))
       (claude--poll-sap)
       (should (eq (claude-workspace-attention "dotfiles:main") 'attention)))
     ;; → active
     (let ((claude-monitor-test--sap-response
            (claude-monitor-test--make-response
             (claude-monitor-test--make-session "dotfiles:main" "active"))))
       (claude--poll-sap)
       (should (eq (claude-workspace-attention "dotfiles:main") 'active)))
     ;; Verify all transitions fired hooks (pushed in reverse order)
     (should (= (length hook-calls) 4))
     (setq hook-calls (nreverse hook-calls))
     (should (eq (cadr (nth 0 hook-calls)) 'active))
     (should (eq (cadr (nth 1 hook-calls)) 'idle))
     (should (eq (cadr (nth 2 hook-calls)) 'attention))
     (should (eq (cadr (nth 3 hook-calls)) 'active)))))

(ert-deftest claude-monitor-test-hook-fires-on-transition ()
  "Test that hook fires on state transitions."
  (claude-monitor-test--with-clean-state
   (let ((hook-calls nil)
         (claude-attention-change-hook nil))
     (add-hook 'claude-attention-change-hook
               (lambda (ws state) (push (list ws state) hook-calls)))
     (let ((claude-monitor-test--sap-response
            (claude-monitor-test--make-response
             (claude-monitor-test--make-session "repo:branch" "active"))))
       (claude--poll-sap))
     (should (= (length hook-calls) 1))
     (should (equal (car hook-calls) '("repo:branch" active))))))

(ert-deftest claude-monitor-test-no-hook-on-same-state ()
  "Test that hook does not fire when state stays the same."
  (claude-monitor-test--with-clean-state
   (let ((hook-calls nil)
         (claude-attention-change-hook nil))
     (add-hook 'claude-attention-change-hook
               (lambda (ws state) (push (list ws state) hook-calls)))
     (let ((claude-monitor-test--sap-response
            (claude-monitor-test--make-response
             (claude-monitor-test--make-session "repo:branch" "active"))))
       (claude--poll-sap)
       (claude--poll-sap))
     ;; Only one hook call despite two polls
     (should (= (length hook-calls) 1)))))

;;; Stopped Detection Tests

(ert-deftest claude-monitor-test-stopped-by-absence ()
  "Test that absent workspace is marked stopped."
  (claude-monitor-test--with-clean-state
   (let ((hook-calls nil)
         (claude-attention-change-hook nil))
     (add-hook 'claude-attention-change-hook
               (lambda (ws state) (push (list ws state) hook-calls)))
     ;; First poll: session present
     (let ((claude-monitor-test--sap-response
            (claude-monitor-test--make-response
             (claude-monitor-test--make-session "repo:branch" "active"))))
       (claude--poll-sap))
     ;; Second poll: session absent
     (let ((claude-monitor-test--sap-response
            (claude-monitor-test--make-response)))
       (claude--poll-sap))
     (should (eq (claude-workspace-attention "repo:branch") 'stopped))
     ;; Hook should have fired for stopped (most recent push)
     (should (equal (car hook-calls) '("repo:branch" stopped))))))

(ert-deftest claude-monitor-test-stopped-no-refire ()
  "Test that stopped workspace doesn't re-fire hook."
  (claude-monitor-test--with-clean-state
   (let ((hook-calls nil)
         (claude-attention-change-hook nil))
     (add-hook 'claude-attention-change-hook
               (lambda (ws state) (push (list ws state) hook-calls)))
     ;; First: active
     (let ((claude-monitor-test--sap-response
            (claude-monitor-test--make-response
             (claude-monitor-test--make-session "repo:branch" "active"))))
       (claude--poll-sap))
     ;; Second: absent → stopped
     (let ((claude-monitor-test--sap-response
            (claude-monitor-test--make-response)))
       (claude--poll-sap))
     ;; Third: still absent → no re-fire
     (let ((claude-monitor-test--sap-response
            (claude-monitor-test--make-response)))
       (claude--poll-sap))
     ;; Only 2 hook calls: active + stopped
     (should (= (length hook-calls) 2)))))

(ert-deftest claude-monitor-test-stale-treated-as-stopped ()
  "Test that stale sessions are treated as stopped."
  (claude-monitor-test--with-clean-state
   (let ((claude-monitor-test--sap-response
          (claude-monitor-test--make-response
           (claude-monitor-test--make-session "repo:branch" "active"
                                              (list :stale t)))))
     (claude--poll-sap)
     (should (eq (claude-workspace-attention "repo:branch") 'stopped)))))

;;; Multiple Workspaces

(ert-deftest claude-monitor-test-multiple-workspaces ()
  "Test different states for different workspaces simultaneously."
  (claude-monitor-test--with-clean-state
   (let ((claude-monitor-test--sap-response
          (claude-monitor-test--make-response
           (claude-monitor-test--make-session "dotfiles:main" "active")
           (claude-monitor-test--make-session "acorn:auth" "idle")
           (claude-monitor-test--make-session "sap:core" "attention"))))
     (claude--poll-sap)
     (should (eq (claude-workspace-attention "dotfiles:main") 'active))
     (should (eq (claude-workspace-attention "acorn:auth") 'idle))
     (should (eq (claude-workspace-attention "sap:core") 'attention)))))

;;; SAP Not Installed

(ert-deftest claude-monitor-test-sap-not-installed ()
  "Test graceful handling when sap is not installed."
  (let ((claude--workspace-states (make-hash-table :test 'equal))
        (claude-sap--pending (make-hash-table :test 'equal))
        (claude-sap-executable "nonexistent-sap-binary-xyz"))
    ;; Should not error
    (claude-sap-status (lambda (_ok _data _err) nil))
    ;; All queries return nil
    (should (null (claude-workspace-attention "any:workspace")))
    (should (null (claude-workspace-session "any:workspace")))))

;;; Public API Tests

(ert-deftest claude-monitor-test-workspace-attention-returns-symbols ()
  "Test that workspace-attention returns correct symbols."
  (claude-monitor-test--with-clean-state
   ;; Unknown workspace → nil
   (should (null (claude-workspace-attention "unknown:ws")))
   ;; Set various states and verify
   (claude--set-workspace-state "ws:a" '(:workspace "ws:a") 'active)
   (claude--set-workspace-state "ws:b" '(:workspace "ws:b") 'idle)
   (claude--set-workspace-state "ws:c" '(:workspace "ws:c") 'attention)
   (claude--set-workspace-state "ws:d" '(:workspace "ws:d") 'stopped)
   (should (eq (claude-workspace-attention "ws:a") 'active))
   (should (eq (claude-workspace-attention "ws:b") 'idle))
   (should (eq (claude-workspace-attention "ws:c") 'attention))
   (should (eq (claude-workspace-attention "ws:d") 'stopped))))

(ert-deftest claude-monitor-test-workspace-session-returns-plist ()
  "Test that workspace-session returns session plist."
  (claude-monitor-test--with-clean-state
   ;; Unknown workspace → nil
   (should (null (claude-workspace-session "unknown:ws")))
   ;; Set state with session data
   (let ((session (list :session_id "sess-123"
                        :workspace "dotfiles:main"
                        :state "active"
                        :last_tool "Edit"
                        :last_tool_detail "config.el")))
     (claude--set-workspace-state "dotfiles:main" session 'active)
     (let ((result (claude-workspace-session "dotfiles:main")))
       (should result)
       (should (equal (plist-get result :session_id) "sess-123"))
       (should (equal (plist-get result :last_tool) "Edit"))
       (should (equal (plist-get result :last_tool_detail) "config.el"))))))

;;; Stopped Session Expiry

(ert-deftest claude-monitor-test-expired-stopped-removed ()
  "Test that stopped sessions older than expiry are removed."
  (let ((claude--workspace-states (make-hash-table :test 'equal))
        (claude-stopped-expiry 1800))
    ;; Stopped session with old last_event_at (2 hours ago)
    (claude--set-workspace-state "repo:old"
                                 (list :workspace "repo:old"
                                       :last_event_at (* (- (float-time) 7200) 1000))
                                 'stopped)
    ;; Stopped session with recent last_event_at (5 minutes ago)
    (claude--set-workspace-state "repo:recent"
                                 (list :workspace "repo:recent"
                                       :last_event_at (* (- (float-time) 300) 1000))
                                 'stopped)
    (claude--expire-stopped-sessions)
    ;; Old one removed, recent one kept
    (should (null (claude-workspace-attention "repo:old")))
    (should (eq (claude-workspace-attention "repo:recent") 'stopped))))

(ert-deftest claude-monitor-test-expiry-ignores-non-stopped ()
  "Test that expiry only affects stopped sessions."
  (let ((claude--workspace-states (make-hash-table :test 'equal))
        (claude-stopped-expiry 1800))
    ;; Active session with old timestamp — should NOT be removed
    (claude--set-workspace-state "repo:active"
                                 (list :workspace "repo:active"
                                       :last_event_at (* (- (float-time) 7200) 1000))
                                 'active)
    (claude--expire-stopped-sessions)
    (should (eq (claude-workspace-attention "repo:active") 'active))))

(ert-deftest claude-monitor-test-expiry-disabled ()
  "Test that nil expiry disables cleanup."
  (let ((claude--workspace-states (make-hash-table :test 'equal))
        (claude-stopped-expiry nil))
    (claude--set-workspace-state "repo:old"
                                 (list :workspace "repo:old"
                                       :last_event_at (* (- (float-time) 7200) 1000))
                                 'stopped)
    (claude--expire-stopped-sessions)
    ;; Still present — expiry disabled
    (should (eq (claude-workspace-attention "repo:old") 'stopped))))

;;; Monitor Start/Stop

(ert-deftest claude-monitor-test-stop-clears-state ()
  "Test that monitor-stop clears the state table."
  (let ((claude--workspace-states (make-hash-table :test 'equal))
        (claude-monitor-timer nil))
    (claude--set-workspace-state "ws:a" '(:workspace "ws:a") 'active)
    (should (eq (claude-workspace-attention "ws:a") 'active))
    (claude-monitor-stop)
    (should (null (claude-workspace-attention "ws:a")))))

;;; File Watcher Tests

(ert-deftest claude-monitor-test-file-watch-starts-with-monitor ()
  "Test that file watcher starts when monitor starts."
  (let ((claude-monitor-timer nil)
        (claude-monitor--file-watch nil)
        (claude-monitor--file-watch-debounce nil)
        (watch-started nil))
    (cl-letf (((symbol-function 'run-with-timer)
               (lambda (&rest _) 'fake-timer))
              ((symbol-function 'claude-monitor--start-file-watch)
               (lambda () (setq watch-started t)))
              ((symbol-function 'add-function) (lambda (&rest _) nil)))
      (claude-monitor-start)
      (should watch-started))
    ;; Clean up
    (when (timerp claude-monitor-timer)
      (cancel-timer claude-monitor-timer))
    (setq claude-monitor-timer nil)))

(ert-deftest claude-monitor-test-file-watch-stops-with-monitor ()
  "Test that file watcher stops when monitor stops."
  (let ((claude-monitor-timer 'fake-timer)
        (claude-monitor--file-watch 'fake-watch)
        (claude-monitor--file-watch-debounce nil)
        (claude--workspace-states (make-hash-table :test 'equal))
        (watch-stopped nil))
    (cl-letf (((symbol-function 'cancel-timer) (lambda (_) nil))
              ((symbol-function 'claude-monitor--stop-file-watch)
               (lambda () (setq watch-stopped t)))
              ((symbol-function 'remove-function) (lambda (&rest _) nil)))
      (claude-monitor-stop)
      (should watch-stopped))))

;;; Focus Change Handler Tests

(ert-deftest claude-monitor-test-focus-change-clears-pending ()
  "Test that focus-change handler clears pending table and polls."
  (let ((claude-monitor-timer 'fake-timer)
        (claude-sap--pending (make-hash-table :test 'equal))
        (polled nil))
    (puthash "sap-status" 'fake-proc claude-sap--pending)
    (cl-letf (((symbol-function 'claude--poll-sap)
               (lambda () (setq polled t)))
              ((symbol-function 'frame-focus-state)
               (lambda (_) t)))
      (claude-monitor--on-focus-change)
      (should (= 0 (hash-table-count claude-sap--pending)))
      (should polled))))

(ert-deftest claude-monitor-test-focus-change-noop-when-stopped ()
  "Test that focus-change is a no-op when monitor is not running."
  (let ((claude-monitor-timer nil)
        (polled nil))
    (cl-letf (((symbol-function 'claude--poll-sap)
               (lambda () (setq polled t))))
      (claude-monitor--on-focus-change)
      (should-not polled))))

(provide 'claude-monitor-test)
;;; claude-monitor-test.el ends here
