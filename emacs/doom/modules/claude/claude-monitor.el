;;; claude-monitor.el --- Session awareness via SAP -*- lexical-binding: t; -*-

;;; Commentary:
;; Monitors Claude sessions via SAP (Session Awareness Protocol).
;; Polls `sap status --json` for session state instead of scraping vterm.
;; No dependencies on other claude modules.
;; Dashboard is the sole UI for workspace status — no modeline.

;;; Code:

(require 'json)

;;; Customization

(defcustom claude-monitor-interval 5
  "Seconds between SAP status polls.
File watcher handles the fast path; this timer is a fallback."
  :type 'integer
  :group 'claude)

;;; Hooks

(defvar claude-attention-change-hook nil
  "Hook run when any workspace's attention status changes.
Called with (workspace-name new-status).
NEW-STATUS is a symbol: `active', `idle', `attention', `stopped', or nil.")

;;; Internal State

(defvar claude-monitor-timer nil
  "Timer for SAP polling.")

(defvar claude-monitor--file-watch nil
  "File notify descriptor for SAP db directory.")

(defvar claude-monitor--file-watch-debounce nil
  "Timer for debouncing file watch events.")

(defvar claude--workspace-states (make-hash-table :test 'equal)
  "Hash table mapping workspace name to (STATE . SESSION-PLIST).
Populated by polling loop, queried by public API.")

;;; Buffer Name Matching

(defun claude-monitor--workspace-for-buffer (buffer)
  "Get workspace name for BUFFER based on buffer name pattern.
Buffer names follow `*claude:REPO:BRANCH*'.
Returns \"REPO:BRANCH\" or nil."
  (let ((name (buffer-name buffer)))
    (when (string-match "^\\*claude:\\([^:]+\\):\\([^*]+\\)\\*$" name)
      (format "%s:%s" (match-string 1 name) (match-string 2 name)))))

(defun claude-monitor--claude-buffers ()
  "Return list of all live claude vterm buffers."
  (seq-filter (lambda (buf)
                (and (buffer-live-p buf)
                     (string-match-p "^\\*claude:[^:]+:[^*]+\\*$"
                                     (buffer-name buf))))
              (buffer-list)))

;;; SAP CLI Wrapper

(defcustom claude-stopped-expiry nil
  "Seconds after which stopped sessions are cleared from state.
Set to nil to disable expiry."
  :type '(choice integer (const nil))
  :group 'claude)

(defcustom claude-sap-executable "sap"
  "Path to the sap executable."
  :type 'string
  :group 'claude)

(defcustom claude-sap-timeout 5
  "Seconds before a sap command is killed."
  :type 'integer
  :group 'claude)

(defvar claude-sap--pending (make-hash-table :test 'equal)
  "Hash table of in-flight sap commands for dedup guard.")

(defun claude-sap--request-pending-p (key)
  "Return t if a sap call with KEY is in-flight."
  (when-let ((proc (gethash key claude-sap--pending)))
    (process-live-p proc)))

(defun claude-sap--run (args callback &optional guard-key)
  "Run sap with ARGS asynchronously, invoke CALLBACK with result.
CALLBACK signature: (lambda (ok data error-msg) ...).
GUARD-KEY if non-nil skips the call when a previous one with the
same key is still in-flight.
SAP returns raw JSON (no envelope), so parsed output is passed directly."
  (cond
   ;; In-flight guard
   ((and guard-key (claude-sap--request-pending-p guard-key))
    nil)
   ;; Check executable
   ((not (executable-find claude-sap-executable))
    (funcall callback nil nil nil))
   ;; Run the command
   (t
    (let* ((default-directory (if (file-directory-p default-directory)
                                  default-directory
                                (expand-file-name "~")))
           (exe (executable-find claude-sap-executable))
           (output-buffer (generate-new-buffer " *sap-output*"))
           (command-args (append (list exe) args))
           (proc (make-process
                  :name (format "sap-%s" (car args))
                  :buffer output-buffer
                  :command command-args
                  :connection-type 'pipe
                  :sentinel
                  (lambda (process event)
                    (claude-sap--sentinel process event callback guard-key))))
           (timer (run-at-time claude-sap-timeout nil
                               (lambda ()
                                 (claude-sap--handle-timeout proc callback guard-key)))))
      ;; Store process and timer
      (process-put proc 'sap-timer timer)
      (process-put proc 'sap-callback callback)
      (when guard-key
        (puthash guard-key proc claude-sap--pending))))))

(defun claude-sap--sentinel (process event callback guard-key)
  "Handle PROCESS completion EVENT, invoke CALLBACK.
GUARD-KEY is cleared from the pending table."
  ;; Cancel timeout timer
  (when-let ((timer (process-get process 'sap-timer)))
    (cancel-timer timer))
  ;; Clear in-flight guard
  (when guard-key
    (remhash guard-key claude-sap--pending))
  ;; Parse output
  (let ((output-buffer (process-buffer process)))
    (unwind-protect
        (if (not (string-match-p "finished" event))
            (funcall callback nil nil
                     (format "sap process %s" (string-trim event)))
          ;; Success — parse raw JSON
          (let* ((output (when (buffer-live-p output-buffer)
                           (with-current-buffer output-buffer
                             (buffer-string))))
                 (parsed nil)
                 (parse-error nil))
            (condition-case err
                (setq parsed (json-parse-string output
                                                :object-type 'plist
                                                :null-object nil
                                                :false-object nil))
              (error
               (setq parse-error (error-message-string err))))
            (if parse-error
                (funcall callback nil nil
                         (format "JSON parse error: %s" parse-error))
              (funcall callback t parsed nil))))
      ;; Clean up output buffer
      (when (buffer-live-p output-buffer)
        (kill-buffer output-buffer)))))

(defun claude-sap--handle-timeout (proc callback guard-key)
  "Kill PROC that timed out, invoke CALLBACK with error.
GUARD-KEY is cleared from the pending table."
  (when (process-live-p proc)
    ;; Clear the sentinel so it doesn't also fire
    (set-process-sentinel proc #'ignore)
    ;; Clear in-flight guard
    (when guard-key
      (remhash guard-key claude-sap--pending))
    ;; Kill process and buffer
    (let ((buf (process-buffer proc)))
      (delete-process proc)
      (when (buffer-live-p buf)
        (kill-buffer buf)))
    ;; Invoke callback
    (funcall callback nil nil "sap timed out")))

;;; SAP Public Commands

(defun claude-sap-status (callback)
  "Query sap for current session states.
CALLBACK: (lambda (ok data error-msg) ...).
Skips call if a previous status query is still in-flight."
  (claude-sap--run '("status" "--json") callback "sap-status"))

(defun claude-sap-latest (workspace callback)
  "Query sap for latest session in WORKSPACE.
CALLBACK: (lambda (ok data error-msg) ...)."
  (claude-sap--run (list "latest" "--workspace" workspace "--json") callback))

;;; State Table

(defun claude--get-workspace-state (workspace)
  "Return state symbol for WORKSPACE, or nil."
  (car (gethash workspace claude--workspace-states)))

(defun claude--set-workspace-state (workspace session state)
  "Set STATE and SESSION data for WORKSPACE."
  (puthash workspace (cons state session) claude--workspace-states))

(defun claude--set-workspace-stopped (workspace)
  "Mark WORKSPACE as stopped, preserving session data."
  (when-let ((entry (gethash workspace claude--workspace-states)))
    (puthash workspace (cons 'stopped (cdr entry))
             claude--workspace-states)))

;;; Session Expiry

(defun claude--expire-stopped-sessions ()
  "Remove stopped sessions older than `claude-stopped-expiry' seconds."
  (when claude-stopped-expiry
    (let ((cutoff (- (float-time) claude-stopped-expiry))
          (to-remove nil))
      (maphash
       (lambda (ws entry)
         (when (eq (car entry) 'stopped)
           (let* ((session (cdr entry))
                  (last-event (plist-get session :last_event_at))
                  (event-time (when last-event (/ last-event 1000.0))))
             (when (and event-time (< event-time cutoff))
               (push ws to-remove)))))
       claude--workspace-states)
      (dolist (ws to-remove)
        (remhash ws claude--workspace-states)))))

;;; Polling Loop

(defun claude--poll-sap ()
  "Poll sap for current session states and update internal state.
Detects stopped sessions by absence from sap status response.
Treats stale sessions as stopped."
  (claude-sap-status
   (lambda (ok data _err)
     (when ok
       (let ((sessions (append (plist-get data :sessions) nil))
             (seen (make-hash-table :test 'equal)))
         ;; Process sessions present in response
         (dolist (session sessions)
           (let* ((ws (plist-get session :workspace))
                  (stale (eq (plist-get session :stale) t))
                  (state (if stale 'stopped
                           (intern (plist-get session :state))))
                  (old-state (claude--get-workspace-state ws)))
             (puthash ws t seen)
             (claude--set-workspace-state ws session state)
             (when (not (eq state old-state))
               (run-hook-with-args 'claude-attention-change-hook
                                   ws state))))
         ;; Detect stopped: known workspaces absent from response
         (maphash
          (lambda (ws entry)
            (unless (or (gethash ws seen)
                        (eq (car entry) 'stopped))
              (claude--set-workspace-stopped ws)
              (run-hook-with-args 'claude-attention-change-hook
                                  ws 'stopped)))
          claude--workspace-states)
         ;; Expire old stopped sessions
         (claude--expire-stopped-sessions))))))

;;; Public Query Interface

(defun claude-workspace-attention (workspace)
  "Return attention state for WORKSPACE.
WORKSPACE is in sap format: \"repo:branch\".
Returns `active', `idle', `attention', `stopped', or nil."
  (claude--get-workspace-state workspace))

(defun claude-workspace-session (workspace)
  "Return full session plist for WORKSPACE, or nil.
WORKSPACE is in sap format: \"repo:branch\".
Session fields: :session_id :workspace :state :started_at
:last_event_at :last_tool :last_tool_detail :transcript_path."
  (cdr (gethash workspace claude--workspace-states)))

;;; File Watcher

(defun claude-monitor--sap-db-dir ()
  "Return the SAP database directory path.
Uses SAP_DB_PATH environment variable if set, otherwise ~/.sap/."
  (or (getenv "SAP_DB_PATH")
      (expand-file-name "~/.sap/")))

(defun claude-monitor--on-sap-db-change (_event)
  "Handle SAP db file change EVENT with debouncing.
Triggers an immediate poll after a short delay to batch rapid writes."
  (when claude-monitor--file-watch-debounce
    (cancel-timer claude-monitor--file-watch-debounce))
  (setq claude-monitor--file-watch-debounce
        (run-at-time 0.3 nil #'claude--poll-sap)))

(defun claude-monitor--start-file-watch ()
  "Start watching the SAP db directory for changes."
  (unless claude-monitor--file-watch
    (let ((dir (claude-monitor--sap-db-dir)))
      (when (file-directory-p dir)
        (condition-case nil
            (setq claude-monitor--file-watch
                  (file-notify-add-watch dir '(change)
                                         #'claude-monitor--on-sap-db-change))
          (error nil))))))

(defun claude-monitor--stop-file-watch ()
  "Stop watching the SAP db directory."
  (when claude-monitor--file-watch-debounce
    (cancel-timer claude-monitor--file-watch-debounce)
    (setq claude-monitor--file-watch-debounce nil))
  (when claude-monitor--file-watch
    (file-notify-rm-watch claude-monitor--file-watch)
    (setq claude-monitor--file-watch nil)))

;;; Focus-In Handler

(defun claude-monitor--on-focus-change ()
  "Handle Emacs focus change (e.g. after sleep/wake).
When any frame gains focus, clears stale pending requests and
triggers an immediate poll."
  (when (and claude-monitor-timer
             (cl-some #'frame-focus-state (frame-list)))
    (clrhash claude-sap--pending)
    (claude--poll-sap)))

;;; Monitor Control

;;;###autoload
(defun claude-monitor-start ()
  "Start SAP polling and file watcher."
  (interactive)
  (unless claude-monitor-timer
    (setq claude-monitor-timer
          (run-with-timer claude-monitor-interval
                          claude-monitor-interval
                          #'claude--poll-sap)))
  (claude-monitor--start-file-watch)
  (add-function :after after-focus-change-function
                #'claude-monitor--on-focus-change))

;;;###autoload
(defun claude-monitor-stop ()
  "Stop SAP polling, file watcher, and clear state."
  (interactive)
  (when claude-monitor-timer
    (cancel-timer claude-monitor-timer)
    (setq claude-monitor-timer nil))
  (claude-monitor--stop-file-watch)
  (remove-function after-focus-change-function
                   #'claude-monitor--on-focus-change)
  (clrhash claude--workspace-states))

;;;###autoload
(defun claude-monitor-toggle ()
  "Toggle monitoring on/off."
  (interactive)
  (if claude-monitor-timer
      (claude-monitor-stop)
    (claude-monitor-start)))

(provide 'claude-monitor)
;;; claude-monitor.el ends here
