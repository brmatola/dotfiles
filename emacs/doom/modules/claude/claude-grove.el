;;; claude-grove.el --- Grove CLI wrapper -*- lexical-binding: t; -*-

;;; Commentary:
;; Thin async wrapper around the grove CLI.
;; All grove interaction goes through this module.
;; All calls are async via `make-process' — Emacs never blocks.

;;; Code:

(require 'json)

;;; Customization

(defgroup claude-grove nil
  "Grove CLI integration."
  :group 'claude
  :prefix "claude-grove-")

(defcustom claude-grove-executable "grove"
  "Path to the grove executable."
  :type 'string
  :group 'claude-grove)

(defcustom claude-grove-timeout 10
  "Seconds before a grove command is killed and callback invoked with error."
  :type 'integer
  :group 'claude-grove)

;;; Internal State

(defvar claude-grove--pending (make-hash-table :test 'equal)
  "Hash table of in-flight grove commands.
Keys are command strings, values are process objects.")

;;; Process Management

(defun claude-grove--request-pending-p (command)
  "Return t if a grove call of type COMMAND is in-flight."
  (when-let ((proc (gethash command claude-grove--pending)))
    (process-live-p proc)))

(defun claude-grove--run (args callback &optional guard-key)
  "Run grove with ARGS asynchronously, invoke CALLBACK with result.
CALLBACK signature: (lambda (ok data error-msg) ...).
GUARD-KEY if non-nil skips the call when a previous one with the same key
is still in-flight."
  (cond
   ;; In-flight guard
   ((and guard-key (claude-grove--request-pending-p guard-key))
    nil)
   ;; Check executable
   ((not (executable-find claude-grove-executable))
    (funcall callback nil nil "grove not found"))
   ;; Run the command
   (t
    (let* ((exe (executable-find claude-grove-executable))
           (output-buffer (generate-new-buffer " *grove-output*"))
           (command-args (append (list exe) args (list "--json")))
           (proc (make-process
                  :name (format "grove-%s" (car args))
                  :buffer output-buffer
                  :command command-args
                  :connection-type 'pipe
                  :sentinel
                  (lambda (process event)
                    (claude-grove--sentinel process event callback guard-key))))
           (timer (run-at-time claude-grove-timeout nil
                               (lambda ()
                                 (claude-grove--handle-timeout proc callback guard-key)))))
      ;; Store process and timer
      (process-put proc 'grove-timer timer)
      (process-put proc 'grove-callback callback)
      (when guard-key
        (puthash guard-key proc claude-grove--pending))))))

(defun claude-grove--sentinel (process event callback guard-key)
  "Handle PROCESS completion EVENT, invoke CALLBACK."
  ;; Cancel timeout timer
  (when-let ((timer (process-get process 'grove-timer)))
    (cancel-timer timer))
  ;; Clear in-flight guard
  (when guard-key
    (remhash guard-key claude-grove--pending))
  ;; Parse output
  (let ((output-buffer (process-buffer process)))
    (unwind-protect
        (if (not (string-match-p "finished" event))
            ;; Non-zero exit or killed
            (let ((output (when (buffer-live-p output-buffer)
                            (with-current-buffer output-buffer
                              (buffer-string)))))
              (funcall callback nil nil
                       (or (claude-grove--extract-error output)
                           (format "grove process %s" (string-trim event)))))
          ;; Success — parse JSON
          (let ((output (when (buffer-live-p output-buffer)
                          (with-current-buffer output-buffer
                            (buffer-string)))))
            (condition-case err
                (let* ((json-object-type 'plist)
                       (json-key-type 'keyword)
                       (parsed (json-parse-string output
                                                  :object-type 'plist
                                                  :null-object nil
                                                  :false-object nil)))
                  (if (eq (plist-get parsed :ok) t)
                      (funcall callback t (plist-get parsed :data) nil)
                    (funcall callback nil nil
                             (or (plist-get parsed :error)
                                 "grove returned ok: false"))))
              (error
               (funcall callback nil nil
                        (format "JSON parse error: %s" (error-message-string err)))))))
      ;; Clean up output buffer
      (when (buffer-live-p output-buffer)
        (kill-buffer output-buffer)))))

(defun claude-grove--handle-timeout (process callback guard-key)
  "Kill PROCESS that timed out, invoke CALLBACK with error."
  (when (process-live-p process)
    ;; Clear the sentinel so it doesn't also fire
    (set-process-sentinel process #'ignore)
    ;; Clear in-flight guard
    (when guard-key
      (remhash guard-key claude-grove--pending))
    ;; Kill process and buffer
    (let ((buf (process-buffer process)))
      (delete-process process)
      (when (buffer-live-p buf)
        (kill-buffer buf)))
    ;; Invoke callback
    (funcall callback nil nil "grove timed out")))

(defun claude-grove--extract-error (output)
  "Extract error message from grove OUTPUT string."
  (when (and output (not (string-empty-p output)))
    (condition-case nil
        (let* ((json-object-type 'plist)
               (json-key-type 'keyword)
               (parsed (json-parse-string output
                                          :object-type 'plist
                                          :null-object nil
                                          :false-object nil)))
          (plist-get parsed :error))
      (error (string-trim output)))))

;;; Public API

(defun claude-grove-repo-list (callback)
  "Fetch repo list from grove asynchronously.
CALLBACK: (lambda (ok data error-msg) ...).
Skips call if a previous repo-list is still in-flight."
  (claude-grove--run '("repo" "list") callback "repo-list"))

(defun claude-grove-workspace-status (branch callback)
  "Fetch workspace status for BRANCH asynchronously.
CALLBACK: (lambda (ok data error-msg) ...)."
  (claude-grove--run (list "workspace" "status" branch) callback))

(defun claude-grove-workspace-create (repo-path branch callback)
  "Create workspace for BRANCH from REPO-PATH asynchronously.
CALLBACK: (lambda (ok data error-msg) ...)."
  (claude-grove--run (list "workspace" "create" branch "--from" repo-path) callback))

(defun claude-grove-workspace-sync (branch callback)
  "Sync workspace BRANCH asynchronously.
CALLBACK: (lambda (ok data error-msg) ...)."
  (claude-grove--run (list "workspace" "sync" branch) callback))

(defun claude-grove-workspace-close (branch mode callback)
  "Close workspace BRANCH asynchronously.
MODE is symbol `merge' or `discard'.
CALLBACK: (lambda (ok data error-msg) ...)."
  (let ((flag (if (eq mode 'merge) "--merge" "--discard")))
    (claude-grove--run (list "workspace" "close" branch flag) callback)))

(defun claude-grove-workspace-switch (branch callback)
  "Get workspace root path for BRANCH asynchronously.
CALLBACK: (lambda (ok data error-msg) ...)."
  (claude-grove--run (list "workspace" "switch" branch) callback))

(defun claude-grove-repo-add (path callback)
  "Register repo at PATH asynchronously.
CALLBACK: (lambda (ok data error-msg) ...)."
  (claude-grove--run (list "repo" "add" path) callback))

(defun claude-grove-repo-remove (name callback)
  "Unregister repo NAME asynchronously.
CALLBACK: (lambda (ok data error-msg) ...)."
  (claude-grove--run (list "repo" "remove" name) callback))

(provide 'claude-grove)
;;; claude-grove.el ends here
