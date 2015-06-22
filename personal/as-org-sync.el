;;; as-org.sync.el --- Auto sync a repository

;;; Commentary:

;;; Code:

(defvar as-org/*repo-locations* '("d:/org" "~/org"))
(defvar as-org/*repo* nil)
(defvar as-org/*sync-buffer* "*org-sync*")
(defvar as-org/*timer* nil)

(defun as-org/set-org-repo ()
  "Find where on the system the org repo is and set as-org/*repo*."
  (dolist (loc as-org/*repo-locations*)
    (let ((git-dir (concat loc "/.git")))
      (if (file-exists-p git-dir)
          (setq as-org/*repo* loc)))))

(defun as-org/git-pull ()
  "Call a git pull command on the repository."
  (shell-command "git pull" as-org/*sync-buffer*))

(defun as-org/git-push ()
  "Call git status."
  (shell-command "git add -A" as-org/*sync-buffer*)
  (shell-command "git commit -m \"Life.\"" as-org/*sync-buffer*)
  (shell-command "git push" as-org/*sync-buffer*))

(defun as-org/save-buffers ()
  "Save any unsaved org buffers."
  (save-some-buffers t
                     (lambda ()
                       (string-match "^.*\.org$"
                                     (buffer-name (current-buffer))))))

(defun as-org/sync ()
  "Sync the org repo."
  (cd as-org/*repo*)
  (as-org/save-buffers)
  (as-org/git-pull)
  (as-org/git-push))

(defun as-org/start ()
  "Start the sync timer."
  (if (not as-org/*timer*)
      (setq as-org/*timer* (run-with-timer 0 (* 30 60) 'as-org/sync))))

(defun as-org/stop ()
  "Stop the sync timer."
  (cancel-timer as-org/*timer*)
  (setq as-org/*timer* nil))

(as-org/set-org-repo)
(if as-org/*repo*
    (as-org/start))

(provide 'as-org-sync)

;;; as-org-sync.el ends here
