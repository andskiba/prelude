;;; as-org.sync.el --- Auto sync a repository

;; -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'async)

(defvar as-org/*repo-locations* '("d:/org" "~/org"))
(defvar as-org/*timer* nil)

(defun as-org/sequence (&rest commands)
  "Run git commands asynchronously in sequence.
COMMANDS is a list of commands and each command is a list of strings."
  (if commands
      (let* ((command (car commands))
             (rest (cdr commands))
             (callback (lambda (proc) (apply 'do-it rest))))
        (apply 'async-start-process
               (format "org-sync-%s" (length commands))
               "git"
               callback
               command))))

(defun as-org/get-org-repo (locations)
  "Find where on the system the org repo is and set as-org/*repo*.
LOCATIONS is a list of directories to check."
  (some (lambda (loc)
          (let ((git-dir (concat loc "/.git")))
            (if (file-exists-p git-dir)
                loc)))
        locations))

(defun as-org/save-buffers ()
  "Save any unsaved org buffers."
  (save-some-buffers t
                     (lambda ()
                       (string-match "^.*\.org$"
                                     (buffer-name (current-buffer))))))

(defun as-org/sync (repo)
  "Sync the org repo.  REPO is the repo directory."
  (cd repo)
  (as-org/save-buffers)
  (as-org/sequence '("pull")
                   '("add" "-A")
                   '("commit" "-m" "\"Life.\"")
                   '("push")))

(defun as-org/start ()
  "Start the sync timer."
  (if (not as-org/*timer*)
      (let ((repo (as-org/get-org-repo as-org/*repo-locations*)))
        (if repo
            (setq as-org/*timer*
                  (run-with-timer 0 (* 30 60)
                                  (lambda () (as-org/sync repo))))))
    (message "Sync already running.")))

(defun as-org/stop ()
  "Stop the sync timer."
  (cancel-timer as-org/*timer*)
  (setq as-org/*timer* nil))

(as-org/start)

(provide 'as-org-sync)

;;; as-org-sync.el ends here
