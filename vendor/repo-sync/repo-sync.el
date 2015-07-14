;;; repo-sync.el --- Auto sync git repositories -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'async)

(defcustom repo-sync-repo-locations nil
  "List of locations to be synced."
  :type '(repeat string)
  :group 'repo-sync)
(defcustom repo-sync-save-buffers-pattern "^.*$"
  "Regexp pattern used with `save-some-buffers' before syncing."
  :type '(string)
  :group 'repo-sync)
(defcustom repo-sync-commit-message "Life."
  "The message to use when doing automatic commits."
  :type '(string)
  :group 'repo-sync)

(defvar *repo-sync-timer* nil)

(defun repo-sync--sequence (&rest commands)
  "Run git commands asynchronously in sequence.
COMMANDS is a list of commands and each command is a list of strings."
  (when commands
    (let* ((command (car commands))
           (rest (cdr commands))
           (callback (lambda (proc)
                       (apply #'repo-sync--sequence rest))))
      (message "Running git command: %s" command)
      (apply 'async-start-process
             (format "repo-sync-%s" (length commands))
             "git"
             callback
             command))))

(defun repo-sync--get-repo (locations)
  "Filter a list of LOCATIONS to only get existing repositories."
  (cl-some (lambda (loc)
             (let ((git-dir (concat loc "/.git")))
               (if (file-exists-p git-dir)
                   loc)))
           locations))

(defun repo-sync--save-buffers ()
  "Save any unsaved org buffers."
  (save-some-buffers t
                     (lambda ()
                       (string-match repo-sync-save-buffers-pattern
                                     (buffer-name (current-buffer))))))

(defun repo-sync--sync (repo)
  "Sync the org repo.  REPO is the repo directory."
  (cd repo)
  (repo-sync--save-buffers)
  (repo-sync--sequence
   '("pull")
   '("add" "-A")
   `("commit" "-m" ,(format "\"%s\"" repo-sync-commit-message))
   '("push")))

(defun repo-sync-start ()
  "Start the sync timer."
  (if (not *repo-sync-timer*)
      (let ((repo (repo-sync--get-repo repo-sync-repo-locations)))
        (when repo
          (setq *repo-sync-timer*
                (run-with-timer 0 (* 30 60) #'repo-sync--sync repo))
          (message "Sync started for repo at %s" repo)))
    (message "Sync already running.")))

(defun repo-sync-stop ()
  "Stop the sync timer."
  (cancel-timer *repo-sync-timer*)
  (setq *repo-sync-timer* nil))

(provide 'repo-sync)

;;; repo-sync.el ends here
