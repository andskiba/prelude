;;; as-org.el --- Andrzej Skiba's configurations for Emacs

;;; Commentary:

;;; Code:

;; Start org repository sync

(require 'repo-sync)

(setq repo-sync-repo-locations '("d:\\org" "~/org"))
(setq repo-sync-save-buffers-pattern "^.*\.org$")

;; (repo-sync-start)

(require 'use-package)

(use-package org
  :config
  (add-to-list 'org-file-apps '("\\.xlsx?" . default)))

;; Encryption

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.
;; (setq org-crypt-key nil)

;; Org file sets

(defvar *as-org/agenda-sets*
  '(("~/org/life.org" "~/misys-org/tasks.org")
    ("~/org/life.org" "~/org/work.org")))
(defvar *as-org/current-agenda-set* -1)
(defun as-org/next-agenda-set ()
  "Cycle through sets of agenda files by assigning `org-agenda-files'."
  (interactive)
  (setf *as-org/current-agenda-set*
        (mod (+ *as-org/current-agenda-set* 1)
             (length *as-org/agenda-sets*)))
  (setf org-agenda-files (nth *as-org/current-agenda-set*
                              *as-org/agenda-sets*))
  (org-agenda-redo))

(defun as/set-org-keys ()
  "Set my key bindings for `org-mode'."
  (local-set-key (kbd "M-S-<down>") 'org-move-subtree-down)
  (local-set-key (kbd "M-S-<up>") 'org-move-subtree-up)
  (local-set-key (kbd "C-c m o i") 'org-time-stamp-inactive))

(add-hook 'org-mode-hook 'as/set-org-keys)

(define-key prelude-mode-map (kbd "M-S-<down>") nil)
(define-key prelude-mode-map (kbd "M-S-<up>") nil)

(global-set-key (kbd "C-c m o n") 'as-org/next-agenda-set)

;; Configuration

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)"  "|"
                  "DONE(d)" "CANCELLED(c)" "DEFERRED(f)")))
(setq org-tag-alist
      '(("Work" . ?w) ("project" . ?p) ("programming" . ?g)
        ("reading" . ?r) ("school" . ?s)))
(setq org-log-done 'time)
(setq org-hide-leading-stars t)
(setq org-startup-folded t)

(setq org-list-description-max-indent 5)

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to 'done' when all sub-entries are done, to 'todo' otherwise.
N-DONE number of done entries.  N-NOT-DONE number of entries not done."
  (let (org-log-done org-log-states)    ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; custom agenda commands

(setq org-agenda-custom-commands
      '(("ic"
         "Misys current TODOs"
         tags-todo
         "CATEGORY=\"current\""
         ((org-agenda-files '("~/org/misys/tasks.org"))))
        ("if"
         "Misys future TODOs"
         tags-todo
         "CATEGORY=\"future\""
         ((org-agenda-files '("~/org/misys/tasks.org"))))
        ("iC"
         "Life current TODOs"
         tags-todo
         "CATEGORY=\"current\""
         ((org-agenda-files '("~/org/work.org"))))
        ("iF"
         "Life current TODOs"
         tags-todo
         "CATEGORY=\"future\""
         ((org-agenda-files '("~/org/work.org"))))))

;; org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (ditaa . t)
   (dot . t)
   (emacs-lisp . t)
   (gnuplot . nil)
   (haskell . nil)
   (latex . t)
   (ocaml . nil)
   (perl . t)
   (python . t)
   (ruby . t)
   (screen . nil)
   ;; (sh . t)
   (sql . nil)
   (sqlite . nil)))

;;; as-org.el ends here
