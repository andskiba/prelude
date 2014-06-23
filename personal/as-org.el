;;; as-org.el --- Andrzej Skiba's configurations for Emacs

;;; Commentary:

;;; Code:

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

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to 'done' when all subentries are done, to 'todo' otherwise."
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
   (sh . t)
   (sql . nil)
   (sqlite . nil)))

;;; as-org.el ends here
