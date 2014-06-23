;;; as-jira.el --- Personal JIRA support

;;; commentary:

;;; code:

(defvar as-jira/jira-url "http://almtools/jira")

(define-key global-map (kbd "C-c m j i") 'as-jira/insert-link)

(defun as-jira/insert-link (issue)
  "Add an 'org-mode' link for ISSUE."
  (interactive "sIssue: ")
  (message "Whitespace at point: '%s'" (thing-at-point 'whitespace))
  (let ((bounds (bounds-of-thing-at-point 'whitespace)))
    (message "Points: '%s' '%s' '%s'" (point) (car bounds) (cdr bounds)))
  (let* ((issue-url (concat as-jira/jira-url "/browse/" issue))
         (whitespace-bounds (bounds-of-thing-at-point 'whitespace))
         (space-before (= (point) (car whitespace-bounds)))
         (space-after (= (point) (cdr whitespace-bounds))))
    (when space-before (insert " "))
    (insert (concat "[[[" issue-url "][" issue "]]]"))
    (when space-after (insert " "))))

;;; as-jira.el ends here
