;;; as-jira.el --- Personal JIRA support

;;; commentary:

;;; code:

(defvar as-jira/jira-url "")

(define-key global-map (kbd "C-c m j i") 'as-jira/insert-link)
(define-key global-map (kbd "C-c m j o") 'as-jira/open-issue)

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

(defvar as-jira/id-regex "\\([A-Z]+\\)[-_]\\([0-9]+\\)")

(defun as-jira/conv-issue-id (issue-id)
  "Convert ISSUE-ID to contain - instead of _ if possible."
  (if (string-match as-jira/id-regex issue-id)
      (concat (match-string 1 issue-id)
              "-"
              (match-string 2 issue-id))
    issue-id))

(defun as-jira/open-issue (issue)
  "Open given ISSUE in the browser."
  (interactive (list
                (let ((issue-at-point (as-jira/conv-issue-id (or (thing-at-point 'symbol)
                                                         ""))))
                  (read-string (format "Issue (%s): " issue-at-point)
                               nil
                               nil
                               issue-at-point))))
  (if (string-match as-jira/id-regex issue)
      (browse-url (concat as-jira/jira-url "browse/" (as-jira/conv-issue-id issue)))
    (error "Incorrect JIRA issue `%s'" issue)))

;;; as-jira.el ends here
