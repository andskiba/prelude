;;; as-editing.el --- Text editing functions

;;; Commentary:

;;; Code:

(defun as/wrap-in-quotes ()
    "Wrap region in double quotes."
    (interactive)
    (when (region-active-p)
      (let ((beginning (region-beginning))
            (end (region-end)))
        (goto-char end)
        (insert "\"")
        (goto-char beginning)
        (insert "\""))))

(defun as/wrap-word-in-quotes ()
  "Wrap thing at point in double quotes."
  (interactive)
  (let ((thing 'word))
    (beginning-of-thing thing)
    (insert "\"")
    (end-of-thing thing)
    (insert "\"")))

;;; as-editing.el ends here
