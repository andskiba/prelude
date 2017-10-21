;;; as-class-tools.el --- Class tools

;;; Commentary:

;;; Code:

(require 'string-inflection)
(require 'thingatpt)

(defun insert-getter-setter (type)
  "Insert getter/setter for word at point asking for TYPE."
  (interactive (list (read-string (format "Type (String): ")
                                  nil nil "String")))
  (let ((name (string-inflection-get-current-word t)))
    (insert-getter name type)
    (newline)
    (insert-setter name type)
    (newline)
    (forward-line)))

(defun insert-getter (name type)
  "Insert a Java getter for given NAME and TYPE."
  (insert type " get" (string-inflection-camelcase-function name)  "();"))

(defun insert-setter (name type)
  "Insert a Java setter for given NAME and TYPE."
  (insert "void set" (string-inflection-camelcase-function name)
          "(" type " " (string-inflection-lower-camelcase-function name)");"))

(provide 'as-class-tools)

;;; as-class-tools.el ends here
