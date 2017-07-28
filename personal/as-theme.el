;;; as-theme.el --- My look and feel

;;; Commentary:

;;; Code:

(defvar as-fonts '("Source Code Pro-12" "Consolas-12" "DejaVu Sans Mono-11"))

(defun as-preferred-font ()
  (or (first (seq-drop-while (lambda (elt) (null (x-list-fonts elt)))
                             as-fonts))
      "Monospace-12"))

(set-face-attribute 'default nil :font (as-preferred-font))


(disable-theme 'zenburn)
(load-theme 'deeper-blue)

(provide 'as-theme)

;;; as-theme.el ends here
