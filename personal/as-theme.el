;;; as-theme.el --- My look and feel

;;; Commentary:

;;; Code:

(defvar as-font (if (eq system-type 'gnu/linux)
                    "DejaVu Sans Mono-11"
                  "Consolas-12"))

(set-face-attribute 'default nil :font as-font)

(disable-theme 'zenburn)
(load-theme 'deeper-blue)

(provide 'as-theme)

;;; as-theme.el ends here
