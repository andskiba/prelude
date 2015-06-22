;;; as-keys.el --- Setup key bindings

;;; Commentary:

;;; Code:

(define-key global-map (kbd "C-c m SPC") 'fixup-whitespace)
(define-key global-map (kbd "C-c m h g") 'ahg-status)

(define-key global-map (kbd "C-c m w") 'web-mode)

;;; as-keys.el ends here
