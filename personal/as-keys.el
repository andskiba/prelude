;;; as-keys.el --- Setup key bindings

;;; Commentary:

;;; Code:

(define-key global-map (kbd "C-c m SPC") 'fixup-whitespace)
(define-key global-map (kbd "C-c m h g") 'ahg-status)

(define-key global-map (kbd "C-c m w") 'web-mode)

(define-key global-map (kbd "C-c m t g") 'insert-getter-setter)

(define-key global-map (kbd "C-c m c f") 'as/fb-counter)

(define-key global-map (kbd "C-x o") 'ace-window)

(global-set-key [f6] 'ivy-resume)

;;; as-keys.el ends here
