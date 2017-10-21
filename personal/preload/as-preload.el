;;; as-preload.el --- settings to load before prelude

;;; Commentary:

;;; Code:

(require 'cl-lib)

;; Make flycheck use regular `load-path' for `require' calls in elisp

(setq-default flycheck-emacs-lisp-load-path 'inherit)

;; Emacs location

;; (defun as-preload--emacs-dir ()
;;   "Get directory from which Emacs was run."
;;   (let ((emacs-bin default-directory))
;;     (cl-reduce (lambda (a b) (concat a "/" b))
;; 	       (butlast (split-string emacs-bin "\\\\") 1))))

;; (defvar emacs-location (as-preload--emacs-dir))

;;; as-preload.el ends here
