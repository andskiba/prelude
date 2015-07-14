;;; as-windows.el --- Windows specific configuration

;;; Commentary:

;;; Code:

(when (eq system-type 'windows-nt)
  ;; Add emacs dir to info directories because for some reason the
  ;; default path that uses a variable does not work.
  ;; `emacs-location' is defined in as-preload.el
  (add-to-list 'Info-directory-list
               (concat emacs-location "/share/info")))

(provide 'as-windows)

;;; as-windows.el ends here
