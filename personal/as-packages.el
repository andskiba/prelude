;;; as-packages.el --- Load packages

;;; Commentary:

;;; Code:

(prelude-require-package 'php-mode)
(prelude-require-package 'yasnippet)
(prelude-require-package 'apache-mode)
(prelude-require-package 'psvn)
(prelude-require-package 'ahg)
(prelude-require-package 'indent-guide)

;; magit

(setq magit-last-seen-setup-instructions "1.4.0")

;; ahg

(require 'ahg)

;; helm

(setq helm-ff-skip-boring-files t)

;; web-mode

(setq web-mode-code-indent-offset 2)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)

;;; CSS modes

(setq css-indent-offset 4)

;;; JS mode

(setq js-indent-level 2)

;; js2-mode

(prelude-require-package 'js2-mode)
(prelude-require-package 'skewer-mode)

(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

(setq js2-basic-offset 2)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(js2-imenu-extras-mode)

;;; Indent-guide

(require 'indent-guide)
(indent-guide-global-mode)

;;; Java

(add-hook 'after-init-hook
          (lambda ()
            (message "activate-malabar-mode")
            (activate-malabar-mode)))

(add-hook 'malabar-java-mode-hook 'flycheck-mode)
(add-hook 'malabar-groovy-mode-hook 'flycheck-mode)

;;; as-packages.el ends here
