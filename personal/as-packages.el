;;; as-packages.el --- Load packages

;;; Commentary:

;;; Code:

;; Playground



;; Prelude

(prelude-require-package 'php-mode)
(prelude-require-package 'yasnippet)
(prelude-require-package 'apache-mode)
(prelude-require-package 'psvn)
(prelude-require-package 'ahg)
(prelude-require-package 'indent-guide)
(prelude-require-package 'counsel)
(prelude-require-package 'use-package)
(prelude-require-package 'ledger-mode)

;; Ledger

(eval-when-compile
  (require 'use-package))

(use-package ledger-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))
  (setq ledger-post-account-alignment-column 2))

;; Magit

(setq-default magit-last-seen-setup-instructions "1.4.0")

;; ahg

(require 'ahg)

;; helm

(setq-default helm-ff-skip-boring-files t)

;; web-mode

(setq-default web-mode-code-indent-offset 2)
(setq-default web-mode-markup-indent-offset 2)
(setq-default web-mode-css-indent-offset 2)

;;; CSS modes

(setq-default css-indent-offset 4)

;;; JS mode

(setq-default js-indent-level 2)

;; js2-mode

(prelude-require-package 'js2-mode)
(prelude-require-package 'skewer-mode)

(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

(setq-default js2-basic-offset 2)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(js2-imenu-extras-mode)

;;; Typescript

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; format options
(setq tide-format-options
      '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t
        :placeOpenBraceOnNewLineForFunctions nil))

;;; Indent-guide

(require 'indent-guide)
(indent-guide-global-mode)

;;; Java

(when (fboundp 'activate-malabar-mode)
  (add-hook 'after-init-hook
            (lambda ()
              (message "activate-malabar-mode")
              (activate-malabar-mode))))

(add-hook 'malabar-java-mode-hook 'flycheck-mode)
(add-hook 'malabar-groovy-mode-hook 'flycheck-mode)

;;; as-packages.el ends here
