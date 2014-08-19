;;; as-packages.el --- Load packages

;;; Commentary:

;;; Code:

(prelude-require-package 'php-mode)
(prelude-require-package 'apache-mode)
(prelude-require-package 'psvn)
(prelude-require-package 'ahg)

(require 'ahg)

;;; CSS modes

(setq css-indent-offset 4)

;;; as-packages.el ends here
