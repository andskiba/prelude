;;; as-misc.el -- miscellaneous modifications

;;; Commentary:

;;; Code:

(prefer-coding-system 'utf-8-unix)

(if (eq system-type 'windows-nt)
    (progn
      (setq exec-path (add-to-list 'exec-path "C:/Program Files (x86)/Git/bin"))
      (setenv "PATH" (concat "C:\\Program Files (x86)\\Git\\bin;" (getenv "PATH")))))

(server-start)

;;; as-misc.el ends here
