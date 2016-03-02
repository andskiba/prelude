;;; as-cedet.el --- Load CEDET

;;; Commentary:

;;; Code:

(let ((cedet-path "d:/projects/emacs/cedet/cedet-devel-load.el"))
  (if (file-exists-p cedet-path)
      (load-file cedet-path)))

(provide 'as-cedet)

;;; as-cedet.el ends here
