;;; as-cedet.el --- Load CEDET

;;; Commentary:

;;; Code:

(let ((cedet-file "d:/projects/emacs/cedet/cedet-devel-load.el"))
  (if (file-exists-p cedet-file)
      (load-file cedet-file)))

(provide 'as-cedet)

;;; as-cedet.el ends here
