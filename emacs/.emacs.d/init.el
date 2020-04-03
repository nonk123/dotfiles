;;; init.el --- my init file.

;;; Commentary:

;;; Code:

(setq custom-file "~/.emacs.d/custom.el")
(unless (file-exists-p custom-file)
  (write-region "" "" custom-file))

(add-to-list 'load-path "~/.emacs.d/lisp/")
(let ((default-directory "~/.emacs.d/my-stuff/"))
  (when (file-exists-p default-directory)
    (normal-top-level-add-subdirs-to-load-path)))

(defun load-init ()
  "Load all parts of the init file."
  (interactive)
  (dolist (file '("package-init"
                  "custom"
                  "utils"
                  "packages"
                  "keys"
                  "miscellaneous"
                  "my-exwm"
                  "my-stuff"))
    (load file t)))

(load-init)

;;; init.el ends here
