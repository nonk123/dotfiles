;;; init.el --- my init file.

;;; Commentary:

;;; Code:

(setq custom-file "~/.emacs.d/lisp/custom.el")
(unless (file-exists-p custom-file)
  (write-region "" "" custom-file))

(add-to-list 'load-path "~/.emacs.d/lisp/")

(defun load-init ()
  "Load all parts of the init file."
  (interactive)
  (dolist (file '("package-init"
                  "custom"
                  "packages"
                  "keys"
                  "misc"
                  "hooks"))
    (load file)))

(load-init)

;;; init.el ends here
