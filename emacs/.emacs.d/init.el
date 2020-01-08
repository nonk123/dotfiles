;;; init.el --- my init file.

;;; Commentary:

;;; Code:
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(defun load-init ()
  "Load all parts of the init file."
  (interactive)
  (dolist (file '("package-init"
                  "packages"
                  "hooks"
                  "keys"
                  "misc"))
    (load file)))

(load-init)
;;; init.el ends here
