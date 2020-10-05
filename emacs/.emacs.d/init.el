;;; init.el --- my init file.

;;; Commentary:

;;; Code:

(setq custom-file "~/.emacs.d/custom.el")

;; Create the custom file unless it's already there. Vital for a new install.
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

(add-to-list 'load-path "~/.emacs.d/lisp/")

(defun load-init ()
  "Load all parts of the init file."
  (interactive)
  (load-file custom-file)
  (mapc #'load '("package-init"
                 "utils"
                 "packages"
                 "miscellaneous"
                 "modes"
                 "emux"
                 "my-exwm"
                 "modal")))

(load-init)

;;; init.el ends here
