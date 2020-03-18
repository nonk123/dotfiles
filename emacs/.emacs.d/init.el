;;; init.el --- my init file.

;;; Commentary:

;;; Code:

(setq custom-file "~/.emacs.d/lisp/custom.el")
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
                  "misc"
                  "my-exwm"
                  "hooks"
                  "my-stuff"))
    (load file))
  (when enable-exwm
    (require 'exwm-systemtray)
    (exwm-systemtray-enable)
    (exwm-enable)
    (dolist (binding exwm-input-global-keys)
      (exwm-input-set-key (car binding) (cdr binding)))))

(load-init)

;;; init.el ends here
