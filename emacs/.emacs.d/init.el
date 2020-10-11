;;; init.el --- my init file.

;;; Commentary:

;;; Code:

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Workaround for Emacs 26; GNU package archive won't work otherwise.
(when (= emacs-major-version 26)
  (defvar gnutls-algorithm-priority) ; avoid a warning in later versions
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(package-initialize)

;; Make sure `use-package' is always installed.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

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
