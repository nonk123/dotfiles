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

(defun emacs.d (file)
  "Return FILE relative to `user-emacs-directory'."
  (expand-file-name file user-emacs-directory))

(defun ensure (file)
  "Ensure FILE exists.  If it doesn't, \"touch\" it."
  (unless (file-exists-p file)
    (write-region "" nil file)))

(setq custom-file (emacs.d "custom.el"))
(ensure custom-file)

(defun load-init ()
  "Load emacs-init.org."
  (interactive)
  (load-file custom-file)
  (let* ((init (emacs.d "emacs-init"))
         (el (concat init ".el"))
         (org (concat init ".org")))
    (and (file-exists-p el) (delete-file el))
    (org-babel-load-file org)))

(load-init)

;;; init.el ends here
