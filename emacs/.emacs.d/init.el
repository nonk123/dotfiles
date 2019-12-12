;;; init.el --- my init file.

;;; Commentary:

;;; Code:
(setq custom-file "~/.emacs.d/custom.el")
(load-file custom-file)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'package-init)
(require 'packages)
(require 'hooks)
(require 'keys)
(require 'misc)
;;; init.el ends here
