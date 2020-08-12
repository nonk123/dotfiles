;;; package-init.el --- preparations for package.el.

;;; Commentary:

;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (require 'use-package nil t)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;;; package-init.el ends here
