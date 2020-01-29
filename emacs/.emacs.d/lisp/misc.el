;;; misc.el --- misc things for the init file.

;;; Commentary:

;;; Code:

(c-add-style "nonk123"
  '("java"
    (c-basic-offset . 4)
    (c-offsets-alist
     (access-label . /)
     (case-label . +))))
(setq c-default-style "nonk123")

(setq-default
  indent-tabs-mode nil
  tab-width 4
  tab-stop-list nil
  epa-pinentry-mode 'loopback
  vc-follow-symlinks t)

(defmacro disable-mode (mode)
  "Disable MODE if it is bound and enabled."
  (when (bound-and-true-p mode)
    `(,mode 0)))

(defmacro enable-mode (mode)
  "Enable MODE if it is bound and disabled."
  (when (and (boundp mode) `(not ,mode))
    `(,mode 1)))

(use-package tramp
  :init (setq tramp-default-method "ssh"))

(use-package whitespace
  :delight
  global-whitespace-mode
  whitespace-mode
  :init
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face trailing tab-mark lines-tail))
  (enable-mode global-whitespace-mode))

(use-package eldoc
  :delight)

(disable-mode menu-bar-mode)
(disable-mode scroll-bar-mode)
(disable-mode tool-bar-mode)

(disable-mode blink-cursor-mode)

(setq display-time-day-and-date t)
(enable-mode display-time-mode)

(enable-mode delete-selection-mode)

(enable-mode electric-pair-mode)
(enable-mode show-paren-mode)

(disable-mode electric-indent-mode)

(enable-mode column-number-mode)

;;; misc.el ends here
