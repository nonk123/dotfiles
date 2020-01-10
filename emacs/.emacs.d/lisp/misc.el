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
  show-trailing-whitespace t
  epa-pinentry-mode 'loopback
  vc-follow-symlinks t)

(setq whitespace-line-column 80)
(setq whitespace-style '(face lines-tail))
(whitespace-mode 1)

(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)

(blink-cursor-mode 0)

(delete-selection-mode 1)

(electric-pair-mode 1)
(show-paren-mode 1)

(global-subword-mode 1)

(electric-indent-mode 0)

(column-number-mode 1)
;;; misc.el ends here
