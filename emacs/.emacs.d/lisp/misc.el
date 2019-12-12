;;; misc.el --- misc things for the init file.

;;; Commentary:

;;; Code:
(c-add-style "nonk123"
  '("java"
    (c-basic-offset . 4)
    (c-offsets-alist
     (access-label . /))))
(setq c-default-style "nonk123")

(setq-default
  indent-tabs-mode nil
  tab-width 4
  tab-stop-list nil
  show-trailing-whitespace t
  epa-pinentry-mode 'loopback
  vc-follow-symlinks t)

(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)

(blink-cursor-mode 0)

(show-paren-mode 1)

(delete-selection-mode 1)
(electric-indent-mode 0)

(column-number-mode 1)

(provide 'misc)
;;; misc.el ends here
