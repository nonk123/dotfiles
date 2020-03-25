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
 tab-stop-list nil
 epa-pinentry-mode 'loopback
 vc-follow-symlinks t)

(menu-bar-mode 0)
(tool-bar-mode 0)

(blink-cursor-mode 0)

(setq display-time-day-and-date t)
(display-time-mode 1)

(delete-selection-mode 1)

(electric-pair-mode 1)
(show-paren-mode 1)

(electric-indent-mode 0)

(column-number-mode 1)

;; Breaks everything in TTY version.
(when (bound-and-true-p scroll-bar-mode)
  (scroll-bar-mode 0))

;;; misc.el ends here
