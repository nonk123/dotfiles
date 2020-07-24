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

(setq inhibit-startup-message t)

(setq-default
 indent-tabs-mode nil
 tab-stop-list nil
 epa-pinentry-mode 'loopback
 vc-follow-symlinks t
 major-mode #'prog-mode)

(menu-bar-mode 0)
(tool-bar-mode 0)

(blink-cursor-mode 0)

(setq display-time-day-and-date t)
(display-time-mode 1)

(delete-selection-mode 1)

(show-paren-mode 1)

(electric-indent-mode 0)

(column-number-mode 1)

(defun temp-path (name)
  (concat (temporary-file-directory) name))

(defun my-term (&optional command &rest args)
  "Call `ansi-term' in project root or home directory."
  (interactive)
  (let ((default-directory (or (projectile-project-root) (expand-file-name "~")))
        (shell (getenv "SHELL"))
        (program (temp-path "my-term")))
    (if command
        (progn
          (with-temp-file program
            (insert command (string-join args " ")))
          (chmod program #o744)
          (ansi-term program))
      (ansi-term shell))))

;;; misc.el ends here
