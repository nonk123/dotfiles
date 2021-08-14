;;; my-exwm.el --- my EXWM ultra-config. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defun execute-command (command)
  "Execute COMMAND asynchronously using the default shell."
  (interactive (list (read-shell-command "$ ")))
  (start-process "*shell*" nil (getenv "SHELL") "-c" command))

(defun lambda-run (command)
  "Return a lambda calling (execute-command COMMAND)."
  (lambda () (interactive) (execute-command command)))

(use-package vterm
  :commands vterm
  :init
  (defun vterm-new-session ()
    (interactive)
    (vterm t)))

(use-package exwm
  :init
  (add-hook 'exwm-update-class-hook
          (lambda ()
            (exwm-workspace-rename-buffer exwm-class-name)))
  (add-hook 'exwm-update-title-hook
            (lambda ()
              (unless exwm-instance-name
                (exwm-workspace-rename-buffer exwm-title))))

  (setq exwm-manage-configurations '((t char-mode t)))
  (setq exwm-workspace-show-all-buffers t)

  (setq
   exwm-input-global-keys
   (mapcar
    (lambda (cell)
      (cons (kbd (car cell)) (cdr cell)))
    `(("s-h" . windmove-left)
      ("s-j" . windmove-down)
      ("s-k" . windmove-up)
      ("s-l" . windmove-right)
      ("s-n" . split-window-below)
      ("s-m" . split-window-right)
      ("s-x" . delete-window)
      ("s-q" . kill-current-buffer)
      ("s-z" . previous-buffer)
      ("s-b" . switch-to-buffer)
      ("s-d" . execute-command)
      ("s-w" . exwm-floating-toggle-floating)
      ("s-f" . exwm-layout-toggle-fullscreen)
      ("s-i" . load-init)
      ("s-<return>" . vterm-new-session)
      ("s-p" . ,(lambda-run "mpc toggle"))
      ,@(mapcar (lambda (i)
                  `(,(format "s-%d" i) .
                    (lambda ()
                      (interactive)
                      (exwm-workspace-switch-create ,i))))
                (number-sequence 0 9))
      ("<print>" . ,(lambda-run "screenshot region"))
      ("S-<print>" . ,(lambda-run "screenshot display")))))

  (require 'exwm-systemtray)
  (exwm-systemtray-enable)
  (exwm-enable)

  ;; Update EXWM keys if they are changed.
  (pcase-dolist (`(,key . ,command) exwm-input-global-keys)
    (exwm-input--set-key key command))
  (exwm-input--update-global-prefix-keys))

(set-frame-font "Hack 9" nil t)
(fringe-mode (cons nil 1))
(scroll-bar-mode -1)

(use-package color-theme-sanityinc-tomorrow
  :init (when first-load
          (color-theme-sanityinc-tomorrow-eighties)))

(provide 'my-exwm)

;;; my-exwm.el ends here
