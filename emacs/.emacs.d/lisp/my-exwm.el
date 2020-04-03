;;; my-exwm.el --- my exwm config.

;;; Commentary:

;;; Code:

(defun exec ()
  "Execute a command asynchronously without showing its output."
  (interactive)
  (sh (read-shell-command "$ ")))

(defun exec-buf ()
  "Execute a command similar to `exec', but show its output in a temp buffer."
  (interactive)
  (with-output-to-temp-buffer "*Command output*"
    (let (output)
      (with-temp-buffer
        (sh (or (read-string "$ ") "") t (current-buffer))
        (setq output (buffer-string)))
      (princ output))))

(defun browser ()
  (sh "qutebrowser"))

(defun helm-dmenu ()
  "Show a dmenu-like prompt using `helm'."
  (interactive)
  (sh (or (helm :sources (helm-build-sync-source "helm-dmenu"
                           :candidates (sh-output-lines "dmenu_path"))
                :prompt "$ "
                :buffer "*helm-dmenu*")
          "")))

(defun x-terminal-emulator ()
  (interactive)
  (sh "x-terminal-emulator"))

(defun screenshot ()
  (interactive)
  (sh "screenshot"))

(defun mpd-select ()
  (interactive)
  (sh "mpd-control select"))

(defun mpd-queue ()
  (interactive)
  (sh "mpd-control queue"))

(defun mpd-prev ()
  (interactive)
  (sh "mpd-control prev"))

(defun mpd-next ()
  (interactive)
  (sh "mpd-control next"))

(defun mpd-interactive ()
  (interactive)
  (sh "mpd-control interactive"))

(defun mpd-toggle ()
  (interactive)
  (sh "mpd-control toggle"))

(defun mpd-status ()
  (interactive)
  (sh "mpd-control status"))

(defun mpd-single ()
  (interactive)
  (sh "mpd-control single"))

(defun mpd-rewind ()
  (interactive)
  (sh "mpd-control seek -8"))

(defun mpd-forward ()
  (interactive)
  (sh "mpd-control seek +8"))

(defun force-kill-current-buffer ()
  "Kill the current buffer even if it has a process running."
  (interactive)
  (let ((process (get-buffer-process (current-buffer))))
    (when (processp process)
      (kill-process process))
    (kill-current-buffer)))

(defun exchange-window (move-function &rest args)
  "Exchange buffers in windows using MOVE-FUNCTION with optional ARGS."
  (let ((old-buffer (current-buffer))
        (old-window (selected-window)))
    (funcall move-function args)
    (set-window-buffer old-window (current-buffer))
    (set-window-buffer (selected-window) old-buffer)))

(defun exchange-left ()
  (interactive)
  (exchange-window 'windmove-left))

(defun exchange-down ()
  (interactive)
  (exchange-window 'windmove-down))

(defun exchange-up ()
  (interactive)
  (exchange-window 'windmove-up))

(defun exchange-right ()
  (interactive)
  (exchange-window 'windmove-right))

(use-package exwm
  :init
  (setq exwm-workspace-number 10)
  (setq exwm-workspace-current-index 1)
  (setq exwm-workspace-show-all-buffers t)
  (setq exwm-layout-show-all-buffers t)
  (setq exwm-input-simulation-keys '(([?\C-c ?\C-c] . ?\C-c)))
  (setq exwm-input-global-keys
        `((,(kbd "s-h") . windmove-left)
          (,(kbd "s-j") . windmove-down)
          (,(kbd "s-k") . windmove-up)
          (,(kbd "s-l") . windmove-right)
          (,(kbd "C-s-h") . exchange-left)
          (,(kbd "C-s-j") . exchange-down)
          (,(kbd "C-s-k") . exchange-up)
          (,(kbd "C-s-l") . exchange-right)
          (,(kbd "M-s-h") . shrink-window-horizontally)
          (,(kbd "M-s-j") . enlarge-window)
          (,(kbd "M-s-k") . shrink-window)
          (,(kbd "M-s-l") . enlarge-window-horizontally)
          (,(kbd "s-n") . split-window-below)
          (,(kbd "s-m") . split-window-right)
          (,(kbd "s-w") . delete-window)
          (,(kbd "s-q") . force-kill-current-buffer)
          (,(kbd "s-b") . switch-to-buffer)
          (,(kbd "s-f") . exwm-layout-toggle-fullscreen)
          (,(kbd "s-r") . exwm-floating-toggle-floating)
          (,(kbd "s-t") . exwm-reset)
          (,(kbd "s-g") . exwm-input-toggle-keyboard)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))
          (,(kbd "C-c C-c") . exwm-input-send-next-key)
          (,(kbd "<s-return>") . vterm)
          (,(kbd "<print>") . screenshot)
          (,(kbd "s-e") . exec)
          (,(kbd "s-E") . exec-buf)
          (,(kbd "s-b") . browser)
          (,(kbd "s-d") . helm-dmenu)
          (,(kbd "s-p") . mpd-select)
          (,(kbd "s-P") . mpd-queue)
          (,(kbd "s-,") . mpd-prev)
          (,(kbd "s-.") . mpd-next)
          (,(kbd "s-;") . mpd-interactive)
          (,(kbd "s-o") . mpd-toggle)
          (,(kbd "s-[") . mpd-status)
          (,(kbd "s-]") . mpd-single)
          (,(kbd "s-{") . mpd-rewind)
          (,(kbd "s-}") . mpd-forward))))

(defun exwm-update-class-actions ()
  (exwm-workspace-rename-buffer exwm-class-name))
(add-hook 'exwm-update-class-hook 'exwm-update-class-actions)

(defun exwm-update-title-actions ()
  (unless exwm-instance-name
    (exwm-workspace-rename-buffer exwm-title)))
(add-hook 'exwm-update-title-hook 'exwm-update-title-actions)

(defvar exwm-enabled nil)

(defun exwm-init-actions ()
  (interactive)
  (sh "x-startup" t)
  (use-package atom-one-dark-theme)
  (load-theme 'atom-one-dark t)
  (unbind global-map "C-z")
  (setq exwm-enabled t))
(add-hook 'exwm-init-hook 'exwm-init-actions)

(defun exwm-exit-actions ()
  (interactive)
  (setq exwm-enabled nil))
(add-hook 'exwm-exit-hook 'exwm-exit-actions)

(defun start-exwm ()
  (scroll-bar-mode 0)
  (require 'exwm-systemtray)
  (exwm-systemtray-enable)
  (exwm-enable)
  (dolist (binding exwm-input-global-keys)
    (exwm-input--set-key (car binding) (cdr binding))))

(when exwm-enabled
  (start-exwm)
  (set-frame-font "Hack 10" nil t))

;;; my-exwm.el ends here
