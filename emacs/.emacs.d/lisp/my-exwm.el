;;; my-exwm.el --- my exwm config.

;;; Commentary:

;;; Code:

(defun exec ()
  "Execute a command asynchronously without showing its output."
  (interactive)
  (sh (or (read-string "$ ") "")))

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

(defun mpd-interactive ()
  (interactive)
  (sh "mpd-control interactive"))

(defun mpd-toggle ()
  (interactive)
  (sh "mpd-control toggle"))

(defun mpd-info ()
  (interactive)
  (sh "notify-send \"Info\" \"$(mpd-control)\""))

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

(use-package exwm
  :init
  (setq exwm-workspace-number 10)
  (setq exwm-workspace-current-index 1)
  (setq exwm-workspace-show-all-buffers t)
  (setq exwm-layout-show-all-buffers t)
  (setq exwm-manage-configurations '((t char-mode t)))
  (setq exwm-input-simulation-keys '(([?\C-c ?\C-c] . ?\C-c)))
  (setq exwm-input-global-keys
        `((,(kbd "s-h") . windmove-left)
          (,(kbd "s-j") . windmove-down)
          (,(kbd "s-k") . windmove-up)
          (,(kbd "s-l") . windmove-right)
          (,(kbd "s-n") . split-window-below)
          (,(kbd "s-m") . split-window-right)
          (,(kbd "s-w") . delete-window)
          (,(kbd "s-q") . force-kill-current-buffer)
          (,(kbd "s-b") . switch-to-buffer)
          (,(kbd "s-f") . exwm-layout-toggle-fullscreen)
          (,(kbd "s-r") . exwm-floating-toggle-floating)
          (,(kbd "s-g") . exwm-input-toggle-keyboard)
          ,@(mapcar (lambda (i)
                     `(,(kbd (format "s-%d" i)) .
                       (lambda ()
                         (interactive)
                         (exwm-workspace-switch-create ,i))))
                   (number-sequence 0 9))
          (,(kbd "C-c C-c") . exwm-input-send-next-key)
          (,(kbd "<s-return>") . x-terminal-emulator)
          (,(kbd "<print>") . screenshot)
          (,(kbd "s-e") . exec)
          (,(kbd "s-E") . exec-buf)
          (,(kbd "s-b") . browser)
          (,(kbd "s-d") . helm-dmenu)
          (,(kbd "s-p") . mpd-select)
          (,(kbd "s-;") . mpd-interactive)
          (,(kbd "s-o") . mpd-toggle)
          (,(kbd "s-[") . mpd-info)
          (,(kbd "s-{") . mpd-rewind)
          (,(kbd "s-}") . mpd-forward)))
  (require 'exwm-systemtray)
  (exwm-systemtray-enable)
  (exwm-enable))

(defun exwm-update-class-actions ()
  (exwm-workspace-rename-buffer exwm-class-name))
(add-hook 'exwm-update-class-hook 'exwm-update-class-actions)

(defun exwm-update-title-actions ()
  (unless exwm-instance-name
    (exwm-workspace-rename-buffer exwm-title)))
(add-hook 'exwm-update-title-hook 'exwm-update-title-actions)

(defun exwm-init-actions ()
  "Actions to perform upon starting EXWM."
  (interactive)
  (sh "x-startup" t)
  (set-frame-font (x-get-resource "font" "emacs") nil t)
  (unbind global-map "C-z"))
(add-hook 'exwm-init-hook 'exwm-init-actions)

;;; my-exwm.el ends here
