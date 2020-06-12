;;; my-exwm.el --- my exwm config. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun exec ()
  "Execute a command asynchronously without showing its output."
  (interactive)
  (sh (read-shell-command "$ ")))

(defun exec-buf ()
  "Execute a command similarly to `exec', but show its output in a temp buffer."
  (interactive)
  (with-output-to-temp-buffer "*Command output*"
    (sh (or (read-shell-command "$ ") "") t standard-output)))

(defun sh-binding (command)
  `(lambda () (interactive) (sh ,command)))

(defun mpd-binding (mpd-command)
  (sh-binding (format "mpd-control %s" mpd-command)))

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

(defun my-vterm ()
  "Call `vterm' in project root or home directory."
  (interactive)
  (let ((default-directory (or (projectile-project-root) (expand-file-name "~"))))
    (vterm)))

(defun exchange-left ()
  (interactive)
  (exchange-window #'windmove-left))

(defun exchange-down ()
  (interactive)
  (exchange-window #'windmove-down))

(defun exchange-up ()
  (interactive)
  (exchange-window #'windmove-up))

(defun exchange-right ()
  (interactive)
  (exchange-window #'windmove-right))

(defvar window-layout-defs '())

(defvar window-layouts '())

(defvar layout-workspace-mappings '())

(setq window-layout-defs
      '(("d" . ("discord"))
        ("D" . (".+\\.\\(sh|el\\)"))
        ("c" . (".+\\.\\(py|rs|[ch]\\(..\\)?\\)" right "vterm.*" below "vterm.*"))))

(defun move-buffer-to-window (buffer &optional window oldwindow)
  (setq window (window-normalize-window window))
  (switch-to-prev-buffer oldwindow)
  (set-window-buffer window buffer))

(defun generate-layouts ()
  "Return a keymap containing window layouts from `window-layouts'."
  (defun repurpose-buffer-hack (buffer-or-name &rest args)
    (dolist (window window-layouts)
      (setq window (car window))
      (unless (window-live-p window)
        (setq window-layouts (remove (assoc window window-layouts) window-layouts))))
    (let ((buffer (if (stringp buffer-or-name)
                      (get-buffer buffer-or-name)
                    buffer-or-name)))
      (cl-dolist (layout window-layouts)
        (when (string= (buffer-name (window-buffer (car layout))) "*scratch*")
          (when (string-match (cdr layout) (buffer-name buffer))
            (move-buffer-to-window buffer (car layout) (selected-window))
            (exwm-workspace-switch-create (window-frame (car layout)))
            (cl-return))))))
  (advice-add #'rename-buffer :after #'repurpose-buffer-hack)
  (advice-add #'pop-to-buffer :after #'repurpose-buffer-hack)

  (cl-loop
   for (letter . tokens) in window-layout-defs
   with workspace
   do (setq workspace (exwm-workspace-add))
   do (dolist (token tokens)
        (cl-typecase token
          (symbol
           (select-window (funcall (intern (format "split-window-%s" token)))))
          (string
           (add-to-list 'window-layouts (cons (selected-window) token)))))
   do (add-to-list 'layout-workspace-mappings (cons letter workspace))))

(defun select-layout (letter)
  (interactive "clayout: ")
  (unless window-layouts
    (generate-layouts))
  (if-let* ((letter (format "%c" letter))
            (mapping (assoc letter layout-workspace-mappings))
            (workspace (cdr mapping))
            (workspace (and (frame-live-p workspace) workspace)))
      (exwm-workspace-switch workspace)
    (user-error "Not a layout")))

(use-package exwm
  :init
  (setq exwm-workspace-number 10)
  (setq exwm-workspace-show-all-buffers t)
  (setq exwm-layout-show-all-buffers t)
  (setq exwm-input-simulation-keys '(([?\C-c ?\C-c] . ?\C-c)))
  (setq exwm-debug t)
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
          (,(kbd "s-g") . exwm-input-toggle-keyboard)
          (,(kbd "s-i") . load-init)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))
          (,(kbd "s-v") . select-layout)
          (,(kbd "C-c C-c") . exwm-input-send-next-key)
          (,(kbd "<s-return>") . my-vterm)
          (,(kbd "<print>") . ,(sh-binding "screenshot"))
          (,(kbd "s-e") . exec)
          (,(kbd "s-E") . exec-buf)
          (,(kbd "s-P") . find-music)
          (,(kbd "s-p") . ,(mpd-binding "select"))
          (,(kbd "s-,") . ,(mpd-binding "prev"))
          (,(kbd "s-.") . ,(mpd-binding "next"))
          (,(kbd "s-o") . ,(mpd-binding "toggle"))
          (,(kbd "s-;") . ,(mpd-binding "clear"))
          (,(kbd "s-[") . ,(mpd-binding "status"))
          (,(kbd "s-]") . ,(mpd-binding "single"))
          (,(kbd "s-{") . ,(mpd-binding "seek -8"))
          (,(kbd "s-}") . ,(mpd-binding "seek +8")))))

(defun exwm-update-class-actions ()
  (exwm-workspace-rename-buffer exwm-class-name))
(add-hook 'exwm-update-class-hook 'exwm-update-class-actions)

(defun exwm-update-title-actions ()
  (unless exwm-instance-name
    (exwm-workspace-rename-buffer exwm-title)))
(add-hook 'exwm-update-title-hook 'exwm-update-title-actions)

(defvar exwm-enabled nil)

(defun exwm-update-input ()
  (interactive)
  (dolist (binding exwm-input-global-keys)
    (exwm-input--set-key (car binding) (cdr binding))))

(defun exwm-init-actions ()
  (interactive)
  (sh "x-startup" t)
  (use-package modus-vivendi-theme)
  (load-theme 'modus-vivendi t)
  (set-frame-font "Hack 10" nil t)
  (unbind global-map "C-z")
  (setq exwm-enabled t))
(add-hook 'exwm-init-hook 'exwm-init-actions)

(defun exwm-exit-actions ()
  (interactive)
  (setq exwm-enabled nil))
(add-hook 'exwm-exit-hook 'exwm-exit-actions)

(defun start-exwm ()
  (require 'exwm-systemtray)
  (exwm-systemtray-enable)
  (exwm-enable)
  (scroll-bar-mode 0)
  (fringe-mode 0))

(when exwm-enabled
  (exwm-update-input))

;;; my-exwm.el ends here
