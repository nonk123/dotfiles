;;; my-exwm.el --- my exwm config. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun exec ()
  "Execute a command asynchronously without showing its output."
  (interactive)
  (sh (read-shell-command "$ ") 0))

(defun exec-buf ()
  "Execute a command similarly to `exec', but show its output in a temp buffer."
  (interactive)
  (with-output-to-temp-buffer "*Command output*"
    (sh (or (read-shell-command "$ ") "") standard-output)))

(defun sh-binding (command)
  `(lambda () (interactive) (sh ,command 0)))

(defun mpd-binding (mpd-command &rest args)
  (sh-binding
   (format "mpd-control %s %s"
           mpd-command
           (string-join (mapcar (lambda (arg) (format "'%s'" arg)) args) " "))))

(defun download-track (url)
  "Download a music track with specified URL using `mpd-control'."
  (interactive "sURL: ")
  (funcall (mpd-binding "download" url)))

(defun quick ()
  "Launch a program or utility from a `helm' listing."
  (interactive)
  (when-let*
      ((quick '(("Browser"    . "qutebrowser --no-err-windows")
                ("Discord"    . "discord")
                ("Parsec"     . "parsecd app_daemon=1")
                ("Moonlight"  . "moonlight")
                ("Steam"      . "steam")
                ("Doomseeker" . "doomseeker")
                ("Aria"       . "Aria")))
       (utils '(("Restart fluidsynth" . "systemctl --user restart fluidsynth")
                ("Restart mpd" . "systemctl --user restart mpd && mpd-control play")
                ("SSH proxy to tilde" . "ssh -ND 9090 nonk@tilde.as205315.net")
                ("Kill all SSH connections" . "pkill ssh")))
       (command (helm
                 :prompt "Launch: "
                 :buffer "*Program selection*"
                 :sources
                 (vector
                  (helm-build-sync-source "Quick launch"
                    :candidates quick)
                  (helm-build-sync-source "Utilities"
                    :candidates utils)))))
    (sh command 0)))

(defvar window-layout-defs
  '(("d" . ())
    ("b" . ())
    ("c" . (:right my-term :below my-term))
    ("t" . (my-term))
    ("i" . ())
    ("1" . ())
    ("2" . ())
    ("3" . ())))

(defvar layout-mappings '())

(defun generate-layouts ()
  "Generate EXWM frames from `window-layout-defs'."
  (dolist (def window-layout-defs)
    (set-buffer "*scratch*")
    (let ((workspace (exwm-workspace-add)))
      (dolist (token (cdr def))
        (cl-typecase token
          (keyword
           (select-window
            (cond
             ((eq token :right)
              (split-window-right))
             ((eq token :below)
              (split-window-below))
             (t
              (user-error "Unrecognized token: %s" token)))))
          (symbol
           (funcall token))))
      (add-to-list 'layout-mappings (cons (car def) workspace)))))

(defun select-layout (&optional show-layouts)
  (interactive)
  (unless layout-mappings
    (generate-layouts))
  (if-let* ((layouts (string-join (mapcar #'car window-layout-defs) " "))
            (layouts (format " (%s)" layouts))
            (prompt (format "Layout%s: " (if show-layouts layouts "")))
            (letter (read-char prompt))
            (letter (format "%c" letter))
            (mapping (assoc letter layout-mappings))
            (workspace (cdr mapping))
            (workspace (and (frame-live-p workspace) workspace)))
      (exwm-workspace-switch workspace)
    (if (and (string= letter "?") (not show-layouts))
        (select-layout t)
      (user-error "Not a layout"))))

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
          (,(kbd "C-s-h") . ,(exchange-window #'windmove-left))
          (,(kbd "C-s-j") . ,(exchange-window #'windmove-down))
          (,(kbd "C-s-k") . ,(exchange-window #'windmove-up))
          (,(kbd "C-s-l") . ,(exchange-window #'windmove-right))
          (,(kbd "M-s-h") . shrink-window-horizontally)
          (,(kbd "M-s-j") . enlarge-window)
          (,(kbd "M-s-k") . shrink-window)
          (,(kbd "M-s-l") . enlarge-window-horizontally)
          (,(kbd "s-n") . split-window-below)
          (,(kbd "s-m") . split-window-right)
          (,(kbd "s-w") . delete-window)
          (,(kbd "s-q") . force-kill-buffer)
          (,(kbd "s-b") . switch-to-buffer)
          (,(kbd "s-f") . exwm-layout-toggle-fullscreen)
          (,(kbd "s-r") . exwm-floating-toggle-floating)
          (,(kbd "s-g") . exwm-input-toggle-keyboard)
          (,(kbd "s-i") . load-init)
          (,(kbd "s-v") . select-layout)
          (,(kbd "C-c C-c") . exwm-input-send-next-key)
          (,(kbd "<s-return>") . my-term)
          (,(kbd "<print>") . ,(sh-binding "screenshot"))
          (,(kbd "s-e") . exec)
          (,(kbd "s-E") . exec-buf)
          (,(kbd "s-P") . find-music)
          (,(kbd "s-d") . quick)
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
  (unless exwm-title
    (exwm-workspace-rename-buffer exwm-class-name)))
(add-hook 'exwm-update-class-hook 'exwm-update-class-actions)

(defun exwm-update-title-actions ()
  (exwm-workspace-rename-buffer exwm-title))
(add-hook 'exwm-update-title-hook 'exwm-update-title-actions)

(defvar exwm-enabled nil)

(defun exwm-update-input ()
  (interactive)
  (dolist (binding exwm-input-global-keys)
    (exwm-input--set-key (car binding) (cdr binding))))

(defun exwm-init-actions ()
  (interactive)
  (sh "x-startup" nil)
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
