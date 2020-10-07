;;; my-exwm.el --- my exwm config. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;; Keybindings.

(defun exec ()
  "Execute a command asynchronously."
  (interactive)
  (sh (read-shell-command "$ ") 0))

(defun exec-buf ()
  "Execute a command similarly to `exec', showing its output in a buffer."
  (interactive)
  (with-output-to-temp-buffer "*Command output*"
    (sh (read-shell-command "$ ") standard-output)))

(defun sh-binding (command)
  "Return a lambda running `sh' with COMMAND.  Useful for setting up keys."
  `(lambda () (interactive) (sh ,command 0)))

(defun mpd-binding (command &rest args)
  "Like `sh-binding', but call `mpd-control' with COMMAND and ARGS."
  (sh-binding (format "mpd-control %s %s" command (string-join args " "))))

(defun download-track (url)
  "Download a music track from URL, using `mpd-control'."
  (interactive "sURL: ")
  ;; Quote the URL in case it contains weird characters.
  (funcall (mpd-binding "download" (format "'%s'" url))))

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
                ("Torrent"    . "transmission-gtk")
                ("PA Volume"  . "pavucontrol")
                ("Zoom"       . "zoom")
                ("Aria"       . "Aria")))
       (utils '(("Restart fluidsynth" . "systemctl --user restart fluidsynth")
                ("Restart mpd" . "systemctl --user restart mpd && mpd-control play")
                ("SSH proxy to tilde" . "ssh -ND 9050 nonk@tilde.as205315.net")
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

;;;; Window layouts.

(defvar window-layout-defs
  '(("d" . ())
    ("b" . ())
    ("c" . (:right my-term :below my-term))
    ("t" . (my-term))
    ("i" . ())
    ("1" . ())
    ("2" . ())
    ("3" . ()))
  "Description of window layouts created on EXWM startup.")

(defvar layout-mappings '())

(defun generate-layouts ()
  "Generate EXWM frames from `window-layout-defs'."
  (dolist (def window-layout-defs)
    (set-buffer "*scratch*") ; *scratch* is the starting buffer
    (let ((workspace (exwm-workspace-add)))
      (dolist (token (cdr def))
        (cl-typecase token
          (keyword        ; :right and :below just split the current window
           (select-window ; this makes the split window current
            (cond
             ((eq token :right)
              (split-window-right))
             ((eq token :below)
              (split-window-below))
             (t
              (user-error "Unrecognized token: %s" token)))))
          (t
           (funcall token)))) ; call symbols and lambdas (untested)
      ;; Save the frame for use in `select-layout'.
      (add-to-list 'layout-mappings (cons (car def) workspace)))))

(defun select-layout ()
  "Ask user to select a layout from `layout-mappings'."
  (interactive)
  (unless layout-mappings ; create the frames on first call
    (generate-layouts))
  (if-let* ((letter (read-key "Layout: "))
            (letter (key-description (list letter))) ; strings are used as keys
            (mapping (assoc letter layout-mappings))
            (workspace (cdr mapping))
            ;; Bail out if the frame is dead.
            (workspace (when (frame-live-p workspace) workspace)))
      (exwm-workspace-switch workspace)
    (user-error "Not a layout")))

(defun copy-10 ()
  "Enter a Unicode character's base-10 value and copy it to clipboard."
  (interactive)
  (with-temp-buffer
    (let ((read-quoted-char-radix 10))
      (quoted-insert 1)
      (clipboard-kill-ring-save (point-min) (point-max)))))

(defun copy-unicode ()
  "Search for a Unicode character and copy it to clipboard."
  (interactive)
  (with-temp-buffer
    (call-interactively #'insert-char)
    (clipboard-kill-ring-save (point-min) (point-max))))

;;;; `use-package' declaration.

(use-package exwm
  :init
  ;; Easy window-switching with s-b.
  (setq exwm-workspace-show-all-buffers t)
  (setq exwm-layout-show-all-buffers t)
  ;; C-c is a prefix key; send ^C with C-c C-c.
  (setq exwm-input-simulation-keys '(([?\C-c ?\C-c] . ?\C-c)))
  (setq exwm-input-global-keys
        (mapcar
         (lambda (binding)
           (cons (kbd (car binding)) (cdr binding)))
         `(("s-h" . windmove-left) ; basic movement keys
           ("s-j" . windmove-down)
           ("s-k" . windmove-up)
           ("s-l" . windmove-right)
           ("C-s-h" . ,(exchange-window #'windmove-left)) ; swap buffers
           ("C-s-j" . ,(exchange-window #'windmove-down))
           ("C-s-k" . ,(exchange-window #'windmove-up))
           ("C-s-l" . ,(exchange-window #'windmove-right))
           ("M-s-h" . shrink-window-horizontally) ; weird resize
           ("M-s-j" . enlarge-window)
           ("M-s-k" . shrink-window)
           ("M-s-l" . enlarge-window-horizontally)
           ("s-n" . split-window-below) ; obviously, window-splits
           ("s-m" . split-window-right)
           ("s-w" . delete-window)     ; kill a window (buffer is left behind)
           ("s-q" . force-kill-buffer) ; kill a buffer
           ("s-b" . switch-to-buffer)  ; select a buffer
           ("s-f" . exwm-layout-toggle-fullscreen) ; window modes
           ("s-r" . exwm-floating-toggle-floating)
           ("s-g" . exwm-input-toggle-keyboard) ; char and line mode switch
           ("s-i" . load-init)     ; reload init file
           ("s-v" . select-layout) ; change workspace
           ("s-'" . copy-unicode)  ; weird stuff
           ("s-0" . copy-10)
           ("s-s" . exwm-input-send-next-key)
           ("s-e" . exec)
           ("s-E" . exec-buf)
           ("<s-return>" . my-term) ; common programs
           ("<print>" . ,(sh-binding "screenshot"))
           ("s-d" . quick) ; dmenu-like prompt
           ("s-p" . ,(mpd-binding "select")) ; music
           ("s-," . ,(mpd-binding "prev"))
           ("s-." . ,(mpd-binding "next"))
           ("s-o" . ,(mpd-binding "toggle"))
           ("s-;" . ,(mpd-binding "clear"))
           ("s-[" . ,(mpd-binding "status"))
           ("s-]" . ,(mpd-binding "single"))
           ("s-ä" . ,(mpd-binding "seek -8"))
           ("s-$" . ,(mpd-binding "seek +8"))))))

;;;; EXWM magic.

(defun exwm-update-class-actions ()
  (unless exwm-title
    (exwm-workspace-rename-buffer exwm-class-name)))
(add-hook 'exwm-update-class-hook #'exwm-update-class-actions)

(defun exwm-update-title-actions ()
  (exwm-workspace-rename-buffer exwm-title))
(add-hook 'exwm-update-title-hook #'exwm-update-title-actions)

(defvar exwm-enabled nil
  "Non-nil if `start-exwm' was run.")

(defun exwm-update-input ()
  "Re-bind EXWM keys after `exwm-input-global-keys' update."
  (dolist (binding exwm-input-global-keys)
    (exwm-input--set-key (car binding) (cdr binding))))

(defun exwm-init-actions ()
  "Run when the EXWM session is initialized."
  (use-package humanoid-themes)
  (load-theme 'humanoid-dark t)
  (set-frame-font "Hack 10" nil t)
  (unbind global-map "C-z") ; can't hide EXWM frames
  (setq exwm-enabled t))
(add-hook 'exwm-init-hook #'exwm-init-actions)

(defun exwm-exit-actions ()
  (setq exwm-enabled nil))
(add-hook 'exwm-exit-hook #'exwm-exit-actions)

(defun start-exwm ()
  "Start EXWM session.  Used in .xinitrc."
  (require 'exwm-systemtray)
  (exwm-systemtray-enable)
  (exwm-enable)
  (scroll-bar-mode 0)
  (fringe-mode 0))

;; If EXWM is still running, re-bind the keys.
(when exwm-enabled
  (exwm-update-input))

;;; my-exwm.el ends here
