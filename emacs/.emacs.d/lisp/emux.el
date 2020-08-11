;;; emux.el --- terminal multiplexing for SSH. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun exchange-window (move-function &rest args)
  "Return an exchange buffers function calling MOVE-FUNCTION with optional ARGS."
  (lambda ()
    (interactive)
    (let ((old-buffer (current-buffer))
          (old-window (selected-window)))
      (funcall move-function args)
      (set-window-buffer old-window (current-buffer))
      (set-window-buffer (selected-window) old-buffer))))

(defvar emux-map
  `(("C-t" . (("c" . emux-connect)
              ("t" . my-term)
              ("q" . force-kill-buffer)
              ("x" . delete-window)
              ("b" . switch-to-buffer)
              ("h" . windmove-left)
              ("j" . windmove-down)
              ("k" . windmove-up)
              ("l" . windmove-right)
              ("C-h" . ,(exchange-window #'windmove-left))
              ("C-j" . ,(exchange-window #'windmove-down))
              ("C-k" . ,(exchange-window #'windmove-up))
              ("C-l" . ,(exchange-window #'windmove-right))
              ("%" . split-window-right)
              ("\"" . split-window-below)
              ("," . previous-buffer)
              ("." . next-buffer)))))

(defun my-term (&optional command &rest args)
  "Call `ansi-term' in project root or home directory."
  (interactive)
  (let* ((default-directory (or (projectile-project-root) (expand-file-name "~")))
         (shell (getenv "SHELL"))
         (shell (concat shell " -l"))
         (program (temp-path "my-term")))
    (with-temp-file program
      (if command
          (insert command (string-join args " "))
        (insert shell)))
    (chmod program #o744)
    (ansi-term program)))

(defun emux-ssh (hostname)
  (let ((connector (temp-path "emux-connector")))
    (with-temp-file connector
      (insert
       (string-join
        (list
         "eval \$(ssh-agent)"
         (format "ssh -tAY %s \"emacsclient -c\"" hostname)
         "kill \$SSH_AGENT_PID")
        "\n")))
    (chmod connector #o744)
    (my-term connector)
    (emux-mode -1)))

(defun emux-connect ()
  (interactive)
  (when-let ((hosts '(("Tilde" . "nonk@tilde.as205315.net")
                      ("Music" . "music@185.222.117.80")))
             (hostname (helm (helm-build-sync-source "SSH Endpoints"
                               :candidates hosts))))
    (emux-ssh hostname)))

(defun force-kill-buffer ()
  "Kill this buffer even if it has a process running."
  (interactive)
  (let ((kill-buffer-query-functions
         (delq 'process-kill-buffer-query-function kill-buffer-query-functions)))
    (kill-this-buffer)))

(define-minor-mode emux-mode
  "Emux keybindings mode."
  :init-value t
  :lighter " Emux"
  :keymap (bind (make-sparse-keymap) emux-map))

;;; emux.el ends here
