(defun exchange-window (move-function &rest args)
  "Return an exchange buffers function calling MOVE-FUNCTION with optional ARGS."
  (lambda ()
    (interactive)
    (let ((old-buffer (current-buffer))
          (old-window (selected-window)))
      (funcall move-function args)
      (set-window-buffer old-window (current-buffer))
      (set-window-buffer (selected-window) old-buffer))))

(defun emux-ssh (hostname)
  (let ((connector (temp-path "emux-connector")))
    (with-temp-file connector
      (insert
       "eval \$(ssh-agent)\n"
       (format "ssh -AY %s\n" hostname)
       "kill \$SSH_AGENT_PID\n"))
    (chmod connector #o744)
    (my-term connector)))

(defun emux-connect ()
  (interactive)
  (when-let ((hosts '(("Tilde" . "nonk@tilde.as205315.net")
                      ("Music" . "music@185.222.117.80")))
             (hostname (helm (helm-build-sync-source "SSH Endpoints"
                               :candidates hosts))))
    (emux-ssh hostname)))

(defvar emux-prefix-keymap
  (bind (make-sparse-keymap)
        `(("t" . emux-connect)
          ("h" . windmove-left)
          ("j" . windmove-down)
          ("k" . windmove-up)
          ("l" . windmove-right)
          ("C-h" . ,(exchange-window #'windmove-left))
          ("C-j" . ,(exchange-window #'windmove-down))
          ("C-k" . ,(exchange-window #'windmove-up))
          ("C-l" . ,(exchange-window #'windmove-right))
          ("," . previous-buffer)
          ("." . next-buffer))))

(bind global-map `(("C-t" . ,emux-prefix-keymap)))
