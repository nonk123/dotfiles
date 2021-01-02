;;; mpc-gui.el --- my epic GUI for `mpc'.

;;; Commentary:

;; An MPD GUI modelled after `simple-mpc'.
;;
;; Now talks over the TCP protocol, without `mpc'.

;;; Code:

(require 'subr-x)

;;;; Variables

(defvar mpc-gui-reconnect-attempts 10
  "Try to reconnect to MPD this many times if something goes wrong.")

(defvar mpc-gui-seek-amount 5
  "Seek this many seconds by default.")

(defvar mpc-gui-server-host "127.0.0.1"
  "MPD server host.")

(defvar mpc-gui-server-port "6600"
  "MPD server port.")

;;;; Faces

(defface mpc-gui-current-track-face
  '((t :inherit font-lock-keyword-face :bold t))
  "Face for the current track in the playlist."
  :group 'mpc-gui)

;;;; Utilities

(defun mpc-gui-get-buffer ()
  "Return the `mpc-gui' buffer."
  (get-buffer-create "*mpc-gui*"))

(defun mpc-gui-run-mpc (command &rest args)
  "Run mpd COMMAND with ARGS.

Return command output as string."
  (with-temp-buffer
    (let* ((host mpc-gui-server-host)
           (port mpc-gui-server-port)
           (stream (open-network-stream "mpc" (current-buffer) host port)))
      ;; Receive the OK.
      (accept-process-output stream)
      (unless (equal "OK MPD " (buffer-substring (point-min) (+ 7 (point-min))))
        (error "Malformed connection response"))
      (erase-buffer)
      ;; Run the command.
      (with-temp-buffer
        (insert command)
        (dolist (arg args)
          (insert " " (format "%s" arg))) ; convert ARG to string
        (insert "\n")
        (process-send-region stream (point-min) (point-max)))
      ;; Receive output.
      (accept-process-output stream)
      (buffer-string))))

(defun mpc-gui-get-key-value (key command &rest args)
  "Run mpd COMMAND with ARGS.  Return the value of KEY from its output."
  (let ((output (apply #'mpc-gui-run-mpc command args)))
    (string-match (concat key ": \\(.+\\)\n") output)
    (match-string 1 output)))

(defun mpc-gui-paused-p ()
  "Return t if playback is paused."
  (equal "pause" (mpc-gui-get-key-value "state" "status")))

(defun mpc-gui-get-track-time ()
  "Return the elapsed time of the current track, in seconds."
  (mpc-gui-get-key-value "elapsed" "status"))

(defun mpc-gui-get-current-track ()
  "Return the current track name from \"mpc current\"."
  (mpc-gui-get-key-value "file" "currentsong"))

(defun mpc-gui--current-line ()
  "Get current line as string."
  (buffer-substring (line-beginning-position) (line-end-position)))

;;;; Playlist

(defun mpc-gui-populate-playlist ()
  "Add all available tracks to the playlist.  Return the new playlist."
  (with-temp-buffer
    (mpc-gui-run-mpc "clear")
    (mpc-gui-run-mpc "update")
    ;; A dirty hack to add all tracks.
    (mpc-gui-run-mpc "searchadd" "\"(modified-since \\\"1\\\")\"")
    (mpc-gui-get-playlist)))

(defun mpc-gui-get-playlist ()
  "Return the current playlist as a list of file names ordered by position."
  (with-temp-buffer
    (insert (mpc-gui-run-mpc "playlist"))
    (let ((playlist '())
          (running t))
      (goto-char (point-min))
      ;; Quit on empty playlist.
      (when (equal "OK\n" (buffer-string))
        (setq running nil))
      (while running
        ;; Delete the leading "0:file: ".
        (goto-char (line-beginning-position))
        (while (not (equal ?\s (char-after)))
          (delete-char 1))
        (delete-char 1) ; delete the space itself
        (push (mpc-gui--current-line) playlist)
        (forward-line 1)
        ;; Stop after reaching the OK confirmation.
        (when (equal "OK" (mpc-gui--current-line))
          (setq running nil)))
      (nreverse playlist)))) ; tracks were added in reverse order

;;;; Display functions

(defmacro mpc-gui--do-display (buffer &rest body)
  "Execute BODY in BUFFER, like `progn'.

Before execution, enable writing to BUFFER, and erase it.

After execution, restore read-only state.

Point value stays the same before and after execution."
  (declare (indent 1))
  (let ((original-point-var (make-symbol "original-point")))
    `(with-current-buffer ,buffer
       (let ((,original-point-var (point))
             (buffer-read-only nil))
         (erase-buffer)
         ;; Solve a weird problem with Arabic filenames.
         (setq bidi-display-reordering nil)
         (prog1 (progn ,@body)
           (goto-char ,original-point-var))))))

(defun mpc-gui-display (&optional _ignore-auto _noconfirm)
  "Display the playlist buffer.

IGNORE-AUTO and NOCONFIRM are passed by `revert-buffer'."
  (mpc-gui--do-display (mpc-gui-get-buffer)
    (let* ((current-track (mpc-gui-get-current-track))
           (playlist (mpc-gui-get-playlist))
           (playlist (or playlist (mpc-gui-populate-playlist))))
      (dolist (track playlist)
        (insert (if (equal track current-track)
                    (propertize track 'face 'mpc-gui-current-track-face)
                  track)
                "\n")))))

;;;; Commands

(defun mpc-gui-play-line ()
  "Play the track with the same ID as the current line number."
  (interactive)
  (mpc-gui-run-mpc "play" (1- (line-number-at-pos)))
  (revert-buffer))

(defun mpc-gui-toggle-playback ()
  "Pause if playing, and play if paused."
  (interactive)
  (mpc-gui-run-mpc "pause"))

(defun mpc-gui-seek-delta (n)
  "Seek N seconds forward/backward."
  (let* ((sign (if (< n 0) "" "+"))
         (amount (format "%s%d" sign n)))
    (mpc-gui-run-mpc "seekcur" amount)))

(defun mpc-gui-seek-forward (&optional arg)
  "Seek forward ARG seconds.  Without ARG, use `mpc-gui-seek-amount'.

Can accept negative ARG to seek backward."
  (interactive "P")
  (mpc-gui-seek-delta (or arg mpc-gui-seek-amount)))

(defun mpc-gui-seek-backward (&optional arg)
  "Seek backward ARG seconds . Without ARG, use `mpc-gui-seek-amount'.

Can accept negative ARG to seek forward."
  (interactive "P")
  (mpc-gui-seek-delta (- (or arg mpc-gui-seek-amount))))

(defun mpc-gui-reload-playlist (&optional interactive)
  "Reload the playlist.  Continue playing the current track.

Return the current playlist for output in `mpc-gui-display'.

If INTERACTIVE is non-nil, revert the buffer."
  (interactive (list t))
  (pcase-let* ((was-paused (mpc-gui-paused-p))
               (current-track (mpc-gui-get-current-track))
               (elapsed (mpc-gui-get-track-time))
               (playlist (mpc-gui-populate-playlist))
               (playlist* playlist)
               (track-id 0)
               (track nil)
               (found nil))
    ;; Find the current track's ID in the new playlist.
    (while (and current-track playlist* (not found))
      (setq track (car playlist*))
      (if (equal track current-track)
          (progn
            ;; Track found.  Play it, and seek to where we were.
            (mpc-gui-run-mpc "play" track-id)
            (mpc-gui-run-mpc "seekcur" elapsed)
            ;; Keep the track paused if it was so.
            (when was-paused
              (mpc-gui-run-mpc "pause" 1))
            (setq found t))
        (setq playlist* (cdr playlist*))
        (cl-incf track-id)))
    (when interactive
      (revert-buffer))
    playlist))

;;;; Main

(defvar mpc-gui-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") #'mpc-gui-reload-playlist)
    (define-key map (kbd "t") #'mpc-gui-toggle-playback)
    (define-key map (kbd "f") #'mpc-gui-seek-forward)
    (define-key map (kbd "b") #'mpc-gui-seek-backward)
    (define-key map (kbd "RET") #'mpc-gui-play-line)
    map))

(define-derived-mode mpc-gui-mode special-mode "mpc-gui"
  "The mode used in the main buffer."
  (setq revert-buffer-function #'mpc-gui-display))

;;;###autoload
(defun mpc-gui ()
  "Open the playlist buffer.  Create if it doesn't exist, and populate."
  (interactive)
  (with-current-buffer (mpc-gui-get-buffer)
    (mpc-gui-display)
    (mpc-gui-mode)
    (hl-line-mode)
    (switch-to-buffer (current-buffer))))

(provide 'mpc-gui)

;;; mpc-gui.el ends here
