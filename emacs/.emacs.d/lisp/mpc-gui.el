;;; mpc-gui.el --- my epic GUI for `mpc'.

;;; Commentary:

;; An mpc GUI modelled after `simple-mpc'.

;;; Code:

(require 'subr-x)

;;;; Variables

(defvar mpc-gui-seek-amount 5
  "Seek this many seconds by default.")

;;;; Faces

(defface mpc-gui-current-track-face
  '((t :inherit font-lock-keyword-face :bold t))
  "Face for the current track in the playlist."
  :group 'mpc-gui)

;;;; Utilities

(defun mpc-gui-get-buffer ()
  "Return the `mpc-gui' buffer."
  (get-buffer-create "*mpc-gui*"))

(defun mpc-gui-call-mpc (destination &rest args)
  "Call mpc with `call-process'.

INFILE, DESTINATION, and ARGS are passed to `call-process'.  Each element of
ARGS is converted to string beforehand."
  (apply #'call-process "mpc" nil destination nil
         (mapcar (lambda (x) (format "%s" x)) args)))

(defun mpc-gui-run-mpc (&rest args)
  "Return mpc output as string.

ARGS are passed to `mpc-gui-call-mpc'."
  (with-temp-buffer
    (apply #'mpc-gui-call-mpc t args)
    (buffer-string)))

(defun mpc-gui--nothing-playing-p ()
  "Return t if no track is currently playing."
  (string-empty-p (mpc-gui-run-mpc "current")))

(defun mpc-gui--paused-p ()
  "Return t if playback is paused."
  (and (string-match "^\\[paused\\]  #" (mpc-gui-run-mpc "status")) t))

(defun mpc-gui--get-track-progress ()
  "Return the track progress grepped from \"mpc status\"."
  (let ((output (mpc-gui-run-mpc "status"))
        ;; TODO: verify this works for >1-hour and <1-minute tracks.
        (regex "\\([0-9]+:[0-9]+/[0-9]+:[0-9]+ ([0-9]+%)\\)"))
    (string-match regex output)
    (match-string 1 output)))

(defun mpc-gui-get-track-progress ()
  "Return a vector of [TIME LENGTH PERCENTAGE] parsed from \"mpc status\".

TIME is the current time of the track. LENGTH is the track's total length. Both
are returned as strings.

PERCENTAGE is TIME divided by LENGTH, as a floating-point number.

Best used with `pcase-let' for destructuring."
  (let ((output (mpc-gui--get-track-progress)))
    (cl-labels ((match (regex)
                       (string-match regex output)
                       (match-string 1 output)))
      (vector (match "\\(.+\\)/")
              (match "/\\(.+\\) ")
              (let* ((raw-string (match " (\\(.+\\)%)"))
                     (number (float (string-to-number raw-string))))
                (/ number 100.))))))

(defun mpc-gui-get-current-track ()
  "Return the current track name from \"mpc current\"."
  (with-temp-buffer
    (mpc-gui-call-mpc t "current")
    (mpc-gui--strip-newline)
    (buffer-string)))

(defun mpc-gui--current-line ()
  "Get current line as string."
  (buffer-substring (line-beginning-position) (line-end-position)))

(defun mpc-gui--strip-newline ()
  "Strip the last newline in the buffer if there is any."
  (unless (= (buffer-size) 0)
    ;; Prevent side-effects.
    (let ((previous-point (point)))
      (goto-char (point-max))
      (delete-char -1)
      (goto-char previous-point))))

;;;; Playlist

(defun mpc-gui--populate-playlist ()
  "Add all available tracks to the playlist.  Return the new playlist."
  (with-temp-buffer
    (mpc-gui-call-mpc t "listall")
    ;; Bail out if no tracks are available.
    (if (= (buffer-size) 0)
        '()
      (mpc-gui-run-mpc "clear")
      (mpc-gui-run-mpc "update")
      (let ((process (start-process "mpc add" nil "mpc" "add")))
        (process-send-region process (point-min) (point-max))
        (process-send-eof process)
        (mpc-gui-get-playlist)))))

(defun mpc-gui-get-playlist ()
  "Return the current playlist as a list of file names ordered by position."
  (with-temp-buffer
    (mpc-gui-call-mpc t "playlist")
    (let ((playlist '()))
      (goto-char (point-min))
      (while (not (eobp))
        (push (mpc-gui--current-line) playlist)
        (forward-line 1))
      (or (nreverse playlist) ; tracks were added in reverse order
          (mpc-gui--populate-playlist)))))

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
    (let ((current-track (mpc-gui-get-current-track)))
      (cl-labels ((fancify (track) (if (equal track current-track)
                                       (propertize track 'face 'mpc-gui-current-track-face)
                                     track)))
        (dolist (track (mpc-gui-get-playlist))
          (insert (fancify track) "\n"))
        (mpc-gui--strip-newline)))))

;;;; Commands

(defun mpc-gui-play-line ()
  "Play the track with the same ID as the current line number."
  (interactive)
  (mpc-gui-run-mpc "play" (line-number-at-pos))
  (revert-buffer))

(defun mpc-gui-toggle ()
  "Pause if playing, and play if paused."
  (interactive)
  (mpc-gui-run-mpc "toggle"))

(defun mpc-gui-seek-delta (n)
  "Seek N seconds forward/backward."
  (let* ((sign (if (< n 0) "" "+"))
         (amount (format "%s%d" sign n)))
    (mpc-gui-run-mpc "seek" amount)))

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

(defun mpc-gui-reload-playlist ()
  "Reload the playlist.  Continue playing the current track."
  (interactive)
  (pcase-let* ((was-paused (mpc-gui--paused-p))
               (current-track (mpc-gui-get-current-track))
               (`[,time _ _] (mpc-gui-get-track-progress))
               (playlist (mpc-gui--populate-playlist))
               (track nil)
               (found nil)
               (track-id 1))
    ;; Find the current track's ID in the new playlist.
    (while (and current-track playlist (not found))
      (setq track (car playlist))
      (if (equal track current-track)
          (progn
            ;; Track found.  Play it, and seek to where we were.
            (mpc-gui-run-mpc "play" track-id)
            (mpc-gui-run-mpc "seek" time)
            ;; Keep the track paused if it was so.
            (when was-paused
              (mpc-gui-run-mpc "pause"))
            (setq found t))
        (setq playlist (cdr playlist))
        (cl-incf track-id)))))

;;;; Main

(defvar mpc-gui-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") #'mpc-gui-reload-playlist)
    (define-key map (kbd "t") #'mpc-gui-toggle)
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
