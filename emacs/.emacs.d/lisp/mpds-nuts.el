;;; mpds-nuts.el --- yet another MPD client inside Emacs

;; Author: nonk123

;;; Commentary:

;; An extremely dumb MPD client.

;;; Code:

;;;; Common variables:

(defvar-local mpds-nuts--trajectory nil)

;;;; `mpc' calls:

(defvar mpds-nuts-mpc-binary "mpc"
  "`mpc' binary used to issue MPD commands.")

(defvar mpds-nuts-mpc-host "127.0.0.1")
(defvar mpds-nuts-mpc-port "6600")

(defun mpds-nuts--mpc->buffer (output-buffer &rest args)
  "Call `mpc' with ARGS and store its output in OUTPUT-BUFFER."
  (push "--wait" args)
  (push (format "--port=%s" mpds-nuts-mpc-port) args)
  (push (format "--host=%s" mpds-nuts-mpc-host) args)
  (apply #'call-process mpds-nuts-mpc-binary nil output-buffer nil args))

(defun mpds-nuts--call-mpc (&rest args)
  "Call `mpc' with ARGS, discarding the output."
  (with-temp-buffer
    (apply #'mpds-nuts--mpc->buffer (current-buffer) args)))

(defun mpds-nuts--mpc->string (&rest args)
  "Call `mpc' with ARGS and return its output as a string."
  (with-temp-buffer
    (apply #'mpds-nuts--mpc->buffer (current-buffer) args)
    (buffer-string)))

(defun mpds-nuts--mpc->lines (&rest args)
  "Call `mpc' with ARGS and return its output as a list of lines."
  (string-lines (apply #'mpds-nuts--mpc->string args)))

;;;; Buffer stuff:

(defvar mpds-nuts-buffer-name "*MPDs Nuts*")

(defface mpds-nuts-line-face
  '((:box . t))
  "The face used for each field line in the MPDs Nuts main view.")

(defun mpds-nuts-buffer ()
  "Get or create the MPDs-Nuts buffer."
  (let ((buffer (get-buffer-create mpds-nuts-buffer-name)))
    (unless (buffer-live-p buffer)
      (setq buffer (generate-new-buffer mpds-nuts-buffer-name)))
    buffer))

(defvar mpds-nuts-starting-field "albumartist"
  "Display all values of this field when MPDs-Nuts is started.")

(defun mpds-nuts-display-field (field)
  "Display all values of FIELD from the MPD database."
  (with-current-buffer (mpds-nuts-buffer)
    (let ((inhibit-read-only t)
	  filters)
      (dolist (filter mpds-nuts--trajectory)
	(push (cadr filter) filters)
	(push (car filter) filters))
      (erase-buffer)
      (dolist (line (apply #'mpds-nuts--mpc->lines "list" field filters))
	(insert line)
	(add-text-properties (pos-bol) (pos-eol)
			     (list :mpds-nuts-field field
				   :mpds-nuts-field-value line
				   :face 'mpds-nuts-line-face))
	(newline)))
    (goto-char (point-min))))

;;;; Major mode:

(defvar mpds-nuts-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'mpds-nuts-go-into)
    (define-key map (kbd "TAB") #'mpds-nuts-go-out)
    map))

(define-derived-mode mpds-nuts-mode special-mode "MPDs-Nuts"
  "The major mode used inside the `mpds-nuts' buffer."
  (mpds-nuts-display-field mpds-nuts-starting-field))

;;;; Commands:

(defun mpds-nuts--trajectory-changed ()
  (when-let* ((artist-combo (or (assoc "albumartist" mpds-nuts--trajectory)
				(assoc "artist" mpds-nuts--trajectory)))
	      (artist-field (car artist-combo))
	      (artist (cadr artist-combo))
	      (album (car-safe (cdr-safe (assoc "album" mpds-nuts--trajectory))))
	      (title (car-safe (cdr-safe (assoc "title" mpds-nuts--trajectory))))
	      (results (mpds-nuts--mpc->lines "search"
					      artist-field artist
					      "album" album
					      "title" title))
	      (file (if (null (cdr results)) (car results)
		      (completing-read "Select file to play: " results))))
    ;; There is no deeper view than a track.
    (setq mpds-nuts--trajectory (cdr mpds-nuts--trajectory))
    (mpds-nuts--call-mpc "clear")
    (mpds-nuts--call-mpc "add" file)
    (mpds-nuts--call-mpc "play" "1")))

(defun mpds-nuts ()
  "Open the MPDs-Nuts MPD client."
  (interactive)
  (with-current-buffer (mpds-nuts-buffer)
    (mpds-nuts-mode)
    (display-buffer (current-buffer))))

(defun mpds-nuts-go-into (pos)
  (interactive "d")
  (when-let* ((field-name (get-text-property pos :mpds-nuts-field))
	      (field-value (get-text-property pos :mpds-nuts-field-value)))
    (let ((next-field (pcase field-name
			((or "artist" "albumartist") "album")
			("album" "title"))))
      (push (cons field-name (cons field-value next-field)) mpds-nuts--trajectory)
      (mpds-nuts--trajectory-changed)
      (when next-field
	(mpds-nuts-display-field next-field)))))

(defun mpds-nuts-go-out (pos)
  (interactive "d")
  (if (null mpds-nuts--trajectory)
      (message "Already at top level")
    (let ((previous-tag (caar mpds-nuts--trajectory)))
      (setq mpds-nuts--trajectory (cdr mpds-nuts--trajectory))
      (mpds-nuts--trajectory-changed)
      (mpds-nuts-display-field previous-tag))))

(provide 'mpds-nuts)

;;; mpds-nuts.el ends here
