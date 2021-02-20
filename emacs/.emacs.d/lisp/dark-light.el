;;; dark-light.el --- easily switch between dark and light themes.

;;; Commentary:

;;; Code:

(defvar dl-dark-theme nil
  "Don't set this variable directly! Use `dl-set-dark-theme' instead.")

(defvar dl-light-theme nil
  "Don't set this variable directly! Use `dl-set-light-theme' instead.")

(defun dl-set-dark-theme (theme)
  "Load THEME and make it the dark theme."
  (load-theme theme t t)
  (setq dl-dark-theme theme))

(defun dl-set-light-theme (theme)
  "Load THEME and make it the light theme."
  (load-theme theme t t)
  (setq dl-light-theme theme))

(defun dl-go-dark ()
  "Switch to the dark theme."
  (interactive)
  (disable-theme dl-light-theme)
  (enable-theme dl-dark-theme))

(defun dl-go-light ()
  "Switch to the light theme."
  (interactive)
  (disable-theme dl-dark-theme)
  (enable-theme dl-light-theme))

(defun dl-switch ()
  "Switch between the dark and light themes.

If the dark theme is active, switch to the light theme.
Switch to the dark theme otherwise."
  (interactive)
  (if (memq dl-light-theme custom-enabled-themes)
      (dl-go-dark)
    (dl-go-light)))

(provide 'dark-light)

;;; dark-light.el ends here
