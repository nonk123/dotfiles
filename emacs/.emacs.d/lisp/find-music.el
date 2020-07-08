;;; find-music.el --- an Emacs client for music_finder. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defvar music-dir "/home/music/music/")

(defun find-music-download (title url)
  (sh
   (format
    "%s echo -e \"mkdir -p %s; cd %s; %s; %s\" | ssh music@185.222.117.80 bash && %s || %s"
    (format "filename=$(cat << EOF\n%s\nEOF\n)\n" title)
    music-dir
    music-dir
    (format "ffmpeg -i \\\"%s\\\" \\\"$filename.mp3\\\"" url)
    "mpc update"
    "notify-send 'Download done'"
    "notify-send 'Something went wrong!'")
   0))

(defun find-music--select (response)
  (helm :prompt "Select track: "
        :buffer "*Track selection*"
        :sources
        (helm-build-sync-source "find-music-source"
          :multimatch t
          :candidates (mapcar
                       (lambda (x)
                         (list (cdr (assoc 'title x)) x))
                       (cdr (assoc 'results (json-read-from-string response)))))))

(defun find-music (query &optional arg)
  (interactive "sQuery: \nP")
  (fetch
   (lambda (response)
     (when-let ((selection (find-music--select response)))
       (dolist (result (if (listp selection) selection (list selection)))
         (find-music-download (cdr (assoc 'title result))
                              (cdr (assoc 'url result))))))
   "http://music-finder.nonk.users.as205315.net/%s/%i/" query (or arg 10)))

;;; find-music.el ends here
