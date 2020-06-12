;;; find-music.el --- an Emacs client for music_finder. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defvar music-dir "/home/music/music/")

(defun find-music-play (query results)
  (sh
   (format
    "echo \"mkdir -p %s; cd %s; %s; %s\" | ssh music@185.222.117.80 bash && %s || %s"
    music-dir
    music-dir
    (string-join
     (cl-loop
      for result in (if (listp results) results (list results))
      for i from 0
      collect (format "ffmpeg -i \\\"%s\\\" \\\"%s-%i.mp3\\\"" result query i))
     "; ")
    "mpc update"
    "notify-send 'Download done'"
    "notify-send 'Something went wrong!'")))

(defun find-music (query &optional arg)
  (interactive "sQuery: \nP")
  (with-temp-buffer
    (url-retrieve
     (url-encode-url (format "http://185.222.117.80:8080/%s/%i/" query (or arg 10)))
     (lambda (_status)
       (forward-paragraph)
       (find-music-play
        query
        (helm :prompt "Select track: "
              :buffer "*Track selection*"
              :sources
              (helm-build-sync-source "find-music-source"
                :multimatch t
                :candidates (mapcar
                             (lambda (x)
                               (cons (cdr (assoc 'title x))
                                     (cdr (assoc 'url x))))
                             (cdr (assoc 'results (json-read)))))))))))

;;; find-music.el ends here
