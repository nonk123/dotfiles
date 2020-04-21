(defvar music-dir "/home/music/music/")

(defun find-music-play (query results)
  (sh
   (format
    "echo \"mkdir -p %s; cd %s; %s; %s\" | ssh music@185.222.117.80 bash"
    music-dir
    music-dir
    (string-join
     (cl-loop
      for result in (if (listp results) results (list results))
      for i from 0
      collect (format "ffmpeg -i '%s' '%s-%i.mp3'" result query i))
     "; ")
    "mpc update; mpc clear; mpc listall | mpc add; mpc play")))

;;;###autoload
(defun find-music (query)
  (interactive "sQuery: ")
  (with-temp-buffer
    (url-insert-file-contents
     (url-encode-url (concat "http://185.222.117.80:8080/" query "/10/")))
    (find-music-play
     query
     (helm :prompt "Select track: "
           :buffer "*Track selection*"
           :sources (helm-build-sync-source "find-music-source"
                      :multimatch t
                      :candidates (mapcar
                                   (lambda (x)
                                     (cons (cdr (assoc 'title x))
                                           (cdr (assoc 'url x))))
                                   (cdr (assoc 'results (json-read)))))))))
