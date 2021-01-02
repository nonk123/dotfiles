;;; alarm-clock.el --- homebrewn alarm clock thing.

;;; Commentary:

;; I just want to get up on time.
;;
;; Not to be confused with wlemuel/alarm-clock on GitHub.

;;; Code:

(require 'parse-time)

;;;; Variables

(defvar alarm-clock-playback-program "mpv"
  "Executable used for playing media files.  Passed to `start-process'.")

(defvar alarm-clock-tick-interval 30
  "Check for alarms to fire, every this-many seconds.")

(defvar alarm-clock-default-media-file nil
  "Default media file for alarm clock to play.")

;;;; Shortcuts

(defconst alarm-clock-all-days '(MON TUE WED THU FRI SAT SUN)
  "All days of week.")

(defconst alarm-clock-weekdays '(MON TUE WED THU FRI)
  "All five weekdays, which are usually the work days.")

;;;; Utilities

(defun alarm-clock-symbol-to-dow (symbol)
  "Convert SYMBOL to a day-of-week number between 0 and 6."
  (pcase symbol
    ('SUN 0)
    ('MON 1)
    ('TUE 2)
    ('WED 3)
    ('THU 4)
    ('FRI 5)
    ('SAT 6)
    (_ (user-error "Not a day-of-week symbol: %s" symbol))))

;;;; Main

(defvar alarm-clock--playback-process nil
  "Process playing an alarm's media file.")

(defun alarm-clock-start-player (file)
  "Return a process that plays back FILE.  Feel free to override.

FILE is an absolute path.

Plays a file with MPV by default."
  (start-process "Alarm Clock" nil "mpv" file))

(defun alarm-clock-play (file)
  "Play FILE with `alarm-clock-start-player'.

Playback is not \"layered\"; the currently playing alarm is stopped."
  (alarm-clock-stop)
  (setq alarm-clock--playback-process
        (alarm-clock-start-player (expand-file-name file))))

(defun alarm-clock-stop (&optional interactive)
  "Stop playing the alarm sound.

When INTERACTIVE is non-nil, show a message if no alarm is playing."
  (interactive (list t))
  (if (process-live-p alarm-clock--playback-process)
      (kill-process alarm-clock--playback-process)
    (when interactive
      (message "No alarm is currently playing")))
  (setq alarm-clock--playback-process nil))

(defvar alarm-clock--schedule '()
  "A list of [(HOUR . MINUTE) DOW MEDIA-FILE] for every set up alarm.

HOUR, MINUTE, and DOW are those from `decode-time'.  MEDIA-FILE is a path.

Never set this value directly!  Use `alarm-clock-set-schedule' instead.

Besides, it will start the necessary timers.")

(defun alarm-clock-tick ()
  "Check for any alarms that can be fired off."
  (let* ((time (decode-time))
         (this-hour (elt time 2))
         (this-minute (elt time 1))
         (this-day-of-week (elt time 6)))
    (pcase-dolist (`[(,hour . ,minute) ,day-of-week ,media-file] alarm-clock--schedule)
      (when (and (= hour this-hour)
                 (= minute this-minute)
                 (= day-of-week this-day-of-week))
        (if (file-exists-p media-file)
            (alarm-clock-play media-file)
          (message "Can't play alarm: file doesn't exist: %s" media-file))))))

(defvar alarm-clock--mainloop-timer nil
  "Timer that's run every minute to see if any alarms can be fired.")

(defun alarm-clock-set-schedule (schedule)
  "Set alarm clock schedule.

SCHEDULE is a list of [TIME DAYS MEDIA-FILE].

TIME is the time of day the alarm sets off (e.g., \"7:00\").  It can be anything
recognized by `parse-time-string', but only the hour and minute matter.

DAYS is a list of symbols `MON', `TUE', `WED', `THU', `FRI', `SAT', `SUN'.
Each symbol may appear once or don't appear at all.  It specifies the days
of week the alarm fires on.

DAYS can be a symbol.  In this case, it becomes (symbol-value DAYS).

Certain shortcuts to DAYS are available: `alarm-clock-all-days' and
`alarm-clock-weekdays'.  The latter is its default value.

MEDIA-FILE is played when the alarm starts.  It defaults to
`alarm-clock-default-media-file' when nil.

SCHEDULE can be an empty list to disable the alarm clock."
  (when alarm-clock--mainloop-timer
    (cancel-timer alarm-clock--mainloop-timer))
  (setq alarm-clock--schedule '())
  (dolist (entry schedule)
    (pcase-let*
        ((`[,time ,days ,media-file] entry)
         (days (or days 'alarm-clock-weekdays))
         (time (parse-time-string time))
         (minute (elt time 1))
         (hour (elt time 2))
         (media-file (or media-file alarm-clock-default-media-file)))
      ;; Allow passing shortcuts without backtick.
      (when (symbolp days)
        (setq days (symbol-value days)))
      (dolist (day days)
        (let ((dow (alarm-clock-symbol-to-dow day)))
          (push `[(,hour . ,minute) ,dow ,media-file] alarm-clock--schedule)))))
  ;; Don't start the timer when the schedule is empty.
  (when alarm-clock--schedule
    (setq alarm-clock--mainloop-timer
          (run-at-time nil alarm-clock-tick-interval #'alarm-clock-tick))))

(provide 'alarm-clock)

;;; alarm-clock.el ends here
