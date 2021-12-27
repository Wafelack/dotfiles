(in-package :stumpwm)

(defparameter *format-de-la-date* "%a %d %b %y %H:%M:%S")

(defcommand dire-la-date () ()
            (message (trim-all (run-shell-command (format nil "rdate '~a'" *format-de-la-date*) t))))

(add-key "C-a" "dire-la-date")
