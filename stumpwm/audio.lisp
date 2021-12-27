(in-package :stumpwm)

(defun trim-all (s)
  (string-trim '(#\Newline #\Space #\Return) s))

(defun head (s n)
  "Return the first n lines of s."
  (let ((os (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))
        (lc 0))
    (loop for c across s
          until (>= lc n)
          do
            (when (eq c #\Newline)
              (setq lc (1+ lc)))
            (vector-push-extend c os))
    (trim-all os)))

(defun tail (s n)
  "Return the last n lines of s."
  (trim-all (reverse (head (reverse s) n))))

(defun find-sink ()
  (let* ((count (run-shell-command "pacmd list-sinks | grep 'sink(s)' | egrep -o '[0-9]+'" t))
         (sink-indices (run-shell-command "pacmd list-sinks | grep 'index'" t))
         (l (tail (head sink-indices (parse-integer count)) 1)))
    (trim-all (run-shell-command (format nil "echo -n '~a' | egrep -o '[0-9]+'" l) t))))

;; Earbuds
(defun trovi-adreson (reg)
  "Trovi la deksesuman bludentan Adreson de Bludenta Aparato."
  (trim-all 
    (run-shell-command 
      (format nil 
              "bluetoothctl devices | egrep '~a' | head -n1 | egrep -o '[0-9A-F][0-9A-F]:[0-9A-F][0-9A-F]:[0-9A-F][0-9A-F]:[0-9A-F][0-9A-F]:[0-9A-F][0-9A-F]:[0-9A-F][0-9A-F]'" 
              reg) 
    t)))

(defcommand konekti-bludentan-aparaton (reg)
    ((:string "Regulesprimo ke kongrui kun la Nombro de l'Aparato: "))
  (when (string= "" (trovi-adreson ""))
    (progn
      (run-shell-command "bluetoothctl scan on")
      (message "Skani por Aparatoj...")
      (sleep bludento/skani-tempo)
      (message "Finita.")
      (sleep 1/2)))
  (let ((adreso (trovi-adreson reg)))
    (message "Adreso trovita: ~a." adreso)
    (run-shell-command (format nil "bluetoothctl pair ~a" adreso) t)
    (message "Aparato parigita.")
    (run-shell-command (format nil "bluetoothctl connect ~a" adreso) t)
    (message "Aparato konektita.")
    (run-shell-command (format nil "pacmd set-default-sink ~a" (find-sink)))
    (message "Eligatjo aliformita.")))

(defcommand malkonekti-bludentan-aparaton (reg)
    ((:string "Regulesprimo ke kongrui kun la Nombro de l'Aparato: "))
  (when (string= "" (trovi-adreson ""))
    (progn
      (run-shell-command "bluetoothctl scan on")
      (message "Skani por Aparatoj...")
      (sleep bludento/skani-tempo)
      (message "Finita.")
      (sleep 1/2)))
  (let ((adreso (trovi-adreson reg)))
    (message "Adreso trovita: ~a." adreso)
    (run-shell-command (format nil "bluetoothctl disconnect ~a" adreso) t)
    (message "Aparato malkonektita.")
    (run-shell-command (format nil "bluetoothctl remove ~a" adreso) t)
    (message "Aparato visxita.")))

;; Volume

(defparameter *pactl-sink* 0)
(defparameter *pactl-command-template* "pactl -- set-sink-volume")

(defcommand update-volume (value)
  ((:number "Percentage to add: "))
  (run-shell-command (format nil "~a ~a ~a%" *pactl-command-template* *pactl-sink* (if (< value 0) value (format nil "+~D" value)))))

(add-key "C-i" "update-volume 10")
(add-key "C-d" "update-volume -10")
(add-key "M" "update-volume -100") ;; This is fine.
