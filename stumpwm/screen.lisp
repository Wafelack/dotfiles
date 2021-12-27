;; Screen layout
(defun desktop-layout ()
  (run-shell-command "xrandr --output eDP-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output HDMI-1 --mode 1680x1050 --pos 1920x0 --rotate normal"))

(defun laptop-layout ()
  (run-shell-command "xrandr --output eDP-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output HDMI-1 --off"))

(defcommand screen-layout (layout)
  ((:y-or-n "Switch to desktop layout?: "))
  (if layout
    (desktop-layout)
    (laptop-layout)))

(add-key "D" "screen-layout y")
(add-key "L" "screen-layout n")

(defun lit-layout (l)
  (string-downcase (string (nth *layout* *layouts*))))

(defun set-layout (l)
  (run-shell-command (format nil "setxkbmap ~a" (lit-layout l))))

(defcommand toggle-layout () ()
  (setq *layout* (mod (1+ *layout*) (length *layouts*)))
  (set-layout *layout*))

;; Brightness
(defcommand set-brightness (output value)
  ((:string "Output: ")
   (:number "New brightness: "))
  (run-shell-command (format nil "xrandr --output ~a --brightness ~f" output (/ value 10)))) ;; As stump only accepts integer values... I do think a 1/10 control is enough.

(add-key "C-e" "set-brightness eDP-1")
(add-key "C-TAB" "toggle-layout")
(add-key "C-c" "set-brightness")
