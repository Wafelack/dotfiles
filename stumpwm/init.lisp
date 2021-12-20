(in-package :stumpwm)

(defvar *layout* 0)
(defvar *layouts* '(US RU FR))

;; Shell commands
(run-shell-command "feh --bg-scale ~/.dotfiles/wallpaper.png")
(run-shell-command "setxkbmap -option caps:escape")
(run-shell-command "xmodmap -e 'keysym Shift_R = Multi_key'")

;; Well, I do think it is easier to write.
(defmacro add-key (binding command)
  `(define-key *root-map* (kbd ,binding) ,command))

;; Keybindings
(set-prefix-key (kbd "C-RET"))

(add-key "P" "exec flameshot gui")
(add-key "C-l" "exec slock")
(add-key "C-f" "fullscreen") 

;; XTerm sucks.
(add-key "t" "exec st")
;; The default binding for exec also sucks.
(add-key "c" "exec")

;; Mode line
(defun get-battery-bar ()
  (let* ((raw (run-shell-command "acpi | grep 'Battery 0' | awk -F',' '{ print $2 }' | egrep -o '[0-9]+'" T))
	 (inted (parse-integer raw)))
    (values inted (bar inted 25 #\# #\Space))))

(defun get-tags (current-id)
  (mapcar #'(lambda (w)
	      (let ((id (window-number w)))
		`(,id . ,(= current-id id))))
	  (stumpwm::all-windows)))

(defvar *modeline-tags* '(no pa re ci vo mu xa ze bi so))

(defun display-windows (current)
  (let ((tags (get-tags (window-number current))))
    (stable-sort tags #'(lambda (a b) (< (car a) (car b))))
    (format NIL "~{~a~} | ~a"
	    (mapcar #'(lambda (tag) (format NIL (if (cdr tag) "(~a)" " ~a ") (nth (car tag) *modeline-tags*)))
		    tags)
	    (window-title current))))

(defvar *max-chars-per-line* 212) ; Hope I'll find a better way to do that later.

(defun gen-mid-space (left right)
  (make-string (- *max-chars-per-line* (+ (length right) (length left))) :initial-element #\Space))

(defun build-mode-line ()
  (multiple-value-bind (percentage bar) (get-battery-bar)
    (let* ((total-width (stumpwm::head-width (current-head)))
	   (current (current-window))
	   (left (display-windows current))
	   (right (format NIL "~a | ~a ~D% | ~a"
			  (string-trim '(#\Return #\Newline #\Linefeed) (run-shell-command "rdate '%a %d %b - %H:%M'" T))
			  bar
        percentage
        (lit-layout *layout*))))
      (format NIL "~a~a~a" left (gen-mid-space left right) right))))

(setf *mode-line-background-color* "#2e3440")
(setf *mode-line-foreground-color* "#eceff4")
(setf *mode-line-position* :bottom)
(setf *mode-line-timeout* 5)
(enable-mode-line (current-screen) (current-head) T)
(setf *screen-mode-line-format* (list '(:eval (build-mode-line))))
;; Commands
(defparameter *pactl-sink* 0)
(defparameter *pactl-command-template* (format NIL "pactl -- set-sink-volume ~a" *pactl-sink*))

(defcommand update-volume (value)
  ((:number "Percentage to add: "))
  (run-shell-command (format NIL "~a ~a%" *pactl-command-template* (if (< value 0) value (format NIL "+~D" value)))))

(add-key "C-i" "update-volume 10")
(add-key "C-d" "update-volume -10")
(add-key "M" "update-volume -100") ;; This is fine.

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

(defcommand set-brightness (output value)
  ((:string "Output: ")
   (:number "New brightness: "))
  (run-shell-command (format NIL "xrandr --output ~a --brightness ~f" output (/ value 10)))) ;; As stump only accepts integer values... I do think a 1/10 control is enough.

(defun lit-layout (l)
  (string-downcase (string (nth *layout* *layouts*))))

(defun set-layout (l)
  (run-shell-command (format NIL "setxkbmap ~a" (lit-layout l))))

(defcommand toggle-layout () ()
  (setq *layout* (mod (1+ *layout*) (length *layouts*)))
  (set-layout *layout*))

(add-key "C-e" "set-brightness eDP-1")
(add-key "C-TAB" "toggle-layout")
(add-key "C-c" "set-brightness")

;; Echo area and input box
(set-fg-color "#eceff4")
(set-bg-color "#2e3440")
(set-border-color "#22272e")
(set-font "unifont")

;; Messages
(setq *message-window-gravity* (setq *message-window-input-gravity* (setq *input-window-gravity* :top)))
(setq *timeout-wait* 10)
