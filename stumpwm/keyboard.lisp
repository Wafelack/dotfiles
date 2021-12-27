(defparameter *layout* 0)
(defparameter *layouts* '(US RU FR))

(run-shell-command "setxkbmap -option caps:escape")
(run-shell-command "xmodmap -e 'keysym Shift_R = Multi_key'")
