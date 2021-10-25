(in-package :stumpwm)

;; Well, I do think it is easier to write.
(defmacro add-key (binding command)
  `(define-key *root-map* (kbd ,binding) ,command))

;; Keybindings
(set-prefix-key (kbd "C-RET"))

;; XTerm sucks.
(add-key "c" "exec st")

(add-key "P" "exec flameshot gui")
(add-key "C-l" "exec slock")

;; Commands
(defparameter *pactl-sink* 0)
(defparameter *pactl-command-template* (format NIL "pactl -- set-sink-volume ~a" *pactl-sink*))

(defun set-volume-to (volume)
  (run-or-raise (format NIL "~a ~a%" *pactl-command-template* volume)) '(:class "PACTL"))

(defun add-to-volume (volume)
  (set-volume-to (if (> volume -1) (format NIL "+~a" volume) volume)))

(add-key "C-i" "update-volume yes 10")
(add-key "C-d" "update-volume yes -10")
(add-key "M" "update-volume no 0")
(add-key "u" "update-volume")

(defcommand update-volume (add vol)
  (
   (:y-or-n "Add to existing volume?: ")
   (:number "Percentage to add to the 0th sink: "))
  (if add
      (set-volume-to vol)
      (add-to-volume vol)))

;; Let's vimify Firefox.
(define-remapped-keys
  '(("Firefox"
     ("C-h" . "Left")
     ("C-j" . "Down")
     ("C-k" . "Up")
     ("C-l" . "Right")
     ("C-n" . "Next")
     ("C-N" . "Prior")
     ("C-y" . "C-c")
     ("C-d" . "C-x")
     ("C-p" . "C-v"))))
