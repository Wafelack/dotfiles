(in-package :stumpwm)

;; Keybindings
(set-prefix-key (kbd "C-RET"))

;; XTerm sucks.
(undefine-key *root-map* (kbd "c"))
(define-key *root-map* (kbd "c") "exec st")

(define-key *root-map* (kbd "P") "exec flameshot gui")

(defparameter *pactl-sink* 0)
(defparameter *pactl-command-template* (format NIL "pactl -- set-sink-volume ~a" *pactl-sink*))



(define-key *root-map* (kbd "C-l") "exec slock")

;; Commands
(defun set-volume-to (volume)
  (run-or-raise (format NIL "~a ~a%" *pactl-command-template* volume)) '(:class "PACTL"))

(defun add-to-volume (volume)
  (set-volume-to (if (> volume -1) (format NIL "+~a" volume) volume)))

(define-key *root-map* (kbd "C-i") "update-volume yes 10") ;; Increase volume
(define-key *root-map* (kbd "C-d") "update-volume yes -10") ;; Decrease volume
(define-key *root-map* (kbd "M") "update-volume no 0")) ;; Mute
(define-key *root-map* (kbd "u") "update-volume")

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
