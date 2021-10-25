(in-package :stumpwm)

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

;; Commands
(defparameter *pactl-sink* 0)
(defparameter *pactl-command-template* (format NIL "pactl -- set-sink-volume ~a" *pactl-sink*))

(defun set-volume (volume)
  (format NIL "exec ~a ~a%" *pactl-command-template* volume))

(defun update-volume (volume)
  (set-volume (if (> volume -1) (format NIL "+~a" volume) volume)))

(add-key "C-i" (update-volume 10))
(add-key "C-d" (update-volume -10))
(add-key "M" (set-volume 0))

;; Echo area and input box
(set-fg-color "#eceff4")
(set-bg-color "#2e3440")
(set-border-color "#22272e")
(set-font "unifont")

(setq *message-window-gravity* (setq *message-window-input-gravity* (setq *input-window-gravity* :top)))
