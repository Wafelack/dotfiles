(in-package :stumpwm)

;; Keybindings
(set-prefix-key (kbd "C-RET"))

(undefine-key *root-map* (kbd "c"))
(define-key *root-map* (kbd "c") "exec st")

(define-key *root-map* (kbd "P") "exec flameshot gui")

(defun set-volume-to (volume)
  (let ((template-command "exec pactl -- set-sink-volume 0 "))
    (concatenate 'strign template-command volume)))

(define-key *root-map* (kbd "C-i") (set-volume-to "+10%")) ;; Increase volume
(define-key *root-map* (kbd "C-d") (set-volume-to "-10%")) ;; Decrease volume
(define-key *root-map* (kbd "M") (set-volume-to "0%")) ;; Mute

(define-key *root-map* (kbd "C-l") "exec slock")

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
