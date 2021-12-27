;; Keybindings
(set-prefix-key (kbd "C-RET"))

(add-key "P" "exec flameshot gui")
(add-key "C-l" "exec slock")
(add-key "C-f" "fullscreen") 


;; XTerm sucks.
(add-key "t" "exec st")
;; The default binding for exec also sucks.
(add-key "c" "exec")

;; Why not?
(add-key "C-r" "loadrc")
