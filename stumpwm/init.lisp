(in-package :stumpwm)

(defparameter *my-modules-directory* (directory-namestring (truename (merge-pathnames (user-homedir-pathname) ".stumpwm.d"))))

(export '*my-modules-directory*)

(defun module-load (name)
  (let ((fp (merge-pathnames (string-downcase (format nil "~a.lisp" name)) *my-modules-directory*)))
    (if (probe-file fp)
      (when (load fp)
        (message "Successfully loaded module `~a'." name))
      (err "Module `~a' (``~a'') does not exist." fp name)))
  (sleep 1/20))

(defcommand load-module (name)
    ((:string "Module name: "))
  (module-load name))

;; Well, I do think it is easier to write.
(defmacro add-key (binding command)
  `(define-key *root-map* (kbd ,binding) ,command))


(defparameter bludento/skani-tempo 2)
(defparameter bludento/aparato "Jabra")

(module-load 'keybindings)
(module-load 'audio)
(module-load 'mode)
(module-load 'messages)
(module-load 'screen)
(module-load 'keyboard)
(module-load 'date)

;; Background
(run-shell-command "feh --bg-scale ~wafelack/.dotfiles/wallpaper.png")
