;; Wafelack's init.el
;;
;; Configuration file for GNU Emacs.

;; Packages
;; Setup straight

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

; (straight-use-package 'evil) ;; Modal edition for the win
(straight-use-package 'magit) ;; Git client
(straight-use-package 'slime) ;; Lisp Interaction Mode
(straight-use-package 'company) ;; Completion
(straight-use-package 'ivy) ;; M-x completion
(straight-use-package 'projectile) ;; Project management


;; Minor modes and hooks
(global-display-line-numbers-mode 1)

(ivy-mode 1)

(projectile-mode +1)

;; TODO: Write a macro to do that.
(add-hook 'c-mode-hook
	  (lambda ()
	    (electric-pair-mode 1)
	    (setq tab-width 8)))
(add-hook 'emacs-lisp-mode-hook 'electric-pair-mode)

(add-hook 'after-init-hook 'global-company-mode)

; (require 'evil)
; (evil-mode 1)

;; SLIME

(setq inferior-lisp-program "/etc/profiles/per-user/wafelack/bin/sbcl")

(defun restart-slime ()
  (interactive)
  (cl-flet ((slime-buf ()
	    	       (get-buffer (format "*slime-repl ~A*" (file-name-nondirectory inferior-lisp-program))))
	    (get-current-buf (slime-buf)
			     (when (and slime-buf
					(eq slime-buf
					    (current-buffer))
					(> (count-windows) 1))
			       (other-window 1))
			     (current-buffer)))
    (let* ((inferior-lisp-buf (get-buffer "*inferior-lisp*"))
	   (slime-buf (slime-buf))
	   (current-buf (get-current-buf slime-buf)))
      (if (and inferior-lisp-buf
	       slime-buf)
	  (slime-restart-inferior-lisp)
	(progn
	  (slime)
	  (switch-to-buffer current-buf)))
      (setq slime-buf (slime-buf))
      (delete-other-windows)
      (split-window-horizontally)
      (select-window (frame-first-window))
      (switch-to-buffer current-buf)
      (other-window 1)
      (switch-to-buffer slime-buffer))))


(slime-setup '(slime-fancy))

;; Miscellanous configuration
(setq insert-default-directory nil)
(setq make-backup-files nil) ;; Source control exists for this problem.
(setq create-lockfiles nil)
(setq auto-save-default nil)
(setq inhibit-splash-screen t) ;; It is really annoying, you know.

;; Keybindings
; (define-key evil-motion-state-map " " nil)
; (define-key evil-motion-state-map (kbd "SPC g i") 'magit)
(define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)

(defun insert-tab-chr ()
  (interactive)
  (insert "\t"))

; (define-key evil-insert-state-map (kbd "TAB") 'insert-tab-chr)

;; Auto-generated stuff

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(magit slime-company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
