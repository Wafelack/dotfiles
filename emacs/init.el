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

(straight-use-package 'evil)
(straight-use-package 'magit)
(straight-use-package 'slime)
(straight-use-package 'company)

;; Minor modes and hooks
(global-display-line-numbers-mode 1)

;; TODO: Write a macro to do that.
(add-hook 'c-mode-hook 'eletric-pair-mode)
(add-hook 'emacs-lisp-mode-hook 'electric-pair-mode)

(add-hook 'after-init-hook 'global-company-mode)

(require 'evil)
(evil-mode 1)

;; SLIME
(setq inferior-lisp-program "/etc/profiles/per-user/wafelack/bin/sbcl")

(slime-setup '(slime-fancy))

;; Miscellanous configuration
(setq insert-default-directory nil)
(setq make-backup-files nil) ;; Source control exists for this problem.
(setq create-lockfiles nil)
(setq inhibit-splash-screen t) ;; It is really annoying, you know.

;; Keybindings
(define-key evil-motion-state-map " " nil)
(define-key evil-motion-state-map (kbd "SPC g i") 'magit)

;; Auto-generated stuff

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(magit evil slime-company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
