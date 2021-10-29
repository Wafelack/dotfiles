;; Wafelack's init.el
;;
;; Configuration file for GNU Emacs.

;; Packages
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(defvar *wafelack-packages* '(slime company evil magit))

(dolist (pack *wafelack-packages*)
  (unless (package-installed-p pack)
    (package-install pack)))

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
