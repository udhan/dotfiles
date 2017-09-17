;; setup package management and use-package
(package-initialize)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(use-package s)

;; define some helper functions
(defun configure ()
  "Edit the `user-init-file'."
  (interactive)
  (find-file user-init-file))

;; ---------------------------------

;; setup theme
(use-package leuven-theme
  :config
  (load-theme 'leuven t))

(set-default-font "Inconsolata 14")

;; better defaults
(use-package better-defaults)

;; some additional defaults
(setq inhibit-splash-screen t)
(setq initial-scratch-message "")
(setq global-linum-mode t)

;; yes-no
(fset 'yes-or-no-p 'y-or-n-p)

;; column width
(setq-default fill-column 120)

;; better scrolling
(setq scroll-margin 0)
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position t)

;; dired
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;; powerline
(use-package powerline
  :config
  (powerline-default-theme))

(use-package powerline-evil)

;; magit
(use-package magit)
(use-package magit-popup)

;; jump
(use-package avy)

;; rest client
(use-package restclient)

;; auto complete
(use-package company
  :config
  (global-company-mode))

(use-package company-restclient)

(use-package ivy
  :init
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode 1))

(use-package projectile)
(use-package counsel-projectile
  :bind (("C-S-P" . counsel-projectile-switch-project)
         :map evil-normal-state-map
         ("C-p" . counsel-projectile)))


;; evil
(use-package evil
  :config
  (evil-mode 1))

(use-package evil-leader
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "b" 'switch-to-buffer
    "w" 'save-buffer
    "i" 'configure
    "<SPC>" 'other-window
    "g" 'magit-status
    "ps" 'counsel-rg
    "j" 'avy-goto-char))

(use-package evil-org)
(use-package evil-magit)

;; cider for clojure
(use-package cider)

;; dashboard
(use-package dashboard
  :config
  (dashboard-setup-startup-hook))

;; help
(use-package which-key
  :init
  (setq which-key-idle-delay 0.5)
  :config
  (which-key-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (counsel-projectile dashboard better-defaults eink-theme company-restclient restclient-company company company-mode leuven-theme material-theme avy which-key cider evil-org evil-leader magit powerline-evil powerline s use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
