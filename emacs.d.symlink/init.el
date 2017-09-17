;; Setup package management
(package-initialize)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(use-package s)

;; Define some helper functions
(defun configure ()
  "Edit the `user-init-file'."
  (interactive)
  (find-file user-init-file))

;; setup theme
(use-package leuven-theme)
(load-theme 'leuven t)
(set-default-font "Inconsolata 14")

;; better defaults
(use-package better-defaults)

(setq inhibit-splash-screen t)
(setq initial-scratch-message "")
(setq global-linum-mode t)

;; Better scrolling
(setq scroll-margin 0)
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position t)

;; yes-no
(fset 'yes-or-no-p 'y-or-n-p)

;; column wrap
(setq-default fill-column 120)

;; dired
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;; Powerline
(use-package powerline)
(use-package powerline-evil)

(require 'powerline)
(powerline-default-theme)

;; Magit
(use-package magit)
(use-package magit-popup)

;; Jump
(use-package avy)

;; Rest client
(use-package restclient)

;; Auto complete
(use-package company)
(use-package company-restclient)
(global-company-mode)

(use-package ivy)
(use-package projectile)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

;; Evil
(use-package evil)
(use-package evil-leader)
(use-package evil-org)

(require 'evil-leader)
(global-evil-leader-mode)

(require 'evil)
(evil-mode 1)

(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "b" 'switch-to-buffer
  "w" 'save-buffer
  "i" 'configure
  "ps" 'counsel-rg
  "j" 'avy-goto-char)

;; Cider for clojure
(use-package cider)

;; Help
(use-package which-key)
(require 'which-key)
(setq which-key-idle-delay 0.5)
(which-key-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (better-defaults eink-theme company-restclient restclient-company company company-mode leuven-theme material-theme avy which-key cider evil-org evil-leader magit powerline-evil powerline s use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
