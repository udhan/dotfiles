;; Setup package install
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(use-package s)

;; better defaults
(unless (fboundp 'helm-mode)
  (ido-mode t)
  (setq ido-enable-flex-matching t))

(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(defvar show-paren-delay)
(setq show-paren-delay 0.2)
(show-paren-mode t)

(setq global-linum-mode t)

(setq ring-bell-function 'ignore)

(setq-default indent-tabs-mode nil)
(setq save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      load-prefer-newer t
      ediff-window-setup-function 'ediff-setup-windows-plain
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))

(setq inhibit-splash-screen t)
(setq initial-scratch-message "")

;; Better scrolling
(setq scroll-margin 0)
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position t)

;; yes-no
(fset 'yes-or-no-p 'y-or-n-p)

;; column wrap
(setq-default fill-column 120)


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
    (avy which-key cider evil-org evil-leader magit powerline-evil powerline s use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
