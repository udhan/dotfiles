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
  "edit the `user-init-file'."
  (interactive)
  (find-file user-init-file))

;; ---------------------------------

;; setup theme
;; (use-package leuven-theme
;;   :config
;;   (load-theme 'leuven t))

(use-package doom-themes
  :config
  (load-theme 'doom-one-light t))

;; (use-package dracula-theme
;;   :config
;;   (load-theme 'dracula t))

(set-default-font "inconsolata 13")

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
(use-package company-tern)

(use-package ivy
  :init
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode 1))

(use-package projectile)
(use-package counsel-projectile
  :bind (("c-s-p" . counsel-projectile-switch-project)
         :map evil-normal-state-map
         ("c-p" . counsel-projectile)))


(use-package neotree)

;; evil
(use-package evil
  :config
  (evil-mode 1))

(use-package evil-leader
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<spc>")
  (evil-leader/set-key
    "b" 'switch-to-buffer
    "w" 'save-buffer
    "i" 'configure
    "<spc>" 'other-window
    "g" 'magit-status
    "ps" 'counsel-rg
    "t" 'neotree-toggle
    "j" 'avy-goto-char
    "c"))

(use-package evil-org)
(use-package evil-magit)

;; cider for clojure
(use-package cider)

(use-package parinfer
  :ensure t
  :init
  (progn
    (setq parinfer-extensions
          '(defaults       ; should be included.
            pretty-parens  ; different paren styles for different modes.
            evil           ; if you use evil.
            paredit        ; introduce some paredit commands.
            smart-tab      ; c-b & c-f jump positions and smart shift with tab & s-tab.
            smart-yank))   ; yank behavior depend on mode.
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode)))

(use-package emmet-mode
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode))

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

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; golang
(use-package go-mode)

;; javascript
(define-key js-mode-map (kbd "m-.") nil)
(use-package js2-refactor)
(use-package xref-js2)
(use-package js2-mode)
  ;; :init
  ;; (add-hook 'js2-mode-hook #'js2-refactor-mode)
  ;; (js2r-add-keybindings-with-prefix "C-c C-r")
  ;; (define-key js2-mode-map (kbd "C-k") #'js2r-kill)
  ;; (add-hook 'js2-mode-hook (lambda ()
  ;;                            (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (company-tern emmet-mode emmet parinfer go-mode dracula-theme counsel-projectile dashboard better-defaults eink-theme company-restclient restclient-company company company-mode leuven-theme material-theme avy which-key cider evil-org evil-leader magit powerline-evil powerline s use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
