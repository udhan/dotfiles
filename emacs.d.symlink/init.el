;; setup package management and use-package
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)
(use-package s)

;; Add to exec path
(setq exec-path (append exec-path '("/Users/mithunshitole/.nix-profile/bin")))
(setq exec-path (append exec-path '("/home/mithun/.nix-profile/bin")))

;; Backups
(setq backup-directory-alist '(("" . "~/.emacs.d/backups")))

;; define some helper functions
(defun configure ()
  "edit the `user-init-file'."
  (interactive)
  (find-file user-init-file))

;; setup theme
(use-package doom-themes
  :config
  (load-theme 'doom-one-light t))

;; os specific font sizes
(if (eq system-type 'darwin)
    (progn
      (set-default-font "inconsolata 16")
      (setq default-frame-alist '((font . "Inconsolata-16"))))
  (progn
    (set-default-font "inconsolata 12")
    (setq default-frame-alist '((font . "Inconsolata-12")))))

;; better defaults
(use-package better-defaults)

;; some additional defaults
(setq inhibit-splash-screen t)
(setq initial-scratch-message "")
(setq global-linum-mode t)

;; yes-no
(fset 'yes-or-no-p 'y-or-n-p)

;; column width
(setq-default fill-column 80)

;; better visible bell
(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "#F2804F")
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg))))
;; better scrolling
(setq scroll-margin 0)
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position t)

;; Highlight 80 and above
(use-package column-enforce-mode
  :config
  (add-hook 'prog-mode-hook 'column-enforce-mode))

;; dired
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;; sidebar
(use-package dired-sidebar
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'ascii)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

;; powerline
(use-package powerline
  :config
  (powerline-default-theme))

(use-package powerline-evil)

;; search
(use-package rg
  :config
  (rg-enable-default-bindings (kbd "M-s")))

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
  (ivy-mode 1)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package projectile)
(use-package counsel-projectile)

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
    "t" 'dired-sidebar-toggle-sidebar
    "j" 'avy-goto-char
    "p" 'counsel-projectile
    "s" 'counsel-rg
    "f" 'counsel-find-file
    "df" 'counsel-describe-function
    "dv" 'counsel-describe-variable
    "l" 'counsel-locate
    "r" 'counsel-linux-app
    "c" 'compile))

(use-package evil-org)
(use-package evil-magit)

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

(use-package window-purpose
  :config
  (purpose-mode 1)
  (add-to-list 'purpose-user-mode-purposes
               '((parinfer-mode . code)
                 (python-mode . code)
                 (go-mode . code)
                 (js2-mode . code)
                 (term-mode . terminal)
                 (shell-mode . terminal)
                 (eshell-mode . terminal)
                 (compilation-mode . messages)))
  ;;(add-to-list 'purpose-user-name-purposes '(<name> . <purpose>))
  ;;(add-to-list 'purpose-user-regexp-purposes '(<pattern> . <purpose>))
  (setq purpose-use-default-configuration t)
  (purpose-compile-user-configuration))

;; Language support -------------------------------

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
;; (use-package dashboard
;;   :config
;;   (dashboard-setup-startup-hook))

;; golang
(use-package go-mode)

;; javascript
(use-package js2-refactor)
(use-package xref-js2)
(use-package js2-mode)
(use-package js-doc)

;; Save state of the editor
(desktop-save-mode 1)
(setq desktop-save t)

;; Auto config -------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (window-purpose purpose-mode column-enforce-mode rg js-doc dired-sidebar company-tern emmet-mode emmet parinfer go-mode dracula-theme counsel-projectile dashboard better-defaults eink-theme company-restclient restclient-company company company-mode leuven-theme material-theme avy which-key cider evil-org evil-leader magit powerline-evil powerline s use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; if there is more than one, they won't work right.
 )
