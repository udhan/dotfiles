;;; Code: Emacs init

;; Init

;; Gc value
(setq gc-cons-threshold (* 100 1024 1024)) ;; 100mb
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Load basics config from sanemacs
(load "~/.emacs.d/sanemacs.el" nil t)

;; Font and theme
(when (member "JetBrains Mono" (font-family-list))
  (set-face-attribute 'default nil :font "JetBrains Mono 15"))

;; Load theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'acme t)

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Defaults
(use-package exec-path-from-shell)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Don't bother with auto save and backups.
(setq auto-save-default nil)
(setq make-backup-files nil)

;; Smoother and nicer scrolling
(setq scroll-margin 10
   scroll-step 1
   next-line-add-newlines nil
   scroll-conservatively 10000
   scroll-preserve-screen-position 1)

(setq mouse-wheel-follow-mouse 't)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; Set some vars
(setq
 cursor-in-non-selected-windows t  ; Hide the cursor in inactive windows
 echo-keystrokes 0.1               ; Show keystrokes right away, don't show the message in the scratch buffer
 initial-major-mode 'org-mode      ; Org mode by default
 sentence-end-double-space nil     ; Sentences should end in one space, come on!
 help-window-select t              ; Select help window so it's easy to quit it with 'q'
 )

;; When split is automatic, always split windows vertically
(setq split-height-threshold 0)
(setq split-width-threshold nil)

;; Enable transparent title bar on macOS
;; (when (memq window-system '(mac ns))
;;   (add-to-list 'default-frame-alist '(ns-appearance . light))
;;   (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;;   (setq ns-use-proxy-icon nil))

;; Underline line at descent position, not baseline position
(setq x-underline-at-descent-line t)

;; No ugly button for checkboxes
(setq widget-image-enable nil)

;; No sound
(setq visible-bell t)

;; No tootlips
(tooltip-mode 0)

;; Show full path in the title bar.
(setq-default frame-title-format "%b (%f)")

;; Remove annoying minimize shortcut
(global-set-key (kbd "C-z") nil)

;; Basic editor tweaks
;; -------------------

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Line spacing
(setq-default line-spacing 2)

;; Indentation
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; Enable session save/restore
(desktop-save-mode 1)

;; Enable pair mode
(electric-pair-mode 1)

(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-strip-common-suffix t)
  (setq uniquify-after-kill-buffer-p t))

;; Set a theme
;; (use-package monokai-theme
;;   :init
;;   (load-theme 'monokai t))

;; (use-package nord-theme
;;   :init
;;   (setq nord-uniform-mode-lines t)
;;   (load-theme 'nord t))

;; (use-package modus-operandi-theme
;;   :init
;;   (load-theme 'modus-operandi t))


(use-package perfect-margin
  :init
  (perfect-margin-mode t))

;; Modal editing

;; Shortcuts using evil leader key
;; This should happen before evil
(use-package evil-leader
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "<SPC>" 'ace-window
    "b" 'switch-to-buffer
    "c" 'comment-dwim
    "e"  'eval-region
    "g"  'magit-status
    "p" 'projectile-command-map
    "s" 'eshell
    ;;"t" 'treemacs
    "j" 'dumb-jump-go
    "d" 'kill-buffer
    "ms" 'bookmark-set
    "mj" 'bookmark-jump
    "ml" 'bookmark-bmenu-list
    "wt" 'window-toggle-side-windows
    "wd" 'delete-window
    "wD" 'delete-other-windows
    "w=" 'balance-windows-area
    "ff" 'counsel-fzf
    "fp" 'deadgrep)
  (global-evil-leader-mode)
  )

;; Evil mode
(use-package evil
  :init (evil-mode +1))

;; Usage
;; Normal: ys<textObject> Visual: gS<textObject>
;; Change cs, delete ds
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; Sometimes file tree is helpful
;;(use-package treemacs)

;; Imporve editor help
(use-package which-key
  :init
  (setq which-key-idle-delay 0.5)
  :config
  (which-key-mode))

(use-package popwin
  :config
  (popwin-mode t))

(use-package shell-pop
  :config
  (custom-set-variables
   '(shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
   '(shell-pop-universal-key "s-=")))

;; Things rarely change in section above.
;; // End basics

;; Packages
;; -------------------

(use-package ivy
  :defer 0.1
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config (ivy-mode t))

;; (use-package ivy-posframe
;;   :after ivy
;;   :config
;;   (setq ivy-posframe-parameters
;;         '((left-fringe . 8)
;;           (right-fringe . 8)
;;           ))
;;   (setq ivy-posframe-height-alist
;;         '((swiper . 15)
;;           (swiper-isearch . 15)
;;           (t . 10)))
;;   (setq ivy-posframe-display-functions-alist
;;         '((complete-symbol . ivy-posframe-display-at-point)
;;           (swiper . nil)
;;           (swiper-isearch . nil)
;;           (t . ivy-posframe-display-at-frame-center)))
;;   (ivy-posframe-mode 1))

;; Completion
(use-package counsel
  :after ivy
  :config (counsel-mode))

(use-package counsel-projectile
  :after counsel
  :config (counsel-mode))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ;;("C-r" . swiper)
         ))

(define-key minibuffer-local-map
  (kbd "s-r") 'counsel-minibuffer-history)

(define-key shell-mode-map
  (kbd "s-r") 'counsel-shell-history)

;; Additional packages
(use-package all-the-icons)
(use-package markdown-mode)

;; Jump to def
(use-package dumb-jump
  :config
  (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-force-searcher 'rg)
  :ensure)

;; Fast context aware search
(use-package deadgrep)

(use-package company
  :commands company-complete
  :bind ("M-/" . company-complete)
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0))

;; Window navigation
(use-package ace-window
  :config
  (global-set-key (kbd "M-o") 'ace-window))

;; https://protesilaos.com/dotemacs/
(setq display-buffer-alist
      '(;; top side window
        ("\\*\\(Flycheck\\|Flymake\\|Package-Lint\\|vc-git :\\).*"
         (display-buffer-in-side-window)
         (window-height . 0.16)
         (side . top)
         (slot . 0)
         (window-parameters . ((no-other-window . t))))
        ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|Messages\\)\\*"
         (display-buffer-in-side-window)
         (window-height . 0.16)
         (side . top)
         (slot . 1)
         (window-parameters . ((no-other-window . t))))
        ;; bottom side window
        ("\\*\\(Output\\|Register Preview\\|deadgrep\\).*"
         (display-buffer-in-side-window)
         (window-width . 0.16)       ; See the :hook
         (side . bottom)
         (slot . -1)
         (window-parameters . ((no-other-window . t))))
        (".*\\*Completions.*"
         (display-buffer-in-side-window)
         (window-height . 0.16)
         (side . bottom)
         (slot . 0)
         (window-parameters . ((no-other-window . t))))
        ("^\\(\\*e?shell\\|ansi-term\\).*"
         (display-buffer-in-side-window)
         (window-height . 0.16)
         (side . bottom)
         (slot . 1))
        ;; left side window
        ("\\*Help.*"
         (display-buffer-in-side-window)
         (window-width . 0.20)       ; See the :hook
         (side . left)
         (slot . 0)
         (window-parameters . ((no-other-window . t))))
        ;; right side window
        ("\\*Faces\\*"
         (display-buffer-in-side-window)
         (window-width . 0.25)
         (side . right)
         (slot . 0)
         (window-parameters . ((no-other-window . t)
                               (mode-line-format . (" "
                                                    mode-line-buffer-identification)))))
        ("\\*Custom.*"
         (display-buffer-in-side-window)
         (window-width . 0.25)
         (side . right)
         (slot . 1))
        ;; bottom buffer (NOT side window)
        ("\\*\\vc-\\(incoming\\|outgoing\\).*"
         (display-buffer-at-bottom))))

(setq window-combination-resize t)
(setq even-window-sizes 'height-only)
(setq window-sides-vertical nil)

;; Editor config
(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package magit)
(use-package yasnippet
  :config
  (yas-global-mode 1))
(use-package yasnippet-snippets)

(use-package flymake
  :commands flymake-mode
  :config
  (setq flymake-fringe-indicator-position 'left-fringe)
  (setq flymake-suppress-zero-counters t)
  (setq flymake-start-on-flymake-mode t)
  (setq flymake-no-changes-timeout nil)
  (setq flymake-start-on-save-buffer t)
  (setq flymake-proc-compilation-prevents-syntax-check t)
  (setq flymake-wrap-around nil)
  )

(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-yasnippet))

(setq counsel-fzf-cmd "/usr/local/bin/fd --type f | /usr/local/bin/fzf -f \"%s\"")

;; Org mode

;; bootstrap themes
;;(use-package ox-twbs)
;;(use-package ox-slimhtml)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((calc . t)
   (ditaa . t)
   (emacs-lisp . t)
   (shell . t)))
;; Don't prompt before running code in org
(setq org-confirm-babel-evaluate nil)
(setq org-html-validation-link nil)

(setq org-publish-project-alist
  '(
    ("know-html"
     :base-directory "~/workspace/notes/know/"
     :publishing-directory "/ssh:dev:~/public/"
     :publishing-function org-html-publish-to-html
     :headline-lavels 4
     :auto-preamble t
     )
    ("know-static"
     :base-directory "~/workspace/notes/know/"
     :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
     :publishing-directory "/ssh:dev:~/public/"
     :recursive t
     :publishing-function org-publish-attachment
     )
    ("know" :components ("know-html" "know-static"))
))


;; Language support
(use-package protobuf-mode)

(use-package cmake-mode)

(use-package go-mode)

(use-package emmet-mode)

;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
(setq lsp-keymap-prefix "s-l")
(use-package lsp-mode
    :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
            (js2-mode . lsp)
            ;; if you want which-key integration
            (lsp-mode . lsp-enable-which-key-integration))
    :commands lsp)
;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ivy-posframe ((t (:inherit default :background "LemonChiffon1"))))
 '(ivy-posframe-border ((t (:inherit default :background "light green" :foreground "light green")))))

;; Additonal Notes
;; Useful default emacs shortcuts
;; -----------------------------
;;
;; M-, (Go back)
;; M-; (toggle comments)
;; C-h o (help at point)

;; Useful commands without shortcuts
;; --------------------------------
;; clean-buffer-list
;; Jump to dired buffer of current file.
;; dired-jump
