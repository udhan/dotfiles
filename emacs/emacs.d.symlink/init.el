(require 'cask "/home/mithun/.cask/cask.el")
(cask-initialize)

(require 'pallet)
(pallet-mode t)

;; Clean interface
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-startup-message t)

;;(setq debug-on-error t)

;; Save all tempfiles in $TMPDIR/emacs$UID
(defconst emacs-tmp-dir (format "%s%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))

(setq auto-save-list-file-prefix emacs-tmp-dir)

;; Better defaults
(require 'better-defaults)

(setq-default word-wrap t)

;; Shift-arrow keys for moving between visible buffers
(windmove-default-keybindings)

;; Theme
;; (require 'eink-theme)
(require 'solarized-theme)
(setq solarized-use-variable-pitch nil)
(setq solarized-scale-org-headlines nil)
(load-theme 'solarized-light t)

;; Adjust font based on screen resolution
(if (= (display-pixel-height) 2048)
    (set-default-font "Source Code Pro 20")
  (set-default-font "Source Code Pro 13"))

;; Evil mode
(require 'evil)
(evil-mode t)

;; Change cursor color based on evil mode, cool!
(setq evil-emacs-state-cursor '("red" box))
(setq evil-normal-state-cursor '("red" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-replace-state-cursor '("purple" hollow))
(setq evil-operator-state-cursor '("red" hollow))

(require 'window-numbering)
(window-numbering-mode 1)

(define-key evil-normal-state-map "1" 'select-window-1)
(define-key evil-normal-state-map "2" 'select-window-2)
(define-key evil-normal-state-map "3" 'select-window-3)
(define-key evil-normal-state-map "4" 'select-window-4)

;; Zen coding/emmet
(require 'emmet-mode)

;; Auto-start on any markup modes
(add-hook 'sgml-mode-hook 'emmet-mode)

;; enable Emmet's css abbreviation.
(add-hook 'css-mode-hook  'emmet-mode)

;; ido!
(require 'ido)
(ido-mode t)

;; AutoCompletion
(require 'company)
(global-company-mode)

;; Google search
(google-this-mode 1)
(global-set-key (kbd "C-x g") 'google-this-mode-submap)

;; Powerline
(require 'smart-mode-line)
(sml/setup)

(require 'smartparens-config)
