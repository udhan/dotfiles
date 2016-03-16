(require 'cask "/home/mithun/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

;; Clean interface
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-startup-message t)

(setq debug-on-error f)

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

;; Sidebar
;;(autoload 'sr-speedbar 1)

;; Theme
(require 'eink-theme)

;; (require 'solarized-theme)
;; (setq solarized-use-variable-pitch nil)
;; (setq solarized-scale-org-headlines nil)
;;(load-theme 'solarized-light t)

;; Adjust font based on screen resolution
(if (= (display-pixel-height) 2048)
    (set-default-font "Source Code Pro 20")
    (set-default-font "Source Code Pro 13"))

;; Evil mode
(require 'evil)
(evil-mode t)

;; Quick search with 2 characters
;;(require 'evil-snipe)
;;(evil-snipe-mode 1)
;;(evil-snipe-override-mode 1)

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

;; Org mode
(require 'org)
(setq org-agenda-files
      (quote ("~/org")))
(setq org-default-notes-file "~/todo.org")
(setq org-use-fast-todo-selection t)

;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))


(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))


;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/org/refile.org") "* TODO %?\n%U\n%a\n"))))

(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
(setq org-mobile-use-encryption t)
