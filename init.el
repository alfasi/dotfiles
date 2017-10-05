(require 'package)

; List the packages you want
(setq package-list '(evil
                     evil-leader))

; Add Melpa as the default Emacs Package repository
; only contains a very limited number of packages
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

; Activate all the packages (in particular autoloads)
(package-initialize)

; Update your local package index
(unless package-archive-contents
  (package-refresh-contents))

; Install all missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'evil)
(evil-mode t)

(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "b" 'switch-to-buffer
  "w" 'save-buffer)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" default)))
 '(package-selected-packages
   (quote
    (nasm-mode evil-magit auto-complete all-the-icons neotree powerline-evil powerline linum-relative dracula-theme evil-leader evil))))

;; Disable GUI stuff
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1) 

;; Spaces only
(setq-default indent-tabs-mode nil)

;; Default tab-size 2 spaces
(setq-default tab-width 2)

;; Python specific tab-width
(defun python-indent-setup ()
  (setq c-basic-offset 4)
  (setq indent-tabs-mode nil))
(add-hook 'python-mode-hook 'python-indent-setup)

;; Auto indent
(add-hook 'lisp-mode-hook '(lambda ()
  (local-set-key (kbd "RET") 'newline-and-indent)))

;; Hide welcome-screen
(setq inhibit-startup-screen t)

;; Set theme
(load-theme 'dracula t)

;; Relative line numbers
(linum-mode)
(linum-relative-global-mode)
(setq linum-relative-current-symbol "")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#282a36" :foreground "#f8f8f2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "PfEd" :family "Monaco")))))

;; Powerline
(powerline-evil-vim-color-theme)

;; Neotree
(setq neo-theme (if (display-graphic-p) 'ascii 'arrow))
(global-set-key [f2] 'neotree-toggle)
(add-hook 'neotree-mode-hook
          (lambda ()
              (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
              (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
              (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))
