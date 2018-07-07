(require 'package)

; List the packages you want
(setq package-list '(evil evil-leader))

; Add Melpa as the default Emacs Package repository
; only contains a very limited number of packages
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

; Activate all the packages (in particular autoloads)
(package-initialize)

(package-install 'flycheck)

;; Enable elpy
(when (require 'elpy nil t)
  (elpy-enable))
(setq elpy-rpc-python-command "python")

; Update your local package index
; (unless package-archive-contents
;   (package-refresh-contents))

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


(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
(require 'js2-refactor)
(require 'xref-js2)

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")

(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "65d9573b64ec94844f95e6055fe7a82451215f551c45275ca5b78653d505bc42" "d6922c974e8a78378eacb01414183ce32bc8dbf2de78aabcc6ad8172547cb074" "f6a935e77513ba40014aa8467c35961fdb1fc936fa48407ed437083a7ad932de" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "3629b62a41f2e5f84006ff14a2247e679745896b5eaa1d5bcfbc904a3441b0cd" "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" default)))
 '(package-selected-packages
   (quote
    (xref-js2 js2-refactor js2-mode eslint-fix clojure-mode groovy-mode exec-path-from-shell js2-highlight-vars cmake-ide indium rubocop material-theme sublime-themes cyberpunk-theme gruvbox-theme ## solarized-theme monokai-theme elpy nasm-mode evil-magit auto-complete all-the-icons neotree powerline-evil powerline linum-relative dracula-theme evil-leader evil)))
 '(show-paren-mode t))

;; Disable GUI stuff
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; Hide welcome-screen
(setq inhibit-startup-screen t)

;; Set theme
(load-theme 'gruvbox t)

;; Relative line numbers
(linum-mode)
(linum-relative-global-mode)
(setq linum-relative-current-symbol "")

;; (custom-set-faces
 ;; ;; custom-set-faces was added by Custom.
 ;; ;; If you edit it by hand, you could mess it up, so be careful.
 ;; ;; Your init file should contain only one such instance.
 ;; ;; If there is more than one, they won't work right.
 ;; '(default ((t (:inherit nil :stipple nil :background "#282a36" :foreground "#f8f8f2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "PfEd" :family "Source Code Pro")))))

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

;; Match parenthesis
(show-paren-mode 1)

;; Scroll be 1 line
(setq scroll-step            1
      scroll-conservatively  10000)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Spaces only
(setq-default indent-tabs-mode nil)

;; Auto indent
(add-hook 'lisp-mode-hook '(lambda ()
  (local-set-key (kbd "RET") 'newline-and-indent)))

;; Default tab-size 2 spaces
(setq-default tab-width 2)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

;; Python specific tab-width
;; (defun python-indent-setup ()
  ;; (setq c-basic-offset 4)
  ;; (setq indent-tabs-mode nil))
;; (add-hook 'python-mode-hook 'python-indent-setup)

(setq js-indent-level 2)


;; Flycheck
(global-flycheck-mode)
(package-install 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(provide 'init)
;;; init.el ends here
