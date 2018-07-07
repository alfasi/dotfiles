; elisp emacs conf
;-*-Emacs-Lisp-*-

; package.el
(package-initialize)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

; add all files under ./lisp
(let ((files (directory-files-and-attributes "~/.emacs.d/lisp" t)))
  (dolist (file files)
    (let ((filename (car file))
          (dir (nth 1 file)))
      (when (and dir
        (not (string-suffix-p "." filename)))
        (add-to-list 'load-path (car file))))))

; no init.el littering
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

; init utils elpa
(require 'init-utils)
(require 'init-elpa)

; install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

; visuals
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(show-paren-mode 1)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq-default left-fringe-width nil)
(setq-default indicate-empty-lines t)
(setq-default indent-tabs-mode nil)

(setq visible-bell t)
(setq vc-follow-symlinks t)
(setq large-file-warning-threshold nil)
(setq split-width-threshold nil)
(setq custom-safe-themes t)
(column-number-mode t)
(setq tab-width 4)
(setq tramp-default-method "ssh")

; backups
(defvar backup-dir "~/.emacs.d/backups/")
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq make-backup-files nil)

; package confs
; (require 'init-fonts)
; (require 'init-gtags)
(require 'init-evil)

; projectile
(use-package projectile
  :ensure t
  :defer 1
  :config
  (projectile-mode)
  (setq projectile-enable-caching t)
  (setq projectile-mode-line
        '(:eval
          (format " Proj[%s]"
            (projectile-project-name)))))

; helm
(use-package helm
  :ensure t
  :diminish helm-mode
  :commands helm-mode
  :config
  (helm-mode 1)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-autoresize-mode t)
  (setq helm-buffer-max-length 40))

; helm-projectile
(use-package helm-projectile
  :commands (helm-projectile helm-projectile-switch-project)
:ensure t)

; emacs-d
(setq server-socket-dir (expand-file-name "server" user-emacs-directory))
(server-start)

; provide
(provide 'init)
