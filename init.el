(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; setup Evil Mode
(straight-use-package 'evil)
(setq evil-want-C-u-scroll 1)
(setq evil-want-C-i-jump nil)
(require 'evil)
(evil-mode 1)
;; bind ctrl-q to vim normal mode
(evil-define-key 'normal 'global (kbd "C-q") 'evil-normal-state)
(evil-define-key 'insert 'global (kbd "C-q") 'evil-normal-state)

(straight-use-package 'evil-collection)
(evil-collection-init)

;;;;;;;;;;;;
;; setup theme
(straight-use-package 'gruvbox-theme)
(load-theme 'gruvbox-light-medium)

(global-set-key (kbd "M-w") 'other-window)
(global-set-key (kbd "M-W") (lambda() (interactive) (other-window -1)))

;;;;;;;;;;;;
;; FONT
(set-frame-font "Liberation Mono" nil t)

(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(setq visible-bell 1)

;; setup helm
(straight-use-package 'helm)
(straight-use-package 'helm-projectile)
(require 'helm)

;;;;;;;;;;;;;;;;;;;;
;; Projectile
;;;;;;;;;;;;;;;;;;;;
(straight-use-package 'projectile)
(projectile-global-mode)
(setq projectile-indexing-method 'native)
(setq projectile-enable-caching t)
(setq projectile-on-system 'helm)
(setq projectile-switch-project-action 'projectile-dired)
(helm-projectile-on)

(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "M-b") 'helm-projectile)
(global-set-key [f1] 'projectile-compile-project)
(global-set-key [f2] 'projectile-run-project)

;;;;;;;;;;;;
;; ZOOM
(straight-use-package 'zoom-frm)
(require 'zoom-frm)
(global-set-key (kbd "C-=") 'zoom-frm-in)
(global-set-key (kbd "C--") 'zoom-frm-out)

;; dired x
(load "dired-x")

;; magit
(straight-use-package 'magit)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("124562b3366c81b85594feba32564cdb14fc053232511369f40a3e5535dac742" "6f358ac456ee889d132b784afaaa1ce0bd9bdfafc89ef6e3fd5e22179ef59905" "d29324c9e80f48dcd2d5649a06de9065b8db979142b667252fdeba1b1fd094c9" "11d9dcf9150178e21b13ac54aa9a08eb72eb0a90605530dd5f25c89707c6a238" "729ddf899d07810d66fb6bd048b1cbef228efbcee0dca69d3d6cd0efcff428e1" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
