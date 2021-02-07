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

;;;;;;;;;;;;
;; evil mode
(straight-use-package 'evil)
(setq evil-want-keybinding nil) ;; hack that fixes evil-collection bug: https://github.com/emacs-evil/evil-collection/issues/60
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
;; theme
(straight-use-package 'gruvbox-theme)
(load-theme 'gruvbox-light-medium t)

;;;;;;;;;;;;
;; window cycling
(global-set-key (kbd "M-w") 'other-window)
(global-set-key (kbd "M-W") (lambda() (interactive) (other-window -1)))

;;;;;;;;;;;;
;; FONT
(set-frame-font "Liberation Mono" nil t)

;;;;;;;;;;;;
;; default settings
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(setq visible-bell 1)
(global-set-key (kbd "C-s") 'save-buffer)

;;;;;;;;;;;;
;; helm / projectile
(straight-use-package 'helm)
(straight-use-package 'helm-projectile)
(require 'helm)
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

;;;;;;;;;;;;
;; dired x
(load "dired-x")

;;;;;;;;;;;;
;; magit
(straight-use-package 'magit)

;;;;;;;;;;;;
;; Autosave
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;;;;;;;;;;;
;; format
(setq-default tab-width 4)
(setq-default c-basic-offset 4)

;;;;;;;;;;;;
;; LANGUAGE ASSOCIATIONS
(add-to-list 'auto-mode-alist '("\\.cl\\'" . c-mode))
