(defconst config-el (expand-file-name "config.el" user-emacs-directory))


;;some general configs

(setq-default
 ring-bell-function 'ignore            ; prevent beep sound.
 inhibit-startup-screen t              ; TODO: maybe better on early-init or performance?
 initial-major-mode 'fundamental-mode  ; TODO: maybe better on early-init or performance?
 initial-scratch-message nil           ; TODO: maybe better on early-init?
 create-lockfiles nil                  ; .#locked-file-name
 confirm-kill-processes nil            ; exit emacs without asking to kill processes
 backup-by-copying t                   ; prevent linked files
 require-final-newline t               ; always end files with newline
 delete-old-versions t                 ; don't ask to delete old backup files
 revert-without-query '(".*"))         ; `revert-buffer' without confirmation

(defalias 'yes-or-no-p 'y-or-n-p)

(global-auto-revert-mode)

(save-place-mode)



(defun fk/add-local-hook (hook function)
  "Add buffer-local hook."
  (add-hook hook function nil t))

;;Better Defaults
(global-hl-line-mode)
(blink-cursor-mode -1)

(with-eval-after-load 'org       
  (setq org-startup-indented t) ; Enable `org-indent-mode' by default
  (add-hook 'org-mode-hook #'visual-line-mode))

(setq-default
 truncate-lines t
 frame-resize-pixelwise t  ; maximized emacs may not fit screen without this
 frame-title-format '((:eval
                       (let ((project-name (projectile-project-name)))
                         (unless (string= "-" project-name)
                           (format "%s| " project-name))))
                      "%b"))  ; project-name| file-name

;; start in flull screen mode
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("7ea491e912d419e6d4be9a339876293fff5c8d13f6e84e9f75388063b5f794d6" "bde7af9e749d26cbcbc3e3ac2ac9b13d52aa69b6148a8d6e5117f112f2797b42" default))
 '(initial-frame-alist '((fullscreen . maximized)))
 '(package-selected-packages
   '(yasnippet-snippets yasnippet flymake-aspell flymake-proselint flymake-quickdef moody modus-themes modus-operandi-theme pacmacs clojure-mode ## elpher markdown-mode pdf-tools-org org-pdftools pdf-tools multiple-cursors projectile olivetti esup ace-window avy which-key magit evil use-package)))

;; remove redundant ui
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;font

(set-frame-font "-SRC-Hack-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")

;; configure use package
(require 'package)
  (unless (assoc-default "melpa" package-archives)
      (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))
    (unless (assoc-default "org" package-archives)
      (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t))

    (package-initialize)
    (unless (package-installed-p 'use-package)
      (package-refresh-contents)
      (package-install 'use-package))
    (require 'use-package)
  (setq use-package-always-ensure t)


(use-package evil
  :demand t
  :config
  (evil-mode 1))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; configure packages
(load-file config-el)
