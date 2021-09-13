
;; settings


;;orgmode

(setq org-agenda-files (append
      (directory-files-recursively "~/_worknotes/" "\\.org$")
      (directory-files-recursively "~/_personal_workspace/personal_notes/" "\\.org$")))

;;diagrams

(require 'subr-x)
(setq homebrew-plantuml-jar-path
      (expand-file-name
       (string-trim
        (shell-command-to-string "brew list plantuml | grep jar"))))


(use-package plantuml-mode
  :mode (("\\.puml$" . plantuml-mode)
         ("\\.plantuml$" . plantuml-mode))
  :config
    (setq plantuml-jar-path homebrew-plantuml-jar-path)
    (setq plantuml-default-exec-mode 'jar)
  )

;; org mode babel 
(use-package ob-plantuml
  :ensure nil
  :after org
  :custom
  (org-plantuml-jar-path homebrew-plantuml-jar-path))

;; Abreviations
(use-package abbrev
    :ensure nil
    :diminish abbrev-mode
    :config
    (if (file-exists-p abbrev-file-name)
        (quietly-read-abbrev-file)))

;; Auto-Completion

(use-package company
     :diminish
     :custom
     (company-begin-commands '(self-insert-command))
     (company-idle-delay .1)
     (company-minimum-prefix-length 2)
     (company-show-numbers t)
     (company-tooltip-align-annotations 't)
     (global-company-mode ))

(use-package flymake
  :hook
  (prog-mode-hook . flymake-mode)
  (text-mode-hook . flymake-mode))

;; Spell checking
(use-package flymake-aspell
    :init
  (setq ispell-dictionary "en_US")
  (setq ispell-program-name (executable-find "/usr/local/bin/aspell"))
  (setq ispell-silently-savep t)
  :bind  (("C-; b" . flyspell-buffer)
          ("C-; w" . flyspell-correct-at-point))
  :hook
  (text-mode-hook . flymake-aspell-setup)
  (prog-mode-hook . flymake-aspell-setup))




;; (defvar ispell-dictionary "english")

;; (use-package flyspell ; built-in
;;   :bind  (("C-; b" . flyspell-buffer)
;;           ("C-; w" . flyspell-correct-at-point))
;;   :config
;;   (setq ispell-program-name (executable-find "/usr/local/bin/aspell")
;;         ispell-list-command "--list"
;;         ispell-extr-args '("--dont-tex-check-comments"))
;;   )

;; Proslint for long form
;;(use-package flymake-proselint
;;  :ensure flymake-quickdef
;;  :config
;;  (add-hook 'text-mode-hook (lambda ()
;;                            (flymake-mode +1)
;;                            (flymake-proselint-setup)))
;;)

;; Completion using ivy
;; (use-package flyspell-correct-ivy
;;   :after (ivy)
;;   :bind
;;   (:map flyspell-mode-map
;;         ("C-;" . flyspell-correct-previous-word-generic)))

;; ivy

(use-package counsel
	:bind (("M-x" . counsel-M-x)
	   ("C-x C-f" . counsel-find-file)
	   ("<f1> f" . counsel-describe-function)
	   ("<f1> v" . counsel-describe-variable)
	   ("C-c g" . counsel-git)
	   ("C-c k" . counsel-rg)
	   ("C-c r" . counsel-recentf)
	   ("M-y" . counsel-yank-pop)
	   ("C-x l" . counsel-locate)))

(use-package ivy
	:bind (("C-c C-r" . ivy-resume)
		   ("C-x b" . ivy-switch-buffer)
		   ("C-x B" . ivy-switch-buffer-other-window))
	:custom
	(ivy-display-style 'fancy)
	(ivy-use-virtual-buffers t)
	(ivy-count-format "(%d/%d) "))
    (use-package swiper
	:bind (("C-s" . swiper)))


;; Parentheses
(setq show-paren-delay 0)           ; how long to wait?
(show-paren-mode t)                 ; turn paren-mode on
(setq show-paren-style 'expression) ; alternatives are 'parenthesis' and 'mixed'

;; Source Control
 (use-package magit
   :bind (("C-x g" . magit-status)))


;; Which-Key
 (use-package which-key
  :config
  (which-key-mode))


 ;;avy

 (use-package avy
   :bind ("M-:" . avy-goto-char))

 ;; windows management

 (use-package ace-window
  :ensure    t
  :bind ("C-x o" . ace-window))

 ;;measure how long it takes to load emacs
(use-package esup
  :commands esup)

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

;; multiple cursors
(use-package multiple-cursors
  :bind
  (("C-M-n" . mc/edit-lines)))


;; snippets for things like org mode <src
(use-package yasnippet
:init
(setq yas-snippet-dirs '("~/.emacs.d/snippets")
      yas-prompt-functions '(yas-ido-prompt))
(add-hook 'text-mode-hook #'yas-minor-mode)
(add-hook 'org-mode-hook #'yas-minor-mode)
(add-hook 'eshell-mode-hook #'yas-minor-mode)
:config
(use-package yasnippet-snippets
  :defer t)
;; Use yas-indent-line fixed in yaml-mode. This fixes issues with parameter mirroring breaking indentation
(setq yas-indent-line 'fixed))



;; (use-package yasnippet                  ; Snippets
;;   :ensure t
;;   :config
;;   (yas-wrap-around-region t)

;;   (with-eval-after-load 'yasnippet
;;     (validate-setq yas-snippet-dirs '(yasnippet-snippets-dir)))

;;   (yas-reload-all)
;;   (yas-global-mode))

;; (use-package yasnippet-snippets         ; Collection of snippets
;;   :ensure t)

;; programing and config files tools

;; markdown mode

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Tools


;; pdf tools
(use-package pdf-tools
  :load-path "site-lisp/pdf-tools/lisp"
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query))

(fset 'learncantril-url-generator
   (kmacro-lambda-form [?0 ?2 ?f ?= ?v right ?w ?x ?0 up ?f ?? ?b ?p escape ?u left ?p ?v ?w ?w left ?x escape ?b left ?x escape ?0 ?v ?$ ?y] 0 "%d"))


;;gemini
(use-package elpher)

;; UI 

;;modus-operandi theme
(use-package modus-themes
  :ensure
  :init (modus-themes-load-themes)
  :config
  (load-theme 'modus-operandi)
  (setq modus-themes-bold-constructs t
	modus-themes-mode-line '3d)
  )

;;Tabs and ribbons for the mode line
(use-package moody
    :config
    (setq moody-slant-function 'moody-slant-apple-rgb)
    (setq x-underline-at-descent-line t)
    (setq moody-mode-line-height 25)
    (moody-replace-mode-line-buffer-identification)
    (moody-replace-vc-mode))

;; configuration and programing languages

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (typescript-mode . lsp)
         (golang-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

;; optional if you want which-key integration
(use-package which-key
    :config
    (which-key-mode))
