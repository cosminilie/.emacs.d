
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

;; Spell checking

(defvar ispell-dictionary "english")

(use-package flyspell ; built-in
  :bind  (("C-; b" . flyspell-buffer)
          ("C-; w" . flyspell-correct-at-point))
  :config
  (setq ispell-program-name (executable-find "aspell")
        ispell-list-command "--list"
        ispell-extr-args '("--dont-tex-check-comments"))
  )

;; Completion using ivy
(use-package flyspell-correct-ivy
  :after (ivy)
  :bind
  (:map flyspell-mode-map
        ("C-;" . flyspell-correct-previous-word-generic)))


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

;;Source Control

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

 
