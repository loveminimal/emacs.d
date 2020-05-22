;;; init-more.el --- Configurations with extra packages.
;;; Commentary:
;;; Code:

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package iedit
  :ensure t
  )

(use-package smex
  :ensure t
  :config (smex-initialize)
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))

(use-package swiper
  :ensure t
  :init
  (defun sanityinc/swiper-at-point (sym)
    "@purcell
Use `swiper' to search for the symbol at point."
    (interactive (list (thing-at-point 'symbol)))
    (swiper sym))
  :bind (("M-s" . sanityinc/swiper-at-point)
	 ("C-s" . swiper)))

(use-package ido-vertical-mode
  :ensure t
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  (setq ido-vertical-show-count t))

(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode)
  :config (which-key-setup-side-window-right-bottom))

(use-package switch-window
  :ensure t
  :init (setq switch-window-shortcut-style 'qwerty))

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :bind (:map company-active-map
	      ("M-n" . nil)
	      ("M-p" . nil)
	      ("C-n" . company-select-next)
	      ("C-p" . company-select-previous))
  :diminish " C")

(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
	try-complete-file-name
	try-expand-dabbrev
	try-expand-dabbrev-all-buffers
	try-expand-dabbrev-from-kill))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; @purcell
;; Comments code for make it work in lisp modes, but I no need
(use-package move-dup
  :ensure t
  :bind (([M-up] . md-move-lines-up)
	 ([M-down] . md-move-lines-down)
	 ;; ([M-S-up] . md/move-lines-up)
	 ;; ([M-S-down] . md/move-lines-down)
	 ([M-S-up] . md-duplicate-down)
	 ([M-S-down] . md-duplicate-up)))

(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode)
  :diminish " FC")

(use-package diff-hl
  :ensure t
  :hook (after-init . global-diff-hl-mode))

(use-package magit
  :ensure t)

(use-package youdao-dictionary
  :ensure t)

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package undo-tree
  :diminish)

(use-package eldoc
  :diminish)

(use-package command-log-mode
  :ensure t
  :hook (after-init . command-log-mode)
  :diminish " cl")

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  :diminish " P")

(use-package ripgrep
  :ensure t)

(use-package yasnippet
  :ensure t
  :hook (after-init . yas-global-mode)
  :config
  (setq yas-snippet-dirs
      '("~/.emacs.d/snippets"                 ;; personal snippets
        ;; "/path/to/some/collection/"           ;; foo-mode and bar-mode snippet collection
        ;; "/path/to/yasnippet/yasmate/snippets" ;; the yasmate collection
        )))

;; @tumashu
(use-package pyim
  :ensure t
  :disabled
  :config
  ;; Active BaseDict
  (use-package pyim-basedict
    :ensure t
    :config (pyim-basedict-enable))

  (setq default-input-method "pyim")
  (setq pyim-default-scheme 'quanpin)   ;; Use QuanPin
  (setq pyim-page-length 9)             ;; Set amounts of the candidate words
  )

(use-package rime
  :ensure t
  ;; :disabled
  :custom
  (default-input-method "rime")
  :config
  ;; nil minibuffer message popup posframe
  ;; (setq rime-show-candidate 'popup)
  (setq rime-show-candidate 'posframe)
  ;; Highlight icon
  (setq mode-line-mule-info '((:eval (rime-lighter))))
  ;; Temporary EN
  (setq rime-disable-predicates
        '(
          ;; rime-predicate-evil-mode-p
          ;; rime-predicate-after-alphabet-char-p
          ;; rime-predicate-prog-in-code-p
          ;; rime-predicate-punctuation-after-space-cc-p
          ;; rime-predicate-space-after-cc-p
          ;; ---------------------------------
          ;; rime-predicate-after-ascii-char-p
          ;; rime-predicate-ace-window-p
          ;; rime-predicate-hydra-p
          ;; rime-predicate-current-input-punctuation-p
          ;; rime-predicate-punctuation-after-ascii-p
          ;; rime-predicate-punctuation-line-begin-p
          ;; rime-predicate-space-after-ascii-p
          ;; rime-predicate-current-uppercase-letter-p
          ;; rime-predicate-tex-math-or-command-p
          ))
  )

;; Toggle input method globally
(global-set-key (kbd "C-h C-j") 'toggle-input-method)

(use-package pinyin-search
  :ensure t)

(provide 'init-more)
;;; init-more.el ends here
