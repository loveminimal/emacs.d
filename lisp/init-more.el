;;; init-more.el --- Configurations with extra packages.
;;; Commentary:
;;; Code:


(use-package xah-fly-keys
  :ensure t
  :disabled
  :init
  (xah-fly-keys-set-layout "qwerty")
  (xah-fly-keys 1))

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


(use-package window-numbering
  :ensure t
  :hook (after-init . window-numbering-mode))


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
  :bind (([M-up] . md/move-lines-up)
	 ([M-down] . md/move-lines-down)
	 ;; ([M-S-up] . md/move-lines-up)
	 ;; ([M-S-down] . md/move-lines-down)
	 ([M-S-up] . md/duplicate-down)
	 ([M-S-down] . md/duplicate-up)))


(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode)
  :diminish " FC")


(use-package yasnippet
  :ensure t
  :disabled
  :hook (after-init . yas-global-mode))


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


;; Use `pomodoro-start' to start, and `pomodoro-stop' to stop
(use-package pomodoro
  :ensure t
  :config
  (pomodoro-add-to-mode-line)
  (setq play-pomodoro-break-sound nil
	play-pomodoro-work-sound nil))


(use-package command-log-mode
  :ensure t
  :hook (after-init . command-log-mode)
  :diminish " cl")


(use-package pdf-tools
  :ensure t)


(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  :diminish " P")


(use-package ripgrep
  :ensure t)


(provide 'init-more)
;;; init-more.el ends here
