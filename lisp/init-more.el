;;; init-more.el --- Configurations with extra packages.
;;; Commentary:
;;; Code:


(use-package smex
  :ensure t
  :config (smex-initialize)
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))


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


;; @Mickey Petersen

(use-package iedit
  :ensure t
  :config
  (defun iedit-dwim (arg)
    "Starts iedit but uses \\[narrow-to-defun] to limit its scope."
    (interactive "P")
    (if arg
	(iedit-mode)
      (save-excursion
	(save-restriction
          (widen)
          ;; this function determines the scope of `iedit-start'.
          (if iedit-mode
              (iedit-done)
            ;; `current-word' can of course be replaced by other
            ;; functions.
            (narrow-to-defun)
            (iedit-start (current-word) (point-min) (point-max)))))))
  (global-set-key (kbd "C-;") 'iedit-dwim))






(provide 'init-more)
;;; init-more.el ends here
