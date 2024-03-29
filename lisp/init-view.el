;;; init-view.el --- Theme. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'init-vars)

;; (when (display-graphic-p)
;;   (load-theme 'tsdh-light t))

;; Main for using in Terminal
;; (when *is-nux*
;;   (set-foreground-color "#ccc")
;;   (set-face-background 'region "white")
;;   )

(use-package github-modern-theme
  :if (display-graphic-p)
  :ensure t
  :disabled
  :config
  (load-theme 'github-modern t))

(use-package monokai-theme
  :ensure t
  ;; :disabled
  :config
  (load-theme 'monokai t))

(use-package solarized-theme
  :ensure t
  :disabled
  :config
  (load-theme 'solarized-dark t)
  ;; (load-theme 'solarized-light t)
  )

;; tip: put it after the theme settings, or it may be covered
(when (display-graphic-p)
  ;; (set-background-color "#f3f9f1")
  (set-cursor-color custom-blue))

(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-use-term-integration t)
  ;; (setq dired-sidebar-use-custom-font t)
  )

(use-package ibuffer-sidebar
  :ensure t
  :commands (ibuffer-sidebar-toggle-sidebar)
  :config
  ;; automatically use evil for ibuffer and dired
  (setq evil-emacs-state-modes (delq 'ibuffer-mode evil-emacs-state-modes)))

(defun sidebar-toggle ()
  "Toggle both `dired-sidebar' and `ibuffer-sidebar'."
  (interactive)
  (dired-sidebar-toggle-sidebar)
  (ibuffer-sidebar-toggle-sidebar))

(provide 'init-view)
;;; init-view.el ends here
