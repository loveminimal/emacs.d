;;; init-view.el --- Theme. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'init-vars)

;; (set-background-color "honeydew")
;; (when (not *is-nux*)
;;   (load-theme 'tsdh-light t)
;;   (set-cursor-color custom-blue)
;;   )

;; Main for using in Terminal
;; (when *is-nux*
;;   (set-foreground-color "#ccc")
;;   (set-face-background 'region "white")
;;   )

(use-package github-modern-theme
  :if (display-graphic-p)
  :ensure t
  :config
  (load-theme 'github-modern t)
  (set-cursor-color custom-blue))

(use-package monokai-theme
  :ensure t
  :disabled
  :config
  (load-theme 'monokai t))

(use-package solarized-theme
  :ensure t
  :disabled
  :config
  (load-theme 'solarized-dark t)
  ;; (load-theme 'solarized-light t)
  )

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
  (setq dired-sidebar-use-custom-font t))

(use-package ibuffer-sidebar
  :ensure t
  :commands (ibuffer-sidebar-toggle-sidebar))

(defun sidebar-toggle ()
  "Toggle both `dired-sidebar' and `ibuffer-sidebar'."
  (interactive)
  (dired-sidebar-toggle-sidebar)
  (ibuffer-sidebar-toggle-sidebar))

(provide 'init-view)
;;; init-view.el ends here
