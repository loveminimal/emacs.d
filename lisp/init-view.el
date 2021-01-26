;;; init-view.el --- Theme.
;;; Commentary:
;;; Code:

;; (set-background-color "honeydew")

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one-light t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Main for using in Terminal
(when *is-nux*
  (set-foreground-color "#ccc")
  (set-face-background 'region "white")
  )

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

;; Init theme as spacemacs-dark
;; (unless (package-installed-p 'spacemacs-theme)
;;   (package-refresh-contents)
;;   (package-install 'spacemacs-theme))

;; (when (package-installed-p 'spacemacs-theme)
;;   (add-hook 'after-init-hook
;; 	    (lambda () (load-theme 'spacemacs-dark t))))

(use-package powerline
  :ensure t
  ;; :disabled
  :config
  ;; (powerline-default-theme)
  ;; (powerline-center-theme)
  (powerline-center-evil-theme))

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
