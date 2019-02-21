;;; init-view.el --- Theme.
;;; Commentary:
;;; Code:

(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t))


(use-package powerline
  :ensure t
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
