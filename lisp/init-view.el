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



(provide 'init-view)
;;; init-view.el ends here
