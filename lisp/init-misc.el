;;; init-misc.el --- Stage configurations be unused or temporary.
;;; Commentary:
;;; Code:
(use-package dumb-jump
  :ensure t
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g b" . dumb-jump-back)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config
  ;; (setq dumb-jump-selector 'helm)
  ;; (setq dumb-jump-selector 'ivy)
  )

(provide 'init-misc)
;;; init-misc.el ends here
