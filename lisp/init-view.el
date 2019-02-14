;;; init-view.el --- Theme.
;;; Commentary:
;;; Code:

(require-package 'monokai-theme)
(add-hook 'after-init-hook '(lambda () (load-theme 'monokai t)))


(require-package 'powerline)
;; (powerline-default-theme)
(powerline-center-theme)
;; (powerline-center-evil-theme)







(provide 'init-view)
;;; init-view.el ends here
