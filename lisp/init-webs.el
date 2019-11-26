;;; init-webs.el --- Configurations of web develop.
;;; Commentary:
;;; Code:

(use-package web-mode
  :ensure t
  :mode ("\\.*tml\\'" "\\.*xml\\'" "\\.ejs\\'"))


(use-package css-mode
  :mode "\\.wxss\\'")


(use-package js2-mode
  :ensure t
  :mode "\\.js\\'")


(use-package vue-mode
  :ensure t
  :mode ("\\.vue\\'"))


(use-package emmet-mode
  :ensure t
  :hook (web-mode css-mode html-mode)
  :diminish " Em")


;; @zilongshanren
(defun my-web-mode-indent-setup ()
  "Self indent setup."
  (setq web-mode-markup-indent-offset 2) ; web-mode, html tag in html file
  (setq web-mode-css-indent-offset 2)    ; web-mode, css in html file
  (setq web-mode-code-indent-offset 2)   ; web-mode, js code in html file
  )
(add-hook 'web-mode-hook 'my-web-mode-indent-setup)


(use-package php-mode
  :ensure t)


(provide 'init-webs)
;;; init-webs.el ends here
