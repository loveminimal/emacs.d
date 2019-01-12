;;; init-webs.el --- Web developer configuration.
;;; Commentary:
;;; Code:

(add-to-list 'auto-mode-alist '("\\.*tml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.*xml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))


(add-to-list 'auto-mode-alist '("\\.wxss\\'" . css-mode))


(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)


(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)


;; @zilongshanren
(defun my-web-mode-indent-setup ()
  "Self indent setup."
  (setq web-mode-markup-indent-offset 2) ; web-mode, html tag in html file
  (setq web-mode-css-indent-offset 2)    ; web-mode, css in html file
  (setq web-mode-code-indent-offset 2)   ; web-mode, js code in html file
  )
(add-hook 'web-mode-hook 'my-web-mode-indent-setup)









(provide 'init-webs)
;;; init-webs.el ends here
