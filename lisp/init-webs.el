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













(provide 'init-webs)
;;; init-webs.el ends here
