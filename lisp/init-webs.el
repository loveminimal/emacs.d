;;; init-webs.el --- Configurations of web develop.
;;; Commentary:
;;; Code:

(use-package web-mode
  :ensure t
  :mode ("\\.*tml\\'" "\\.*xml\\'" "\\.ejs\\'"))

(use-package vue-mode
  :ensure t
  :mode ("\\.vue\\'")
  :config
  ;; 0, 1, or 2, representing (respectively) none, low, and high coloring
  (setq mmm-submode-decoration-level 0))

(use-package emmet-mode
  :ensure t
  :hook (web-mode css-mode html-mode)
  :diminish " Em")

;; @zilongshanren
(defun my-web-mode-indent-setup ()
  "Self indent setup."
  (setq web-mode-markup-indent-offset 4) ; web-mode, html tag in html file
  (setq web-mode-css-indent-offset 4)    ; web-mode, css in html file
  (setq web-mode-code-indent-offset 4)   ; web-mode, js code in html file
  )
(add-hook 'web-mode-hook 'my-web-mode-indent-setup)

(provide 'init-webs)
;;; init-webs.el ends here
