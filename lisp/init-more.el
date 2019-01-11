;;; init-more.el --- Configurations with installed packages.
;;; Commentary:
;;; Code:

(add-hook 'after-init-hook 'global-flycheck-mode)
(add-hook 'after-init-hook 'yas-global-mode)
(global-set-key (kbd "C-=") 'er/expand-region)


(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "M-n") nil)
     (define-key company-active-map (kbd "M-p") nil)
     (define-key company-active-map (kbd "C-n") #'company-select-next)
     (define-key company-active-map (kbd "C-p") #'company-select-previous)
     ))


(setq hippie-expand-try-functions-list '(try-complete-file-name-partially
					 try-complete-file-name
					 try-expand-dabbrev
					 try-expand-dabbrev-all-buffers
					 try-expand-dabbrev-from-kill))


(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
(setq ido-vertical-show-count t)


(setq switch-window-shortcut-style 'qwerty)
(add-hook 'after-init-hook 'which-key-mode)
(which-key-setup-side-window-right-bottom)



(provide 'init-more)
;;; init-more.el ends here
