;;; init-more.el --- Configurations with installed packages.
;;; Commentary:
;;; Code:


(require-package 'flycheck)
(add-hook 'after-init-hook 'global-flycheck-mode)


(require-package 'yasnippet)
(add-hook 'after-init-hook 'yas-global-mode)


(require-package 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)


(require-package 'diff-hl)
(global-diff-hl-mode)


(require-package 'company)
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


(require-package 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


(require-package 'ido-vertical-mode)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
(setq ido-vertical-show-count t)


(require-package 'switch-window)
(setq switch-window-shortcut-style 'qwerty)

(require-package 'which-key)
(add-hook 'after-init-hook 'which-key-mode)
(which-key-setup-side-window-right-bottom)


(require-package 'diminish)
(diminish 'eldoc-mode)
(diminish 'undo-tree-mode)
(diminish 'which-key-mode)
(diminish 'abbrev-mode "AV")


;; @Mickey Petersen
(require-package 'iedit)
(require 'iedit)
(defun iedit-dwim (arg)
  "Starts iedit but uses \\[narrow-to-defun] to limit its scope."
  (interactive "P")
  (if arg
      (iedit-mode)
    (save-excursion
      (save-restriction
        (widen)
        ;; this function determines the scope of `iedit-start'.
        (if iedit-mode
            (iedit-done)
          ;; `current-word' can of course be replaced by other
          ;; functions.
          (narrow-to-defun)
          (iedit-start (current-word) (point-min) (point-max)))))))
(global-set-key (kbd "C-;") 'iedit-dwim)



;; Need no extra configurations

(require-package 'magit)
(require-package 'youdao-dictionary)
(require-package 'htmlize)

(provide 'init-more)
;;; init-more.el ends here
