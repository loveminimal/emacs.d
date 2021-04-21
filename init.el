;;; init.el --- The main entry of Emacs. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq debug-on-error t)

;; (set-face-attribute 'default nil
;;                     :family "Monaco"
;;                     ;; :height 100
;;                     )

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "nano" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'init-defs)
(require 'init-upkg)

(use-package init-abbr)

(use-package init-base)
(use-package init-evil)
(use-package init-view)
(use-package init-more)

(use-package init-orgs)
(use-package init-site)

(use-package init-webs)
(use-package init-misc)

;; Main for using in Terminal
(when *is-nux*
  (set-foreground-color "#ccc")
  (set-face-background 'region "white")
  )

(add-hook 'after-init-hook
	  (lambda () (require 'server)
	    (unless (server-running-p) (server-start))))

(when (file-exists-p macro-file)
  (load macro-file))

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here
