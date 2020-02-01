;;; init.el --- The main entry of Emacs.
;;; Commentary:
;;; Code:

(setq debug-on-error t)


(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))


(require 'init-upkg)


(use-package init-abbr)                 ;; Module it

(use-package init-base)
(use-package init-evil)
(use-package init-view)
(use-package init-more)

(use-package init-orgs)
(use-package init-site)

(use-package init-webs)


(add-hook 'after-init-hook
	  (lambda () (require 'server)
	    (unless (server-running-p) (server-start))))


(when (file-exists-p custom-file)
  (load custom-file))


(provide 'init)
;;; init.el ends here
