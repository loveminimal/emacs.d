;;; init.el --- The main entry of Emacs. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq debug-on-error t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'init-vars)
(require 'init-upkg)

(require 'init-abbr)

(require 'init-base)
(require 'init-evil)
(require 'init-view)
(require 'init-more)

(require 'init-orgs)
(require 'init-site)

(require 'init-webs)
(require 'init-misc)

;; (add-hook 'after-init-hook
;; 	  (lambda () (require 'server)
;; 	    (unless (server-running-p) (server-start))))

(when (file-exists-p macro-file)
  (load macro-file))

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here
