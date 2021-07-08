;;; init.el --- The main entry of Emacs. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq debug-on-error t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Adjust garbage collection thresholds during startup, and thereafter

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq macro-file (expand-file-name "macro.el" user-emacs-directory))

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
