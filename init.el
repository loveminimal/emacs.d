;;; init.el --- The main entry of Emacs. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq debug-on-error t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "nano" user-emacs-directory))
(defvar macro-file (expand-file-name "macro.el" user-emacs-directory) "A file to record macros.")
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(defvar *is-mac* (eq system-type 'darwin))
(defvar *is-win* (eq system-type 'windows-nt))
(defvar *is-nux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)))

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
