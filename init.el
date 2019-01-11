;;; init.el --- Init entry.
;;; Commentary:
;;; Code:

(setq debug-on-error t)


(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-time)		;; @purcell
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))


(require 'init-elpa)
(require 'init-evil)
(require 'init-base)
(require 'init-view)
(require 'init-more)
(require 'init-webs)
(require 'init-orgs)
(require 'init-misc)


(when (file-exists-p custom-file)
	(load custom-file))



(provide 'init)
;;; init.el ends here
