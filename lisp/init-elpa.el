;;; init-elpa.el --- Import Melpa.
;;; Commentary:
;;; Code:

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (setq package-archives '(("gnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
			   ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))))

(require 'cl)

(defvar my/packages '(
		      monokai-theme
		      expand-region
		      company
		      smex
		      ido-vertical-mode
		      switch-window
		      which-key
		      web-mode
		      js2-mode
		      emmet-mode
		      flycheck
		      youdao-dictionary
		      htmlize
		      powerline
		      key-chord
		      evil
		      evil-leader
	              yasnippet
		      diminish
		      iedit
		      yaml-mode
		      org-pomodoro
		      org-projectile
		      ox-gfm
		      pomodoro
		      magit
		      diff-hl
		      php-mode
		      ) "Default packages.")

(setq package-selected-packages my/packages)

(defun my/packages-installed-p ()
  "Is installed?"
  (loop for pkg in my/packages
	when (not (package-installed-p pkg)) do (return nil)
	finally (return t)))

(unless (my/packages-installed-p)
  (message "%s" "Refreshing package datebase...")
  (package-refresh-contents)
  (dolist (pkg my/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))



(provide 'init-elpa)
;;; init-elpa ends here
