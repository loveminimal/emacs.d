;;; init-base.el --- Basic configurations.
;;; Commentary:
;;; Code:


(setq inhibit-startup-screen t
      make-backup-files nil
      auto-save-default nil
      ring-bell-function 'ignore)


(setq-default
 initial-scratch-message (concat ";; Happy hacking, Gnu emacs :)\n\n")
 line-spacing 0.1
 truncate-lines t
 word-wrap t)


;; Permanently force Emacs to indent with spaces, never with TABs
(setq-default  indent-tabs-mode nil)


;; turn on mouse in console mode
(xterm-mouse-mode t)


(display-time)
(setq display-time-24hr-format t)
(fset 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode t)
(defalias 'list-buffers 'ibuffer)


;; @Xah_Lee
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))


(progn
  "Set coding system."
  (set-language-environment 'utf-8)
  (set-default-coding-systems 'utf-8)
  (prefer-coding-system 'utf-8))


(when (member "Monaco" (font-family-list))
  (set-frame-font "Monaco-12" t t))
(when (member "楷体" (font-family-list))
  (set-fontset-font t 'han "楷体"))


(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))


;; Keybindings without extra configuration
(global-set-key (kbd "C-.") 'set-mark-command)


;;;;;; Some Basic Modes

(use-package dired
  :config
  (require 'dired-x)
  (setq dired-recursive-deletes 'top)
  (setq dired-recursive-copies 'always))


(use-package ido-mode
  :hook after-init)

(use-package recentf-mode
  :hook after-init)

(use-package electric-pair-mode
  :hook after-init)

(use-package winner-mode
  :hook after-init)

(use-package shell-script-mode
  :mode ("\\.ps1\\'"))

(add-hook 'after-init-hook 'global-auto-revert-mode)

;; (use-package electric-indent-mode
;;   :hook after-init)

;; (add-hook 'after-init-hook 'global-hl-line-mode)

(setq desktop-path (list user-emacs-directory)
      desktop-auto-save-timeout 600)
(desktop-save-mode 1)


(use-package show-paren-mode
  :hook after-init
  :init
  ;; @zilongshanren
  (define-advice show-paren-function (:around (fn) fix-show-paren-function)
    "Highlight enclosing parens."
    (cond ((looking-at-p "\\s(") (funcall fn))
          (t (save-excursion
              (ignore-errors (backward-up-list))
              (funcall fn))))))


(when (fboundp 'global-prettify-symbols-mode)
  (add-hook 'after-init-hook 'global-prettify-symbols-mode))


(use-package abbrev
  :config
  (clear-abbrev-table global-abbrev-table)
  (define-abbrev-table 'global-abbrev-table
    '(
      ("isme" "#+HUGO_BASE_DIR: ~/site\n#+HUGO_SECTION: posts\n#+HUGO_WEIGHT: auto\n#+HUGO_AUTO_SET_LASTMOD: t\n#+AUTHOR: Jack Liu\n#+DATE: \n#+TITLE:")
      ("ispp" ":PROPERTIES:\n:EXPORT_FILE_NAME: \n:EXPORT_DATE: \n:END:")
      ("isii" "<img width='50%' src='/images/git.jpg' />")
      ("usp" "use-package")
      ("jss" "JavaScript")
    ))
  (setq-default abbrev-mode t)
  (setq save-abbrevs nil)
  :diminish)


;;;;;; Some Basic Functions


(defun open-init-file ()
  "Quickly open init file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))


;; @purcell
(global-set-key (kbd "RET") 'newline-and-indent)
(defun sanityinc/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))
(global-set-key (kbd "S-<return>") 'sanityinc/newline-at-end-of-line)


;; @purcell
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))


;; @purcell
(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))


;; @purcell
(defun sanityinc/adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

(global-set-key (kbd "M-C-8") (lambda () (interactive) (sanityinc/adjust-opacity nil -2)))
(global-set-key (kbd "M-C-9") (lambda () (interactive) (sanityinc/adjust-opacity nil 2)))
(global-set-key (kbd "M-C-7") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))



(provide 'init-base)
;;; init-base.el ends here
