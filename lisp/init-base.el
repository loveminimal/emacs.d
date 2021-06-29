;;; init-base.el --- Basic configurations. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'init-defs)

;;;;;; Switch Git Bash
(when *is-win*
  (setq explicit-shell-file-name
        "C:/Program Files/Git/bin/bash.exe")
  (setq shell-file-name explicit-shell-file-name)
  (add-to-list 'exec-path "C:/Program Files/Git/bin")
  )

;;;;;; Some Better Settings

(setq inhibit-startup-screen t
      make-backup-files nil
      auto-save-default nil
      system-time-locale "C"
      ring-bell-function 'ignore)

(global-hl-line-mode 1)
(global-display-line-numbers-mode 1)
(column-number-mode 1)

(setq-default
 initial-scratch-message (concat ";; Happy hacking, Gnu emacs :)\n\n")
 line-spacing 0.1
 truncate-lines t
 indent-tabs-mode nil ;; Disable TABs.
 word-wrap t)

;; Turn on mouse in console mode
(xterm-mouse-mode t)

(display-time)
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

(when *is-win*
  (when (member "Consolas" (font-family-list))
    (set-frame-font "consolas-12" t t))
  (when (member "Monaco" (font-family-list))
    (set-frame-font "Monaco-10.5" t t))
  (when (member "楷体" (font-family-list))
    (set-fontset-font t 'han "楷体-12"))
  )

(when *is-nux*
  (when (member "Hack" (font-family-list))
    (set-frame-font "Hack-11.5" t ))
  (when (member "WenQuanYi Micro Hei Mono" (font-family-list))
    (set-fontset-font t 'han "WenQuanYi Micro Hei Mono-13.5"))
  )

;; Main for using in Terminal
(when *is-nux*
  (set-foreground-color "#ccc")
  (set-face-background 'region "white")
  )

(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

;; Keybindings without extra configuration
(global-set-key (kbd "C-c C-'") 'set-mark-command)
(global-set-key (kbd "<f12>") 'save-buffer)

;;;;;; Some Basic Modes

(require 'dired-x)
(setq dired-recursive-deletes 'top)
(setq dired-recursive-copies 'always)

(add-hook 'after-init-hook 'ido-mode)
(add-hook 'after-init-hook 'recentf-mode)
(add-hook 'after-init-hook 'electric-pair-mode)
(add-hook 'after-init-hook 'winner-mode)
(add-hook 'after-init-hook 'global-auto-revert-mode)
;; (add-hook 'after-init-hook 'electric-indent-mode')

(defvar desktop-path (list user-emacs-directory))
(defvar desktop-auto-save-timeout 600)
(desktop-save-mode 1)

(add-hook 'after-init-hook 'show-paren-mode)
;; @zilongshanren
(define-advice show-paren-function (:around (fn) fix-show-paren-function)
  "Highlight enclosing parens."
  (cond ((looking-at-p "\\s(") (funcall fn))
        (t (save-excursion
            (ignore-errors (backward-up-list))
            (funcall fn)))))

;; (when (fboundp 'global-prettify-symbols-mode)
;;   (add-hook 'after-init-hook 'global-prettify-symbols-mode))

;;;;;; Some Basic Functions

(defun open-home-file ()
  "Quickly open index file of site."
  (interactive)
  (find-file "~/site/org/index.org"))
(global-set-key (kbd "<f5>") 'open-home-file)

(defun open-nav-file ()
  "Quickly open nav file of site."
  (interactive)
  (find-file "~/site/org/nav.org"))
(global-set-key (kbd "<f6>") 'open-nav-file)

(defun open-gtd-file ()
  "Quickly open gtd file of site."
  (interactive)
  (find-file "~/site/org/gtd.org"))

(defun open-init-file ()
  "Quickly open init file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f8>") 'open-init-file)

(defun open-config-file (file-name)
  "Quickly open a config file which name is `FILE-NAME'."
  (interactive "sWhich config: ")
  (find-file (concat "~/.emacs.d/lisp/init-" file-name ".el")))

(defun open-base-file ()
  "Quickly open init file."
  (interactive)
  (open-config-file "base"))

(defun org-open-at-point-and-delete-other-windows ()
    "Open link file and just keep the goal file."
  (interactive)
  (org-open-at-point)
  (delete-other-windows))

(defun jk/org-delete-headline ()
  "Delete the old headline if existed."
  (interactive)
  (setq can-loop t)
  (while can-loop
    (beginning-of-line)
    (setq sp (point))
    (setq is-headline (re-search-forward "[ *]" nil t))
    (if is-headline
        (progn
          (setq ep (point))
          (if (= (- ep sp) 1)
              (delete-char -1)
            (progn
              (beginning-of-line)
              (setq can-loop nil))))
      (progn
        (beginning-of-line)
        (setq can-loop nil))))
  (if (not can-loop)
      (message "END")))

(defun jk/org-insert-headline (level)
  "Insert LEVEL * ahead of current line."
  (interactive "swhich level: ")
  (jk/org-delete-headline)
  (let ((x 0) (len (string-to-number level)))
    (while (< x len)
      (if (= len (+ x 1))
          (insert "* ")
        (insert "*")
        )
    (setq x (+ x 1)))))

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
    (error "No file is currently being edited?"))
  (when (yes-or-no-p (format "really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

;; @purcell
(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "snew name: ")
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

;; @xah_lee
(defun xah-clean-whitespace ()
  "Delete trailing whitespace, and replace repeated blank lines to just 1.
only space and tab is considered whitespace here.
works on whole buffer or text selection, respects `narrow-to-region'.

url `http://ergoemacs.org/emacs/elisp_compact_empty_lines.html'
version 2017-09-22"
  (interactive)
  (let ($begin $end)
    (if (region-active-p)
        (setq $begin (region-beginning) $end (region-end))
      (setq $begin (point-min) $end (point-max)))
    (save-excursion
      (save-restriction
        (narrow-to-region $begin $end)
        (progn
          (goto-char (point-min))
          (while (re-search-forward "[ \t]+\n" nil "move")
            (replace-match "\n")))
        (progn
          (goto-char (point-min))
          (while (re-search-forward "\n\n\n+" nil "move")
            (replace-match "\n\n")))
        (progn
          (goto-char (point-max))
          (while (equal (char-before) 32) ; char 32 is space
            (delete-char -1))))
      (message "white space cleaned"))))

(add-hook 'before-save-hook 'xah-clean-whitespace)

;; @xah_lee
(defun xah-clean-empty-lines ()
  "Replace repeated blank lines to just 1.
works on whole buffer or text selection, respects `narrow-to-region'.

url `http://ergoemacs.org/emacs/elisp_compact_empty_lines.html'
version 2017-09-22"
  (interactive)
  (let ($begin $end)
    (if (region-active-p)
        (setq $begin (region-beginning) $end (region-end))
      (setq $begin (point-min) $end (point-max)))
    (save-excursion
      (save-restriction
        (narrow-to-region $begin $end)
        (progn
          (goto-char (point-min))
          (while (re-search-forward "\n\n\n+" nil "move")
            (replace-match "\n\n")))))))

(provide 'init-base)
;;; init-base.el ends here
