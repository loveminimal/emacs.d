;;; init-base.el --- Basic configurations. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;;;;; Some better settings.

(setq inhibit-startup-screen t
      make-backup-files nil
      auto-save-default nil
      ring-bell-function 'ignore)

;; (set-background-color "honeydew")
(global-hl-line-mode 1)
(global-display-line-numbers-mode 1)
(column-number-mode 1)
(setq system-time-locale "C")

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
(defvar display-time-24hr-format t)
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

(if *is-win*
    (progn
      (when (member "Roboto Mono" (font-family-list))
        (set-frame-font "Monaco-10" t t)
        (set-fontset-font t 'han "Microsoft YaHei UI Light-12")
        )
      ;; (when (member "Consolas" (font-family-list))
      ;;   (set-frame-font "consolas-12" t t))
      ;; (when (member "Monaco" (font-family-list))
      ;;   (set-frame-font "Monaco-11" t t))
      ;; (when (member "楷体" (font-family-list))
      ;;   (set-fontset-font t 'han "楷体-13"))
      ;; (when (member "Symbola" (font-family-list))
      ;;   (set-fontset-font t 'unicode "Symbola" nil 'prepend))
      ))

(if *is-nux*
    (progn
      (when (member "Hack" (font-family-list))
        (set-frame-font "Hack-11.5" t ))
      (when (member "WenQuanYi Micro Hei Mono" (font-family-list))
        (set-fontset-font t 'han "WenQuanYi Micro Hei Mono-13.5"))
      ;; ====== Sans Mono Font ======
      ;; (set-frame-font "Source Code Pro-11.5" t t)
      ;; (set-fontset-font t 'han "WenQuanYi Micro Hei Mono-13.5")
      ))

;; (set-default-font "DejaVu Sans Mono 11")
;; (set-default-font "WenQuanYi Micro Hei Mono 11")

(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

;; Keybindings without extra configuration
(global-set-key (kbd "C-c C-'") 'set-mark-command)

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
  "Quickly open index file."
  (interactive)
  (find-file "~/site/org/index.org"))
(global-set-key (kbd "<f6>") 'open-home-file)

(defun open-nav-file ()
  "Quickly open nav file."
  (interactive)
  (find-file "~/site/org/nav.org"))
(global-set-key (kbd "<f5>") 'open-nav-file)

(defun open-gtd-file ()
  "Quickly open wiki file."
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
  "delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "no file is currently being edited"))
  (when (yes-or-no-p (format "really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

;; @purcell
(defun rename-this-file-and-buffer (new-name)
  "renames both current buffer and file it's visiting to new-name."
  (interactive "snew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

;; @purcell
(defun sanityinc/adjust-opacity (frame incr)
  "adjust the background opacity of frame by increment incr."
  (unless (display-graphic-p frame)
    (error "cannot adjust opacity of this frame"))
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
  "delete trailing whitespace, and replace repeated blank lines to just 1.
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
  "replace repeated blank lines to just 1.
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

;; @me
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
  "Insert `level' * ahead of current line."
  (interactive "swhich level: ")
  (jk/org-delete-headline)
  (let ((x 0) (len (string-to-number level)))
    (while (< x len)
      (if (= len (+ x 1))
          (insert "* ")
        (insert "*")
        )
    (setq x (+ x 1)))))

(global-set-key (kbd "C-c C-h") 'jk/org-insert-headline)

(provide 'init-base)
;;; init-base.el ends here
