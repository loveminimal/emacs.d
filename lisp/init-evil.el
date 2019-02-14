;;; init-evil.el --- Vim.
;;; Commentary:
;;; Code:

(require-package 'evil)
(add-hook 'after-init-hook 'evil-mode)

(require-package 'evil-leader)
(global-evil-leader-mode)

(require-package 'key-chord)
(key-chord-mode 1)


(setcdr evil-insert-state-map nil)
(define-key evil-motion-state-map "\C-u" 'scroll-down-command)
(define-key evil-insert-state-map [escape] 'evil-normal-state)
(setq-default key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map ",," 'evil-normal-state)


(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "<SPC>" 'smex

  "bb" 'ido-switch-buffer
  "bj" 'ibuffer
  "bk" 'ido-kill-buffer
  
  "ff" 'ido-find-file
  "fi" 'open-init-file
  "fo" 'org-open-at-point
  "fd" 'delete-this-file
  "fr" 'rename-this-file-and-buffer
  "fj" 'dired-jump
  "fs" 'save-buffer
  "fS" 'save-some-buffers
  
  "hf" 'describe-function
  "hk" 'describe-key
  "hv" 'describe-variable

  "ll" 'comment-line

  "mj" 'list-bookmarks
  "ms" 'bookmark-set
  "mm" 'bookmark-jump

  "sa" 'mark-whole-buffer
  
  "tt" 'toggle-truncate-lines
  
  "ww" 'switch-window
  "wo" 'switch-window-then-maximize
  "wr" 'split-window-right
  "wd" 'split-window-below

  "yy" 'youdao-dictionary-search-at-point+

  "qq" 'save-buffers-kill-terminal)



(provide 'init-evil)
;;; init-evil.el ends here
