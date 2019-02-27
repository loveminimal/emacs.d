;;; init-evil.org --- Set Emacs as Vim.
;;; Commentary:
;;; Code:


(use-package evil
  :ensure t
  :hook (after-init . evil-mode)
  :config
  (setcdr evil-insert-state-map nil)
  :bind (:map evil-motion-state-map
         ("C-u" . scroll-down-command)
         :map evil-insert-state-map
         ([escape] . evil-normal-state)))


(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "<SPC>" 'smex

    "aa" 'org-agenda
    "cc" 'org-capture
    "cf" 'org-capture-finalize
    "ck" 'org-capture-kill
    "cr" 'org-capture-refile
    "os" 'org-schedule
    "od" 'org-deadline

  
    "bb" 'ido-switch-buffer
    "bj" 'ibuffer-sidebar-toggle-sidebar
    "bk" 'ido-kill-buffer

    
    "ff" 'ido-find-file
    "fi" 'open-init-file
    "fo" 'org-open-at-point
    "fd" 'delete-this-file
    "fr" 'recentf-open-files
    "fj" 'dired-sidebar-toggle-sidebar
    "fs" 'save-buffer
    "fS" 'save-some-buffers

    "gg" 'magit-status
    
    "hf" 'describe-function
    "hk" 'describe-key
    "hv" 'describe-variable

    "ii" 'org-toggle-inline-images
  
    "ll" 'comment-line
  
    "mj" 'list-bookmarks
    "ms" 'bookmark-set
    "mm" 'bookmark-jump
  
    "sa" 'mark-whole-buffer
    
    "tt" 'toggle-truncate-lines
    "tw" 'toggle-frame-maximized
    "tf" 'toggle-frame-fullscreen
    
    "ww" 'switch-window
    "wo" 'switch-window-then-maximize
    "wr" 'split-window-right
    "wd" 'split-window-below
    "wk" 'delete-window
  
    "yy" 'youdao-dictionary-search-at-point+

    "ps" 'pomodoro-start
    "pS" 'pomodoro-stop
  
    "qq" 'save-buffers-kill-terminal))


(use-package use-package-chords
  :ensure t
  :config
  (key-chord-mode 1)
  (setq-default key-chord-two-keys-delay 0.5)
  (key-chord-define evil-insert-state-map ",," 'evil-normal-state))


(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))


(provide 'init-evil)
;;; init-evil.el ends here
