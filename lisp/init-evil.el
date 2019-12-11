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

    "//" 'org-sparse-tree

    "oo" 'org-export-dispatch

    "aa" 'org-agenda
    "cc" 'org-capture
    "jj" 'org-todo
    "os" 'org-schedule
    "od" 'org-deadline
    "mm" 'org-refile
    ;; "pp" 'org-priority


    "pp" 'pomidor
    "ps" 'pomidor-stop
    "pq" 'pomidor-quit
    "pr" 'pomidor-reset
    "pb" 'pomidor-break
  

    "bb" 'ido-switch-buffer
    "bj" 'ibuffer-sidebar-toggle-sidebar
    "bk" 'ido-kill-buffer

    
    "ff" 'ido-find-file
    "fi" 'open-init-file
    "fp" 'open-plan-file
    "fw" 'open-wiki-file
    "fn" 'open-notes-file
    "fo" 'org-open-at-point
    "fd" 'delete-this-file
    "fe" 'org-export-dispatch
    "fr" 'rename-this-file-and-buffer
    "fR" 'recentf-open-files
    "fj" 'dired-sidebar-toggle-sidebar
    "fs" 'save-buffer
    "fa" 'save-some-buffers

    "gg" 'magit-status
    
    "hf" 'describe-function
    "hk" 'describe-key
    "hv" 'describe-variable
    "hp" 'describe-package

    "ii" 'org-toggle-inline-images

    "ll" 'comment-line
  
    "mj" 'list-bookmarks
    "ms" 'bookmark-set
  
    "sa" 'mark-whole-buffer

    "ss" 'projectile-ripgrep
    
    "tt" 'toggle-truncate-lines
    "tw" 'toggle-frame-maximized
    "tf" 'toggle-frame-fullscreen
    
    "ww" 'switch-window
    "wo" 'switch-window-then-maximize
    "wr" 'split-window-right
    "wd" 'split-window-below
    "wk" 'delete-window
    "wh" 'winner-undo
    "wl" 'winner-redo
  
    "yy" 'youdao-dictionary-search-at-point+

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
