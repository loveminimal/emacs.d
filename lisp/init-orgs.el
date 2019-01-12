;;; init-orgs.el --- Get Things Done.
;;; Commentary:
;;; Code:

(setq org-hide-emphasis-markers t
      org-src-fontify-natively t
      org-tags-column 80)


(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)


;; Capture
(global-set-key (kbd "C-c c") 'org-capture)















(provide 'init-orgs)
;;; init-orgs.el ends here
