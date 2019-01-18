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




(defun jk/md-export ()
  "Create front-matter of GFM-md.
Note that it just adapts to a title including most five words."
  (interactive)
  (save-buffer)				;; Save current buffer
  (setq fname (car (split-string (buffer-name) "\\."))) ;; if current buffer is "hello-world.org", FNAME will be "hello-world"
  (setq fnamel (split-string fname "-"))		;; Now, fnamel will be ("hello" "world")
  ;; Split the fnamel and join them with <space>, now ftitle will be "hello world"
  (setq ftitle (concat "---\ntitle: " (car fnamel) " " (car (cdr fnamel)) " " (car (cdr (cdr fnamel))) " " (car (cdr (cdr (cdr fnamel)))) " " (car (cdr (cdr (cdr (cdr fnamel))))) "\n---\n\n"))
  (write-region ftitle nil (concat fname ".md")) ;; Add ftitle to a file named "hello-world.md"
  (org-gfm-export-as-markdown)			 ;; Invoke gfm plugin to open a relative .md buffer
  (replace-string ".." "")			 ;; ! Just for hexo-blog's special img-show format...
  (append-to-file nil t (concat fname ".md"))	 ;; Append the contents of "hello-world.md" to "hello-world.md" again
  (kill-this-buffer)
  (switch-window-then-maximize)) ;; Kill the "hello-world.md" generate by GFM plugin to keep you stay in current .org file


(defun jk/insert-date ()
  "Insert current date."
  (interactive)
  (insert "#+DATE: ")
  (insert (current-time-string))
  (insert "\n\n"))


(provide 'init-orgs)
;;; init-orgs.el ends here
