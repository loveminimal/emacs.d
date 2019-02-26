;;; init-orgs.el --- Org settings.
;;; Commentary:
;;; Code:

;; (image-type-available-p 'imagemagick)    ;; t
(setq org-image-actual-width (/ (display-pixel-width) 3))
;; (setq org-image-actual-width 800)
;; it's okay now, to exec command like 'org-toggle-inline-images' but not toggle 'iimage-mode'

(use-package org
  :init
  (setq org-hide-emphasis-markers t
	org-src-fontify-natively t
	org-tags-column 80)
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture))
  :config
  (add-hook 'org-mode-hook 'org-content))


;;; GTD -- Personal Management.
;; Capture, Refile, Archive

;; Setting up capture
(setq org-default-notes-file (concat org-directory "/notes.org"))

;; Using capture
;; 'C-c c' (org-capture) - with N prefix to visit the specific target location
;; 'C-c C-c' (org-capture-finalize) - back to the window before the capture process
;; 'C-c C-w' (org-capture-refile) - finalize the capture process by refiling the note to a different place
;; 'C-c C-k' (org-capture-kill) - abort the capture process

;; Capture templates
;; To use templates for different types of capture items, and for different target locations.
;; 'C-c c C' - customize the variable 'org-capture-templates'

;; ("KEYS" "DESCRIPTION" TYPE TARGET
;;   "TEMPLATE")



















;;; jk-org2md-hexo.el -- A package render org to HEXO's md.

(use-package ox-gfm
  :ensure t)


(defun jk/md-export ()
  "Export org to markdown which will be added Front-matter."
  (interactive)
  (save-buffer)	                                        ;; Save current buffer
  (setq fname (car (split-string (buffer-name) "\\."))) ;; if current buffer is "hello-world.org", FNAME will be "hello-world"
  (setq fdir "../_posts/")				;; Set exported markdown dir
  (setq fnamed (concat fdir fname ".md"))		;; "hello-world.md"
  (setq fnamel (split-string fname "-"))		;; Now, FNAMEL will be ("hello" "world")
  ;; Split the FNAMEL and join them with <space>, now FTITLE will be "hello world"
  (setq ftitle (concat "---\ntitle: "
		       (car fnamel) " "
		       (car (cdr fnamel)) " "
		       (car (cdr (cdr fnamel))) " "
		       (car (cdr (cdr (cdr fnamel)))) " "
		       (car (cdr (cdr (cdr (cdr fnamel))))) "\n"))
  (write-region ftitle nil fnamed) ;; Add FTITLE to a file named "hello-world.md"
  ;; Append date like "date: 2019/02/10 10:47:56" to "hello-world.md"
  (beginning-of-buffer)
  (forward-char 8)
  (let (p1 p2)
    (setq p1 (point))
    (end-of-line)
    (setq p2 (point))
    (append-to-file "date: " nil fnamed)
    (append-to-file p1 p2 fnamed)
    (append-to-file "\nupdated: " nil fnamed)
    (append-to-file (format-time-string "%Y/%m/%d %T") nil fnamed)
    (append-to-file "\n---\n\n" nil fnamed))
  (org-gfm-export-as-markdown)			 ;; Invoke gfm plugin to open a relative .md buffer
  (replace-string ".." "")			 ;; ! Just for hexo-blog's special img-show format...
  (append-to-file nil t fnamed)	 ;; Append the contents of "hello-world.md" to "hello-world.md" again
  (kill-this-buffer)
  (switch-window-then-maximize)) ;; Kill the "hello-world.md" generate by GFM plugin to keep you stay in current .org file


(defun jk/insert-date ()
  "Insert current date."
  (interactive)
  (beginning-of-buffer)
  (insert "#+DATE: ")
  ;; (org-time-stamp t)
  (insert (format-time-string "%Y/%m/%d %T"))
  (insert "\n\n"))




(provide 'init-orgs)
;;; init-orgs.el ends here
