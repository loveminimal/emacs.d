;;; init-misc.el --- Stage configurations be unused or temporary.
;;; Commentary:
;;; Code:


;;; org2md.el -- A package render org to HEXO's md.

(use-package ox-gfm
  :ensure t)

;; (setq org-export-with-toc nil)

(defun org2md-insert-date ()
  "Insert current date."
  (interactive)
  (goto-char (point-min))
  (insert "#+DATE: ")
  ;; (org-time-stamp t)
  (insert (format-time-string "%Y/%m/%d %T"))
  (insert "\n\n"))


(defun org2md-export-md ()
  "Export .org to .md which will be added Front-matter."
  (interactive)
  (save-buffer)
  (let ((filename (car (split-string (buffer-name) "\\.")))
	(filedir "../_posts/"))
	
	(let ((filename-dot-md (concat filedir filename ".md"))
	      (filename-list (split-string filename "-")))
	  (let ((file-title (concat "---\ntitle: "
				  (car filename-list) " "
				  (car (cdr filename-list)) " "
				  (car (cdr (cdr filename-list))) " "
				  (car (cdr (cdr (cdr filename-list)))) " "
				  (car (cdr (cdr (cdr (cdr filename-list))))) " "
				  (car (cdr (cdr (cdr (cdr (cdr filename-list)))))) " "
				  (car (cdr (cdr (cdr (cdr (cdr (cdr filename-list))))))) " "
				  (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr filename-list)))))))) "\n")))
	    (write-region file-title nil filename-dot-md)
	    (goto-char (point-min))
	    (forward-char 8)
	    (let (p1 p2)
	      (setq p1 (point))
	      (end-of-line)
	      (setq p2 (point))
	      (append-to-file "date: " nil filename-dot-md)
	      (append-to-file p1 p2 filename-dot-md)
	      (append-to-file "\nupdated: " nil filename-dot-md)
	      (append-to-file (format-time-string "%Y/%m/%d %T") nil filename-dot-md)
	      (append-to-file "\n---\n\n" nil filename-dot-md)
	      (org-gfm-export-as-markdown)
	      (while (search-forward ".." nil t)
		(replace-match "" nil t))
	      (append-to-file nil t filename-dot-md)
	      (kill-buffer)
	      (switch-window-then-maximize))))))



;; (setq org-export-with-toc nil)
(defun org2md-without-toc ()
  "Mainly export a chinese doc without toc for JIANSHU."
  (interactive)
  (setq org-export-with-toc nil)
  (org-gfm-export-as-markdown)
  (delete-other-windows)
  (setq org-export-with-toc t))









(provide 'init-misc)
;;; init-misc.el ends here
