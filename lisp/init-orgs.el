;;; init-orgs.el --- Org settings.
;;; Commentary:
;;; Code:

;; (image-type-available-p 'imagemagick)    ;; t
(setq org-image-actual-width (/ (display-pixel-width) 3))
;; (setq org-image-actual-width 800)
;; it's okay now, to exec command like 'org-toggle-inline-images' but not toggle 'iimage-mode'

 (use-package org
  :init
  (setq org-hide-emphasis-markers nil
	org-src-fontify-natively t
        org-hide-block-startup t
	org-tags-column 80)
  :bind ("C-c l" . org-store-link)
  :config
  (add-hook 'org-mode-hook 'org-content))


 (use-package ox-md)

 ;;; GTD -- Personal Management.

;; Template Expansion
;; %  -escapes allow dynamic insertion of content in templates.
;; %t - Timestamp, date only
;; %T - Timestamp, with date and time
;; %u, %U - Like the above, but inactive timestamps
;; %i - Initial content, the region when capture is called while the region is active
;; %a - Annotation, normally the link created with 'org-store-link'
;; %A - Like %a, but prompt for the description part
;; %l - Like %a, but only insert the literal link.
;; %n - User name (taken from 'user-full-name')
;; %? - After completing the template, position cursor here.

 (use-package org-capture
  :bind ("C-c c" . org-capture)
  :init
  ;; (setq org-default-notes-file "~/.gtd/notes.org")
  (setq org-default-notes-file "~/site/org/notes.org")
  (defun open-notes-file ()
    "Quickly open notes."
    (interactive)
    (find-file org-default-notes-file))
  :config
  ;; (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-capture-templates
	'(
	  ("d" "diary" entry (file+headline "~/site/org/diary.org" "日记")
	   "* %t\n\n%?\n\n-----"
           :empty-lines 1)
	  ("i" "idea" entry (file+headline "~/site/org/idea.org" "闪念")
	   "* %T\n\n%?\n\n-----"
           :empty-lines 1)
	  ("s" "story" entry (file+headline "~/site/org/story.org" "故事")
	   "*  %?\n%U"
           :empty-lines 1)
	  ("f" "fragment" entry (file+headline "~/site/org/fragment.org" "FRAGMENT")
	   "*  %?\n%U"
           :empty-lines 1)
	  ("j" "just-todo" entry (file+headline "" "INBOX")
	   "* TODO  %?\n%U")
	  ("c" "capture-todo" entry (file+headline "" "INBOX")
	   "* TODO  %?\n%U\n%a")
	  ("n" "note" entry (file+headline "" "NOTES")	;; "" => `org-default-notes-file'
	   "* %? :@note:\n%U\n%a")
	  )))

;; Refile and Copy
;; 'C-c M-w' (org-copy) - Copying works like refiling but not delete the original note.
;; 'C-c C-w' (org-refile) - Refile the entry or region at point.
;; 'C-u C-c C-w' - Use the refile interface to jump to a heading.


;; Archiving
;; 'C-c C-x C-a' (org-archive-subtree-default)
;; Moving subtrees
;; 'C-c C-x C-s' or 'C-x $' (org-archive-subtree) - Archive the subtree starting at the
;; cursor position to the location given by 'org-archive-location'.
;; The default archive location is a file in the same directory as the current file, with
;; the name derived by appending '_archive' to the current file name.
;; or
;; #+ARCHIVE: %s_done::



;; TODO
;; 'C-c C-t' (org-todo) - Rotate the TODO state of the current item

;; Tracking TODO state changes
;; '!' - for a timestamp
;; '@' - for a note with timestamp
;; '/!' - A timestamp should be recorded when entering/leaving the state

;; #+TODO: TODO(t) WAIT(w@/!) | DONE(d!) CANCELED(c@)

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n!)" "CALENDAR(c@)" "|" "DONE(D!/!)")
	(sequence "SOMEDAY(s@)" "REFER(r@)"  "|" "TRASH(T)")
	(sequence "PROJECT(p@)" "|" "DONE(D!/!)" "CANCELLED(C@/!)")
	(sequence "BUG(b@)" "KNOWNCAUSE(k@)" "|" "FIXED(F@/!)")))

(setq org-todo-keyword-faces
      '(("TODO" . "red")
	("NEXT" . "green")
	("DONE" . "grey")
	("SOMEDAY" . "yellow")
	("REFER" . (:background "gold" :foreground "white" :weight bold))
	("PROJECT" . (:background "blue" :foreground "white" :weight bold))
	("TRASH" . "grey")
	("CANCELLED" . "lightblue")
	("BUG" . "red")
	("KNOWNCAUSE" . "yellow")
	("FIXED" . "grey")))


;; TAGS
;; 'C-c C-q' (org-set-tags-command) - Enter new tags for the current headline
;; 'C-c C-c' (org-set-tags-command) - Same as above when the cursor is in a headline.

(setq org-tag-alist
      '((:startgroup . nil)
	("@work" . ?w)
	("@life" . ?l)
	(:endgroup . nil)
	("@misc" . ?m)))

;; Tag searches
;; 'C-c / m' or 'C-c \' (org-match-sparse-tree) - Create a sparse tree with all headlines matching a tags/property/TODO search.
;; 'C-c a m' (org-tags-view) - Create a global list of tag matches from all agenda file.


;; Agenda Views
;; 'C-c [' (org-agenda-file-to-front) - Add current file to the list of agenda files.
;; 'C-c ]' (org-remove-file) - Remove current file from the list of agenda files.
;; 'C-c a' (org-agenda) - It will prompt for a command to execute.
;; If the current buffer is in Org mode and visiting a file, you can also
;; - press '<' once to indicate that the agenda should be temporarily (until the next use of 'C-c a'),
;; - press '<' twice means to restrict to the current subtree or region (if active).

;; The built-in agenda views
;; 'C-c a a' (org-agenda-list) - Compile an agenda for the current week from a list of Org files.
;; <SPC> <TAB> and <RET> can be used from the agenda buffer to jump to the diary file

(use-package org-agenda
  :bind ("C-c a" . org-agenda))


;; Dates and Times

;; DEADLINE - the task is supposed to be finished on the given date
;; SCHEDULED - the task is planned to be started on the given date

;; Insert deadlines or schedules
;; 'C-c C-d' (org-deadline) - Insert 'DEADLINE' keyword along with a stamp.
;; 'C-c C-s' (org-schedule) - Insert 'SCHEDULE' keyword along with a stamp.
;; 'C-c / d' (org-check-deadlines) - Create a sparse tree with all deadlines

(use-package ox-hugo
  :ensure t
  :disabled
  :after ox)

(use-package org-hugo-auto-export-mode
  :disabled
  :hook (org-mode))

(use-package pomidor
  :ensure t
  :disabled
  :bind (("<f12>" . pomidor))
  :config (setq pomidor-sound-tick nil
                pomidor-sound-tack nil
                pomidor-seconds (* 25 60) ;; 25 minutes for the work period
                pomidor-break-seconds (* 5 60) ;; 5 minutes break time
                )
  :hook (pomidor-mode . (lambda ()
                          (display-line-numbers-mode -1) ; Emacs 26.1+
                          (setq left-fringe-width 0 right-fringe-width 0)
                          (setq left-margin-width 2 right-margin-width 0)
                          ;; force fringe update
                          (set-window-buffer nil (current-buffer)))))

;; If your Emacs cannot play sounds you can provide your own function to do it
;; (setq pomidor-play-sound-file
;;       (lambda (file)
;;         (start-process "my-pomidor-play-sound"
;;                        nil
;;                        "mplayer"
;;                        file)))


(use-package htmlize
  :ensure t
  ;; :config
  ;; (setq htmlize-output-type 'font)
  )

  
(use-package ox-html
  :config
  (setq
   ;; org-html-doctype "html5"
   ;; org-export-default-language "ch"
   user-full-name "Jack Liu"))

(progn
  "Settings of `org-export'."
  (setq org-export-in-background t
        ;; Hide html built-in style and script.
        org-html-htmlize-output-type 'inline-css
        org-html-head-include-default-style nil
        org-html-head-include-scripts nil
        ))


(use-package ox-publish
  :config
  
  ;; org-publish-project-alist
  ;; ("project-name" :property value :property value ...)
  ;; ("project-name" :components ("project-name" "project-name" ...))
  
  (setq org-publish-project-alist
        '(("orgfiles"
           ;; ; Sources and destinations for files.
           :base-directory "~/site/org/"  ;; local dir
           :publishing-directory "~/site/public/" ;; :publishing-directory "/ssh:jack@192.112.245.112:~/site/public/"
           ;; :preparation-function
           ;; :complete-function

           ;; ; Selecting files
           :base-extension "org"
           ;; :exclude "PrivatePage.org"     ;; regexp
           ;; :include
           :recursive t

           ;; ; Publishing action
           :publishing-function org-html-publish-to-html
           ;; :htmlized-source


           ;; ;;; Options for the exporters

           ;; ; Generic properties
           ;; :archived-trees	org-export-with-archived-trees
           ;; :exclude-tags	org-export-exclude-tags
           :headline-levels 4 ;; org-export-headline-levels
           ;; :language	org-export-default-language
           ;; :preserve-breaks	org-export-preserve-breaks
           :section-numbers nil	;; org-export-with-section-numbers
           ;; :select-tags	org-export-select-tags
           :with-author "Jack Liu" ;; org-export-with-author
           ;; :with-broken-links	org-export-with-broken-links
           ;; :with-clocks	t ;; org-export-with-clocks
           ;; :with-creator nil ;; org-export-with-creator
           ;; :with-date org-export-with-date
           ;; :with-drawers	org-export-with-drawers
           ;; :with-email	org-export-with-email
           ;; :with-emphasize	org-export-with-emphasize
           ;; :with-fixed-width org-export-with-fixed-width
           ;; :with-footnotes	org-export-with-footnotes
           ;; :with-latex	org-export-with-latex
           ;; :with-planning	org-export-with-planning
           :with-priority t ;; org-export-with-priority ;
           ;; :with-properties	org-export-with-properties
           ;; :with-special-strings	org-export-with-special-strings
           ;; :with-sub-superscript	org-export-with-sub-superscripts
           ;; :with-tables	org-export-with-tables
           ;; :with-tags	org-export-with-tags
           ;; :with-tasks	org-export-with-tasks
           ;; :with-timestamps	org-export-with-timestamps
           ;; :with-title	org-export-with-title
           :with-toc t ;; org-export-with-toc
           ;; :with-todo-keywords	org-export-with-todo-keywords
 
 
           ;; ; HTML specific properties
           ;; :html-allow-name-attribute-in-anchors	org-html-allow-name-attribute-in-anchors
           ;; :html-checkbox-type	org-html-checkbox-type
           ;; :html-container	org-html-container-element
           ;; :html-divs	org-html-divs
           :html-doctype "html5" ;; org-html-doctype
           ;; :html-extension	org-html-extension
           ;; :html-footnote-format nil ;; org-html-footnote-format
           ;; :html-footnote-separator	org-html-footnote-separator
           ;; :html-footnotes-section	org-html-footnotes-section
           ;; :html-format-drawer-function	org-html-format-drawer-function
           ;; :html-format-headline-function	org-html-format-headline-function
           ;; :html-format-inlinetask-function	org-html-format-inlinetask-function
           ;; :html-head-extra	org-html-head-extra
           ;; :html-head-include-default-style	org-html-head-include-default-style
           ;; :html-head-include-scripts	org-html-head-include-scripts
           ;; :html-head	org-html-head
           ;; :html-home/up-format	org-html-home/up-format
           ;; :html-html5-fancy	org-html-html5-fancy
           ;; :html-indent	org-html-indent
           ;; :html-infojs-options	org-html-infojs-options
           ;; :html-infojs-template	org-html-infojs-template
           ;; :html-inline-image-rules	org-html-inline-image-rules
           ;; :html-inline-images	org-html-inline-images
           ;; :html-link-home	org-html-link-home
           ;; :html-link-org-files-as-html	org-html-link-org-files-as-html
           ;; :html-link-up	org-html-link-up
           ;; :html-link-use-abs-url	org-html-link-use-abs-url
           ;; :html-mathjax-options	org-html-mathjax-options
           ;; :html-mathjax-template	org-html-mathjax-template
           ;; :html-metadata-timestamp-format	org-html-metadata-timestamp-format
           ;; :html-postamble-format t ;; org-html-postamble-format
           ;; :html-postamble t ;; org-html-postamble
           ;; :html-preamble-format	org-html-preamble-format
           ;; :html-preamble nil ;; org-html-preamble
           ;; :html-self-link-headlines	org-html-self-link-headlines
           ;; :html-table-align-individual-field	de{org-html-table-align-individual-fields
           ;; :html-table-attributes	org-html-table-default-attributes
           ;; :html-table-caption-above	org-html-table-caption-above
           ;; :html-table-data-tags	org-html-table-data-tags
           ;; :html-table-header-tags	org-html-table-header-tags
           ;; :html-table-row-tags	org-html-table-row-tags
           ;; :html-table-use-header-tags-for-first-column	org-html-table-use-header-tags-for-first-column
           ;; :html-tag-class-prefix	org-html-tag-class-prefix
           ;; :html-text-markup-alist	org-html-text-markup-alist
           ;; :html-todo-kwd-class-prefix	org-html-todo-kwd-class-prefix
           ;; :html-toplevel-hlevel	org-html-toplevel-hlevel
           ;; :html-use-infojs	org-html-use-infojs
           ;; :html-validation-link	org-html-validation-link
           ;; :html-viewport	org-html-viewport
           ;; :html-wrap-src-lines	org-html-wrap-src-lines
           ;; :html-xml-declaration	org-html-xml-declaration
 
 
           ;; ; Markdown specific properties
           ;; :md-footnote-format	org-md-footnote-format
           ;; :md-footnotes-section	org-md-footnotes-section
           ;; :md-headline-style	org-md-headline-style


           ;; ; Other options
           :table-of-contents t
           ;; :style "<link rel=\"stylesheet\" href=\"../other/mystyle.css\" type=\"text/css\" />"
           )
          ;; static assets
          ("js"
           :base-directory "~/site/js/"
           :base-extension "js"
           :publishing-directory "~/site/public/js/"
           :recursive t
           :publishing-function org-publish-attachment
           )
          ("css"
           :base-directory "~/site/css/"
           :base-extension "css"
           :publishing-directory "~/site/public/css/"
           :recursive t
           :publishing-function org-publish-attachment
           )
          ("images"
           :base-directory "~/site/images/"
           :base-extension "jpg\\|gif\\|png\\|svg\\|gif"
           :publishing-directory "~/site/public/images/"
           :recursive t
           :publishing-function org-publish-attachment
           )
          ("website" :components ("orgfiles" "js" "css" "images")
           ))))


(defun save-and-publish-project ()
    "Save all buffers and publish."
  (interactive)
  (save-some-buffers t)
  (org-publish-current-project t))


(defun save-and-publish-file ()
    "Save current buffer and publish."
  (interactive)
  (save-buffer t)
  (org-publish-current-file t))


(define-minor-mode auto-save-and-publish-file-mode
  "Toggle auto save and publish current file."
  :global nil
  :lighter ""
  (if auto-save-and-publish-file-mode
      ;; When the mode is enabled
      (progn
        (add-hook 'after-save-hook #'save-and-publish-file :append :local))
    ;; When the mode is disabled
    (remove-hook 'after-save-hook #'save-and-publish-file :local)))


(use-package auto-save-and-publish-file-mode
  :hook (org-mode))


(use-package simple-httpd
  :ensure t
  :config
  (setq httpd-root "~/site/public"))


(defun preview-current-buffer-in-browser ()
  "Open current buffer as html."
  (interactive)
  (let ((fileurl (concat "http://127.0.0.1:8080/" (file-name-base (buffer-name)) ".html")))
    (save-and-publish-file)
    (httpd-start)
    (browse-url fileurl)))


(defun delete-org-and-html ()
  "Delete the relative html when it exists."
  (interactive)
  (let ((fileurl (concat "~/site/public/" (file-name-base (buffer-name)) ".html")))
    (if (file-exists-p fileurl)
        (delete-file fileurl))
    (delete-this-file)))


(defun org-open-at-point-and-delete-other-windows ()
    "Open link file and just keep the goal file."
  (interactive)
  (org-open-at-point)
  (delete-other-windows))

(provide 'init-orgs)
;;; init-orgs.el ends here
