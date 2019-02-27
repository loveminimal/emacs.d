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
  :bind ("C-c l" . org-store-link)
  :config
  (add-hook 'org-mode-hook 'org-content))



;;; GTD -- Personal Management.

;; Capture, Refile, Archive

;; Setting up capture
;; (setq org-default-notes-file (concat org-directory "/notes.org"))

;; Using capture
;; 'C-c c' (org-capture) - with N prefix to visit the specific target location
;; 'C-c C-c' (org-capture-finalize) - back to the window before the capture process
;; 'C-c C-w' (org-capture-refile) - finalize the capture process by refiling the note to a different place
;; 'C-c C-k' (org-capture-kill) - abort the capture process

;; Capture templates
;; To use templates for different types of capture items, and for different target locations.
;; 'C-c c C' - customize the variable 'org-capture-templates'

;; Symbol Syntax
;; ("KEYS" "DESCRIPTION" TYPE TARGET
;;   "TEMPLATE" [PROPERTIES])

;; An Example
;; (setq org-capture-templates
;;       '(("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
;;          "* TODO %?\n  %i\n  %a")
;;         ("j" "Journal" entry (file+olp+datetree "~/org/journal.org")
;;          "* %?\nEntered on %U\n  %i\n  %a")))
;; =>
;; * TODO
;;   [[file:LINK TO WHERE YOU INITIATED CAPTURE]]

;; Template Expansion
;; '%'-escapes allow dynamic insertion of content in templates.
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
  :config
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-capture-templates
	'(("t" "todo" entry (file+headline "" "INBOX")	;; "" => `org-default-notes-file'
	   "* TODO %?\n%U\n%a")
	  ("n" "note" entry (file+headline "" "NOTES")
	   "* %? :NOTE:\n%U\n%a"))))

;; Refile and Copy
;; 'C-c M-w' (org-copy) - Copying works like refiling but not delete the original note.
;; 'C-c C-w' (org-refile) - Refile the entry or region at point.
;; 'C-u C-c C-w' - Use the refile interface to jump to a heading.
;; ...


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
;; 'S-<RIGHT>/<LEFT>' - Same as above
;; 'C-c / t' (org-show-todo-tree) - View TODO items in a _sparse tree_.
;; 'C-c a t' (org-todo-list) - Show the global TODO list.
;; 'S-M-<RET>' (org-insert-todo-heading) - Insert a new TODO entry below the current one.

;; Extended Use of TODO Keywords
;; To classify TODO items with _TODO keywords_ (stored in 'org-todo-keywords')

;; Fast Access to TODO States
;; You can set up keys for single-letter access to the states.

;; Progress Logging
;; Org mode can automatically record a timestamp and possibly a note when
;; you mark a TODO item as DONE, or
;; each time you change the state of a TODO item.

;; Closing items
;; (setq org-log-done 'time) - Each time you turn an entry from TODO to DONE,
;; a line 'CLOSED: [timestamp]' will be inserted just after the headline.
;; (setq org-log-done 'note) - To record a note along with the timestamp.
;; '#+STARTUP: logdone' & '#+STARTUP: lognotedone'

;; Tracking TODO state changes
;; '!' - for a timestamp
;; '@' - for a note with timestamp
;; '/!' - A timestamp should be recorded when entering/leaving the state

;; (setq org-todo-keywords
;;       '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))
;; or
;; #+TODO: TODO(t) WAIT(w@/!) | DONE(d!) CANCELED(c@)

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "CALENDAR(c)" "|" "DONE(D)")
	(sequence "SOMEDAY(s)" "REFER(r)"  "|" "TRASH(T)")
	(sequence "PROJECT(p)" "|" "DONE(D)" "CANCELLED(C)")
	(sequence "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(F)")))

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
;; with a 'C-u' prefix argument, ignore headlines that are not a TODO line.
;; 'C-c a m' (org-tags-view) - Create a global list of tag matches from all agenda file.
;; 'C-c a M' (org-tags-view) - Same as above but only TODO items.


;; Agenda Views
;; 'C-c [' (org-agenda-file-to-front) - Add current file to the list of agenda files.
;; 'C-c ]' (org-remove-file) - Remove current file from the list of agenda files.
;; 'C-c a' (org-agenda) - It will prompt for a command to execute.
;; More commands can be added by configuring the variable 'org-agenda-custom-commands'
;; If the current buffer is in Org mode and visiting a file, you can also
;; first press '<' once to indicate that the agenda should be temporarily (until the next use of 'C-c a'),
;; press '<' twice means to restrict to the current subtree or region (if active).

;; The built-in agenda views
;; weekly/daily agenda
;; 'C-c a a' (org-agenda-list) - Compile an agenda for the current week from a list of Org files.
;; Calendar/Diary integration
;; In order to include entries from the Emacs diary into Org mode's agenda, you only need:
;; (setq org-agenda-include-diary t)
;; <SPC> <TAB> and <RET> can be used from the agenda buffer to jump to the diary file

(use-package org-agenda
  :bind ("C-c a" . org-agenda))


;; Dates and Times
;; Creating timestamps
;; 'C-c .' (org-time-stamp)
;; 'C-c !' (org-time-stamp-inactive)
;; 'C-c C-c' - Normalize timestamp, insert/fix day name if missing or wrong
;; 'C-c <' (org-date-from-calendar)
;; 'C-c >' (org-goto-calendar)
;; 'C-c C-o' (org-open-at-point)
;; 'S-<LEFT>/<RIGHT>' (org-timestamp-down/up-day)
;; 'S-<UP>/<DOWN>' (org-timestamp-up/down-down)
;; 'C-c C-y' (org-evaluate-time-range)

;; Deadlines and Scheduling - Both the timestamp and the keyword
;; have to be positioned immediately after the task they refer to.
;; DEADLINE - the task is supposed to be finished on the given date
;; SCHEDULED - the task is planned to be started on the given date

;; Insert deadlines or schedules
;; 'C-c C-d' (org-deadline) - Insert 'DEADLINE' keyword along with a stamp.
;; 'C-c C-s' (org-schedule) - Insert 'SCHEDULE' keyword along with a stamp.
;; 'C-c / d' (org-check-deadlines) - Create a sparse tree with all deadlines
;; ...








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
