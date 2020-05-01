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
        ;; Capital char means that with ANCHOR.
	'(
	  ("d" "diary" entry (file+headline "~/site/org/diary.org" "日记")
	   "* %u\n\n%?\n\n"
           :prepend 1
           :empty-lines 1)
	  ("D" "DIARY" entry (file+headline "~/site/org/diary.org" "日记")
	   "* %u\n%a\n\n%?\n\n"
           :prepend 1
           :empty-lines 1)
	  ("i" "idea" entry (file+headline "~/site/org/idea.org" "闪念")
	   "* %U\n\n%?\n\n"
           :prepend 1
           :empty-lines 1)
	  ("I" "IDEA" entry (file+headline "~/site/org/idea.org" "闪念")
	   "* %U\n%a\n\n%?\n\n"
           :prepend 1
           :empty-lines 1)
	  ("j" "joker" entry (file+headline "~/site/org/joker.org" "JOKER")
	   "* %U\n\n%?\n\n"
           :prepend 1
           :empty-lines 1)
	  ("s" "story" entry (file+headline "~/site/org/story.org" "故事")
	   "*  %?\n%U"
           :prepend 1
           :empty-lines 1)
	  ("S" "STORY" entry (file+headline "~/site/org/story.org" "故事")
	   "*  %?\n%U\n%a"
           :prepend 1
           :empty-lines 1)
	  ("w" "wiki" entry (file+headline "~/site/org/wiki.org" "WIKI")
	   "*  %?\n%U"
           :prepend 1
           :empty-lines 1)
	  ("W" "WIKI" entry (file+headline "~/site/org/wiki.org" "WIKI")
	   "*  %?\n%U\n%a"
           :prepend 1
           :empty-lines 1)
          ("c" "capture-everything" entry (file+headline "~/site/org/gtd.org" "IBX")
	   "* TODO  %?\n%U"
           :empty-lines 1)
          ("C" "CAPTURE-EVERYTHING" entry (file+headline "~/site/org/gtd.org" "IBX")
	   "* TODO  %?\n%U\n%a"
           :empty-lines 1)
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
	(sequence "BUG(b@)" "KNOWNCAUSE(k@)" "|" "FIXED(F@/!)")
        (sequence "ANCHOR(a)" "|" "DONE(D)")))

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
      '(
        (:startgroup . gtd)
        ("@work" . ?w)
        ("@life" . ?l)
        (:endgroup . gtd)

	("@misc" . ?m)
        ))

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

(provide 'init-orgs)
;;; init-orgs.el ends here
