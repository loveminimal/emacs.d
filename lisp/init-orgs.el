;;; init-orgs.el --- Org settings. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'init-vars)

(global-set-key (kbd "<f2>") 'org-edit-special)
(global-set-key (kbd "<f3>") 'org-edit-src-exit)
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

(require 'ox-md)

;; Store new notes at the beginning of a file or entry.
(setq org-reverse-note-order t)

;;;;;; GTD -- Personal Management.
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
  :config
  (setq org-capture-templates
        ;; Capital char means that with ANCHOR.
	'(
	  ("d" "diary" entry (file+headline "~/site/org/diary.org" "日记")
	   "* %u\n\n%?\n\n"
           :prepend 1
           :empty-lines 1)
	  ("i" "idea" entry (file+headline "~/site/org/idea.org" "闪念")
	   "* %U\n\n%?\n\n"
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
	  ("w" "wiki" entry (file+headline "~/site/org/wiki.org" "WIKI")
	   "*  %?\n%U"
           :prepend 1
           :empty-lines 1)
          ("c" "capture-everything" entry (file+headline "~/site/org/gtd.org" "IBX")
	   "* TODO  %?\n%U"
           :empty-lines 1)
          ("C" "CAPTURE-EVERYTHING" entry (file+headline "~/site/org/gtd.org" "IBX")
	   "* TODO  %?\n%U\n%a"
           :empty-lines 1)
	  )))

;; Tracking TODO state changes
;; '!' - for a timestamp
;; '@' - for a note with timestamp
;; '/!' - A timestamp should be recorded when entering/leaving the state

;; #+TODO: TODO(t) WAIT(w@/!) | DONE(d!) CANCELED(c@)
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w@)" "NEXT(n!)" "CALENDAR(c@)" "|" "DONE(D!/!)")
	(sequence "SOMEDAY(s@)" "REFER(r@)"  "|" "TRASH(T)")
	(sequence "PROJECT(p@)" "|" "DONE(D!/!)" "CANCELLED(C@/!)")
	(sequence "BUG(b@)" "KNOWNCAUSE(k@)" "|" "FIXED(F@/!)")
        (sequence "ANCHOR(a)" "|" "DONE(D)")))

(setq org-todo-keyword-faces
      '(("TODO" . "red")
	("WAIT" . "green")
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

(use-package org-agenda
  :bind ("C-c a" . org-agenda))

(use-package writeroom-mode
  :ensure t
  ;; :disabled
  ;; :hook (org-mode)
  )

(provide 'init-orgs)
;;; init-orgs.el ends here
