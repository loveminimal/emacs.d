;; -*- lexical-binding: t -*-
;; -------------------------------------------------------------------
;; GNU Emacs / N Λ N O - Emacs made simple
;; Copyright (C) 2020 - N Λ N O developers 
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>
;; -------------------------------------------------------------------
;;
;; Nano mode line format:
;;
;; [ status | name (primary)               secondary | item1 | item2 ]
;;
;; -------------------------------------------------------------------
(require 'subr-x)


;; -------------------------------------------------------------------
(defun vc-branch ()
  (if vc-mode
      (let ((backend (vc-backend buffer-file-name)))
        (concat "#" (substring-no-properties vc-mode
                                 (+ (if (eq backend 'Hg) 2 3) 2))))  nil))


;; From https://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html
;; ---------------------------------------------------------------------
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat "…/" output)))
    output))

;; ---------------------------------------------------------------------
(defun nano-modeline-make-action (symbol function)
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] function)
    (propertize (format " %s " symbol)
		'keymap map
		'face       'nano-face-header-default
		'pointer 'hand
		;; Bug on OSX when mouse-face is defined
		;; -> Unable to load color "#FFFFFF"
		;; 'mouse-face 'nano-face-header-highlight
		)))

(defun nano-modeline-default-actions ()
  (let* ((window    (get-buffer-window (current-buffer)))
         (term-mode (not (display-graphic-p)))
         (dedicated (window-dedicated-p window)))
    (cond ;;(term-mode '())
          (dedicated '(("•" . nano-modeline-action-unset-dedicated)))
          (t         '(("<" . nano-modeline-action-prev-buffer)
	               (">" . nano-modeline-action-next-buffer))))))

(defun nano-modeline-action-prev-buffer ()
  (interactive)
  (previous-buffer))

(defun nano-modeline-action-next-buffer ()
  (interactive)
  (next-buffer))

(defun nano-modeline-action-unset-dedicated ()
  (interactive)
  (set-window-dedicated-p (get-buffer-window (current-buffer)) nil))


(defun nano-modeline-compose (status name primary secondary &optional actions)
  "Compose a string with provided information"
  (let* ((char-width    (window-font-width nil 'header-line))
         (window        (get-buffer-window (current-buffer)))
	 (actions       (or actions (nano-modeline-default-actions)))
	 (actions-length (apply '+ (mapcar 'length (mapcar 'car actions))))
	 (filler        (make-string
			 (max 0 (- char-width 2 (length actions))) ?\ ))
         (space-up       +0.15)
         (space-down     -0.20)
	 (gui            (display-graphic-p))
	 (prefix (cond ((string= status "RO")
			(propertize " RO " 'face 'nano-face-header-popout))
                       ((string= status "**")
			(propertize " ** " 'face 'nano-face-header-critical))
                       ((string= status "RW")
			(propertize " RW " 'face 'nano-face-header-faded))
                       (t (propertize status 'face 'nano-face-header-popout))))
         (left (concat
                (propertize " "  'face 'nano-face-header-default
			         'display `(raise ,space-up))
                (propertize name 'face 'nano-face-header-strong)
                (propertize " "  'face 'nano-face-header-default
			         'display `(raise ,space-down))
		(propertize primary 'face 'nano-face-header-default)))
         (right (concat secondary " "))
         (available-width (- (window-total-width) 1
			     (* 2 (length actions)) actions-length
			     (length prefix) (length left) (length right)
			     (/ (window-right-divider-width) char-width)))
	 (available-width (max 1 available-width)))
    (concat prefix
	    (if gui
		(propertize " " 'face 'nano-face-header-separator))
	    left
	    (propertize (make-string available-width ?\ ) 'face 'nano-face-header-default)
	    (if gui
		(propertize filler 'face 'nano-face-header-filler)
	      (propertize " " 'face 'nano-face-header-filler))
	    (propertize right 'face 'nano-face-header-default)
	    (if gui 
		(propertize " "   'face 'nano-face-header-separator))
	    (mapconcat (lambda (action)
			 (nano-modeline-make-action (car action) (cdr action)))
		       actions
		       (if gui
			   (propertize " " 'face 'nano-face-header-separator)
			 "")))))

;; ---------------------------------------------------------------------
(defun nano-modeline-mu4e-dashboard-mode-p ()
  (bound-and-true-p mu4e-dashboard-mode))
  
(defun nano-modeline-mu4e-dashboard-mode ()
  (nano-modeline-compose (nano-modeline-status)
                         "Mail"
                         (nano-modeline-mu4e-context)
                         ""))

;; ---------------------------------------------------------------------
(defun nano-modeline-elfeed-search-mode-p ()
  (derived-mode-p 'elfeed-search-mode))

(defun nano-modeline-elfeed-search-mode ()
  (nano-modeline-compose (nano-modeline-status)
                         "Elfeed"
                         (concat "(" (elfeed-search--header)  ")")
                         ""))

;; Elfeed (regular header)
(with-eval-after-load 'elfeed
  (defun elfeed-setup-header ()
    (setq header-line-format (default-value 'header-line-format)))
  (setq elfeed-search-header-function #'elfeed-setup-header))

;; ---------------------------------------------------------------------
(defun nano-modeline-elfeed-show-mode-p ()
  (derived-mode-p 'elfeed-show-mode))

(defun nano-modeline-elfeed-show-mode ()
  (let* ((title        (elfeed-entry-title elfeed-show-entry))
         (tags         (elfeed-entry-tags elfeed-show-entry))
         (tags-str     (mapconcat #'symbol-name tags ", "))
         (date         (seconds-to-time (elfeed-entry-date elfeed-show-entry)))
         (feed         (elfeed-entry-feed elfeed-show-entry))
         (feed-title   (plist-get (elfeed-feed-meta feed) :title))
         (entry-author (elfeed-meta elfeed-show-entry :author)))
    (nano-modeline-compose (nano-modeline-status)
                           (s-truncate 40 title "…")
                           (concat "(" tags-str ")")
                           feed-title)))

;; ---------------------------------------------------------------------
(defun nano-modeline-calendar-mode-p ()
  (derived-mode-p 'calendar-mode))

(defun nano-modeline-calendar-mode () "")

;; Calendar (no header, only overline)
(with-eval-after-load 'calendar
  (defun calendar-setup-header ()
    (setq header-line-format "")
    (face-remap-add-relative
     'header-line `(:overline ,(face-foreground 'default)
                    :height 0.5
                    :background ,(face-background 'default))))
  (add-hook 'calendar-initial-window-hook #'calendar-setup-header)

  ;; From https://emacs.stackexchange.com/questions/45650
  (add-to-list 'display-buffer-alist
               `(,(rx string-start "*Calendar*" string-end)
                 (display-buffer-below-selected))))

;; ---------------------------------------------------------------------
(defun nano-modeline-org-capture-mode-p ()
  (bound-and-true-p org-capture-mode))

(defun nano-modeline-org-capture-mode ()
  (nano-modeline-compose (nano-modeline-status)
                         "Capture"
                         "(org)"
                         ""
			 '(("CAPTURE" . org-capture-finalize))))

(with-eval-after-load 'org-capture
  (defun org-capture-turn-off-header-line ()
    (setq-local header-line-format (default-value 'header-line-format))
    ;; (fit-window-to-buffer nil nil 8)
    ;; (face-remap-add-relative 'header-line '(:background "#ffffff"))
    (message nil))
  (add-hook 'org-capture-mode-hook
            #'org-capture-turn-off-header-line))

;; ---------------------------------------------------------------------
(setq Info-use-header-line nil)
(defun nano-modeline-info-breadcrumbs ()
  (let ((nodes (Info-toc-nodes Info-current-file))
        (cnode Info-current-node)
	(node Info-current-node)
        (crumbs ())
        (depth Info-breadcrumbs-depth)
	line)
    (while  (> depth 0)
      (setq node (nth 1 (assoc node nodes)))
      (if node (push node crumbs))
      (setq depth (1- depth)))
    (setq crumbs (cons "Top" (if (member (pop crumbs) '(nil "Top"))
			         crumbs (cons nil crumbs))))
    (forward-line 1)
    (dolist (node crumbs)
      (let ((text
	     (if (not (equal node "Top")) node
	       (format "%s"
		       (if (stringp Info-current-file)
			   (file-name-sans-extension
			    (file-name-nondirectory Info-current-file))
			 Info-current-file)))))
	(setq line (concat line (if (null line) "" " > ")
                                (if (null node) "..." text)))))
    (if (and cnode (not (equal cnode "Top")))
        (setq line (concat line (if (null line) "" " > ") cnode)))
    line))

(defun nano-modeline-info-mode-p ()
  (derived-mode-p 'Info-mode))

(defun nano-modeline-info-mode ()
  (nano-modeline-compose (nano-modeline-status)
                         "Info"
                         (concat "("
                                 (nano-modeline-info-breadcrumbs)
                                 ")")
                         ""))

;; ---------------------------------------------------------------------
(defun nano-modeline-org-agenda-mode-p ()
  (derived-mode-p 'org-agenda-mode))

(defun nano-modeline-org-agenda-mode ()
  (nano-modeline-compose (nano-modeline-status)
                         "Agenda"
                         ""
                         (format-time-string "%H:%M")))

;; ---------------------------------------------------------------------
(defun nano-modeline-term-mode-p ()
  (derived-mode-p 'term-mode))

(defun nano-modeline-term-mode ()
  (nano-modeline-compose " >_ "
                         "Terminal"
                         (concat "(" shell-file-name ")")
                         (shorten-directory default-directory 32)))

;; ---------------------------------------------------------------------
(defun nano-modeline-mu4e-main-mode-p ()
  (derived-mode-p 'mu4e-main-mode))

(defun nano-modeline-mu4e-main-mode ()
  (nano-modeline-compose (nano-modeline-status)
                         "Mail"
                         (nano-modeline-mu4e-context)
                         (format-time-string "%A %d %B %Y, %H:%M")))

;; ---------------------------------------------------------------------
(defun nano-modeline-mu4e-headers-mode-p ()
  (derived-mode-p 'mu4e-headers-mode))

(defun nano-modeline-mu4e-headers-mode ()
  (nano-modeline-compose (nano-modeline-status)
                         (mu4e~quote-for-modeline mu4e~headers-last-query)
                         ""
                         ""))

(with-eval-after-load 'mu4e
  (defun mu4e~header-line-format () (nano-modeline)))

;; ---------------------------------------------------------------------
(setq mu4e-modeline-max-width 72)

(defun nano-modeline-mu4e-view-mode-p ()
  (derived-mode-p 'mu4e-view-mode))

(defun nano-modeline-mu4e-view-mode ()
  (let* ((msg     (mu4e-message-at-point))
         (subject (mu4e-message-field msg :subject))
         (from    (mu4e~headers-contact-str (mu4e-message-field msg :from)))
         (date     (mu4e-message-field msg :date)))
    (nano-modeline-compose (nano-modeline-status)
                           (s-truncate 40 subject "…")
                           ""
                           from)))

(add-hook 'mu4e-view-mode-hook
          (lambda () (setq header-line-format "%-")
                     (face-remap-add-relative 'header-line
                                              '(:background "#ffffff" :height 1.0))))


;; ---------------------------------------------------------------------
(defun nano-modeline-nano-help-mode-p ()
  (derived-mode-p 'nano-help-mode))

(defun nano-modeline-nano-help-mode ()
  (nano-modeline-compose (nano-modeline-status)
                         "N Λ N O"
                         "(help)"
                         ""))

;; ---------------------------------------------------------------------
(defun nano-modeline-message-mode-p ()
  (derived-mode-p 'message-mode))

(defun nano-modeline-message-mode ()
  (nano-modeline-compose (nano-modeline-status)
                         "Message" "(draft)" ""
			 '(( "SEND" . message-send))))


;; ---------------------------------------------------------------------
(setq org-mode-line-string nil)
(with-eval-after-load 'org-clock
  (add-hook 'org-clock-out-hook
            '(lambda () (setq org-mode-line-string nil)
                        (force-mode-line-update))))

(defun nano-modeline-org-clock-mode-p ()
  org-mode-line-string)

(defun nano-modeline-org-clock-mode ()
    (let ((buffer-name (format-mode-line "%b"))
          (mode-name   (format-mode-line "%m"))
          (branch      (vc-branch))
          (position    (format-mode-line "%l:%c")))
      (nano-modeline-compose (nano-modeline-status)
                             buffer-name 
                             (concat "(" mode-name
                                     (if branch (concat ", "
                                             (propertize branch 'face 'italic)))
                                     ")" )
                             org-mode-line-string)))

;; ---------------------------------------------------------------------
(defun nano-modeline-docview-mode-p ()
  (derived-mode-p 'doc-view-mode))

(defun nano-modeline-docview-mode ()
  (let ((buffer-name (format-mode-line "%b"))
	(mode-name   (format-mode-line "%m"))
	(branch      (vc-branch))
	(page-number (concat
		      (number-to-string (doc-view-current-page)) "/"
		      (or (ignore-errors
			    (number-to-string (doc-view-last-page-number)))
			  "???"))))
    (nano-modeline-compose
     (nano-modeline-status)
     buffer-name
     (concat "(" mode-name
	     (if branch (concat ", "
				(propertize branch 'face 'italic)))
	     ")" )
     page-number)))

;; ---------------------------------------------------------------------
(defun nano-modeline-pdf-view-mode-p ()
  (derived-mode-p 'pdf-view-mode))

(defun nano-modeline-pdf-view-mode ()
  (let ((buffer-name (format-mode-line "%b"))
	(mode-name   (format-mode-line "%m"))
	(branch      (vc-branch))
	(page-number (concat
		      (number-to-string (pdf-view-current-page)) "/"
		      (or (ignore-errors
			    (number-to-string (pdf-cache-number-of-pages)))
			  "???"))))
    (nano-modeline-compose
     "RW"
     buffer-name
     (concat "(" mode-name
	     (if branch (concat ", "
				(propertize branch 'face 'italic)))
	     ")" )
     page-number)))

;; ---------------------------------------------------------------------
(defun buffer-menu-mode-header-line ()
  (face-remap-add-relative
   'header-line `(:background ,(face-background 'nano-face-subtle))))
(add-hook 'Buffer-menu-mode-hook
          #'buffer-menu-mode-header-line)

;; ---------------------------------------------------------------------
(defun nano-modeline-completion-list-mode-p ()
  (derived-mode-p 'completion-list-mode))

(defun nano-modeline-completion-list-mode ()
    (let ((buffer-name (format-mode-line "%b"))
          (mode-name   (format-mode-line "%m"))
          (position    (format-mode-line "%l:%c")))
      (nano-modeline-compose (nano-modeline-status)
                             buffer-name "" position)))

;; ---------------------------------------------------------------------

(defun nano-modeline-default-mode ()
    (let ((buffer-name (format-mode-line "%b"))
          (mode-name   (format-mode-line "%m"))
          (branch      (vc-branch))
          (position    (format-mode-line "%l:%c")))
      (nano-modeline-compose (nano-modeline-status)
                             buffer-name
                             (concat "(" mode-name
                                     (if branch (concat ", "
                                            (propertize branch 'face 'italic)))
                                     ")" )
                             position)))

;; ---------------------------------------------------------------------
(defun nano-modeline-status ()
  "Return buffer status: read-only (RO), modified (**) or read-write (RW)"
  
  (let ((read-only   buffer-read-only)
        (modified    (and buffer-file-name (buffer-modified-p))))
    (cond (modified  "**") (read-only "RO") (t "RW"))))
  
;; ---------------------------------------------------------------------
(defun nano-modeline-mu4e-context ()
  "Return the current mu4e context as a non propertized string."

  (if (> (length (mu4e-context-label)) 0)
      (concat "(" (substring-no-properties (mu4e-context-label) 1 -1) ")")
    "(none)"))


;; ---------------------------------------------------------------------
(defun nano-modeline ()
  "Install a header line whose content is dependend on the major mode"
  (interactive)
  (setq-default header-line-format
  '((:eval
     (cond ((nano-modeline-elfeed-search-mode-p)   (nano-modeline-elfeed-search-mode))
           ((nano-modeline-elfeed-show-mode-p)     (nano-modeline-elfeed-show-mode))
           ((nano-modeline-info-mode-p)            (nano-modeline-info-mode))
           ((nano-modeline-calendar-mode-p)        (nano-modeline-calendar-mode))
           ((nano-modeline-org-capture-mode-p)     (nano-modeline-org-capture-mode))
           ((nano-modeline-org-agenda-mode-p)      (nano-modeline-org-agenda-mode))
           ((nano-modeline-org-clock-mode-p)       (nano-modeline-org-clock-mode))
           ((nano-modeline-term-mode-p)            (nano-modeline-term-mode))
           ((nano-modeline-mu4e-dashboard-mode-p)  (nano-modeline-mu4e-dashboard-mode))
           ((nano-modeline-mu4e-main-mode-p)       (nano-modeline-mu4e-main-mode))
           ((nano-modeline-mu4e-headers-mode-p)    (nano-modeline-mu4e-headers-mode))
;;         ((nano-modeline-mu4e-view-mode-p)       (nano-modeline-mu4e-view-mode))
           ((nano-modeline-pdf-view-mode-p)        (nano-modeline-pdf-view-mode))
	   ((nano-modeline-docview-mode-p)         (nano-modeline-docview-mode))
	   ((nano-modeline-completion-list-mode-p) (nano-modeline-completion-list-mode))
	   ((nano-modeline-message-mode-p)         (nano-modeline-message-mode))
           ((nano-modeline-nano-help-mode-p)       (nano-modeline-nano-help-mode))
           (t                                      (nano-modeline-default-mode)))))))

;; ---------------------------------------------------------------------
(defun nano-modeline-update-windows ()
  "Modify the mode line depending on the presence of a window below."
  
  (dolist (window (window-list))
    (with-selected-window window
      (if (or (one-window-p t)
	      (eq (window-in-direction 'below) (minibuffer-window))
	      (not (window-in-direction 'below)))
	      (with-current-buffer (window-buffer window)
	        (setq mode-line-format "%-"))
	    (with-current-buffer (window-buffer window)
 	      (setq mode-line-format nil)))
;;      (if (window-in-direction 'above)
;;	      (face-remap-add-relative 'header-line '(:overline "#777777"))
;;	    (face-remap-add-relative 'header-line '(:overline nil)))
      )))
(add-hook 'window-configuration-change-hook 'nano-modeline-update-windows)

(setq eshell-status-in-modeline nil)
(setq-default mode-line-format "%-")
(nano-modeline)

(provide 'nano-modeline)
