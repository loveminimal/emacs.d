;;; init-site.el --- Exports org to site. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package htmlize
  :ensure t
  ;; :config
  ;; (setq htmlize-output-type 'font)
  )

(progn
  "Settings of `org-export'."
  (setq org-export-in-background t
        ;; Hide html built-in style and script.
        org-html-htmlize-output-type 'inline-css
        org-html-head-include-default-style nil
        org-html-head-include-scripts nil
        ))

(eval-after-load 'ox-html
  (setq user-full-name "Jack Liu"))

(use-package ox-publish
  :config
  (setq org-publish-project-alist
        '(("orgfiles"
           ;; ; Sources and destinations for files.
           :base-directory "~/site/org/" ;; local dir
           :publishing-directory "~/site/public/" ;; :publishing-directory "/ssh:jack@192.112.245.112:~/site/public/"
           ;; :preparation-function
           ;; :complete-function
           :base-extension "org"
           ;; :exclude "PrivatePage.org"     ;; regexp
           ;; :include
           :recursive t

           ;; ; Publishing action
           :publishing-function org-html-publish-to-html

           ;; ; Generic properties
           :headline-levels 6    ;; org-export-headline-levels
           :language "zh"        ;; org-export-default-language
           :section-numbers nil  ;; org-export-with-section-numbers
           :with-planning t      ;; org-export-with-planning
           :with-priority t      ;; org-export-with-priority ;
           ;;  :with-tags not-in-toc ;; org-export-with-tags
           :with-toc t           ;; org-export-with-toc

           :html-doctype "html5" ;; org-html-doctype
           ;;  :html-metadata-timestamp-format "%Y-%m-%d" ;; org-html-metadata-timestamp-format
           :html-head-include-default-style nil ;; org-html-head-include-default-style
           :html-head-include-scripts nil ;; org-html-head-include-scripts
           :html-head
           "<link rel=\"shortcut icon\" href=\"/themes/assets/rose-red.png\" type=\"image/x-icon\" />
           <link rel=\"stylesheet\" href=\"/themes/style.css\" type=\"text/css\"  />
           <script type=\"module\" src=\"/themes/main.js\" defer></script>" ;; org-html-head
           :html-checkbox-type unicode  ;; org-html-checkbox-type
           :html-indent t               ;; org-html-indent
           ;; :html-link-home "index.html"	;; org-html-link-home
           ;; :html-link-up "uUP"          ;; org-html-link-up
           :html-validation-link "<a href=\"http://beian.miit.gov.cn/\">豫ICP备19025929号</a>"	;; org-html-validation-link
           )

          ;; static assets
          ("confs"
           :base-directory "~/site/"
           :base-extension "js"
           :publishing-directory "~/site/public/"
           :recursive nil
           :publishing-function org-publish-attachment
           )
          ("images"
           :base-directory "~/site/images/"
           :base-extension any
           :publishing-directory "~/site/public/images/"
           :recursive t
           :publishing-function org-publish-attachment
           )
          ("themes"
           :base-directory "~/site/themes/"
           :base-extension any
           :publishing-directory "~/site/public/themes/"
           :recursive t
           :publishing-function org-publish-attachment
           )

          ("website" :components ("orgfiles" "confs" "images" "themes"))
          ("statics" :components ("confs" "images" "themes"))
          )))

(defun save-and-publish-website()
  "Save all buffers and publish."
  (interactive)
  (when (yes-or-no-p "Really save and publish current project?")
    (save-some-buffers t)
    (org-publish-project "website" t)
    (message "Site published done.")))

(defun save-and-publish-statics ()
  "Just copy statics like js, css, and image file .etc."
  (interactive)
  (org-publish-project "statics" t)
  (message "Copy statics done."))

(defun save-and-publish-file ()
  "Save current buffer and publish."
  (interactive)
  (save-buffer t)
  (org-publish-current-file t))

(defun delete-org-and-html ()
  "Delete current org and the relative html when it exists."
  (interactive)
  (when (yes-or-no-p "Really delete current org and the relative html?")

    (let ((fileurl (concat "~/site/public/" (file-name-base (buffer-name)) ".html")))
      (if (file-exists-p fileurl)
          (delete-file fileurl))
      (delete-file (buffer-file-name))
      (kill-this-buffer)
      (message "Delete org and the relative html done."))))

(defun just-delete-relative-html ()
  "Just delete the relative html when it exists."
  (interactive)
  (when (yes-or-no-p "Really delete the relative html?")

    (let ((fileurl (concat "~/site/public/" (file-name-base (buffer-name)) ".html")))
      (if (file-exists-p fileurl)
          (progn
            (delete-file fileurl)
            (message "Delete the relative html done.")
            )
        (message "None relative html.")))))

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
    (unless (httpd-running-p) (httpd-start))
    (browse-url fileurl)))

(provide 'init-site)
;;; init-site.el ends here
