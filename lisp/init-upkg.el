;;; init-upkg.el --- Init use-package.
;;; Commentary:
;;; Code:


;; @purcell --- Install into separate package dirs for each Emacs version
(let ((versioned-package-dir
    (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                        user-emacs-directory)))
(setq package-user-dir versioned-package-dir))


(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (setq package-archives
    '(("gnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))))


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


(eval-when-compile
  (require 'use-package))


(use-package diminish
  :ensure t)


(use-package delight
  :ensure t)




(provide 'init-upkg)
;;; init-upkg.el ends here
