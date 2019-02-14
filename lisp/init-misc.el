;;; init-misc.el --- Transfer Station.
;;; Commentary:
;;; Code:


(require-package 'diminish)
(diminish 'eldoc-mode)
(diminish 'undo-tree-mode)
(diminish 'which-key-mode)
(diminish 'abbrev-mode "AV")


;; @Mickey Petersen
(require-package 'iedit)

(require 'iedit)
(defun iedit-dwim (arg)
  "Starts iedit but uses \\[narrow-to-defun] to limit its scope."
  (interactive "P")
  (if arg
      (iedit-mode)
    (save-excursion
      (save-restriction
        (widen)
        ;; this function determines the scope of `iedit-start'.
        (if iedit-mode
            (iedit-done)
          ;; `current-word' can of course be replaced by other
          ;; functions.
          (narrow-to-defun)
          (iedit-start (current-word) (point-min) (point-max)))))))
(global-set-key (kbd "C-;") 'iedit-dwim)


(require-package 'magit)


(provide 'init-misc)
;;; init-misc.el ends here
