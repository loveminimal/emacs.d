;;; init-abbr.el --- Abbreviations. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package abbrev
  :config
  (clear-abbrev-table global-abbrev-table)
  (define-abbrev-table 'global-abbrev-table
    '(
      ("frm" "float: right; margin-left: 8px;")
      ("flm" "float: left; margin-right: 8px;")

      ("emr" "✔")
      ("emw" "✘")

      ("usp" "use-package")
      ("jss" "JavaScript")
      ("ccc" "#+CAPTION:")

      ("uuu" "↑")
      ("rrr" "→")
      ("ddd" "↓")
      ("lll" "←")
    ))
  (setq-default abbrev-mode t)
  (setq save-abbrevs nil)
  :diminish)

(provide 'init-abbr)
;;; init-abbr.el ends here
