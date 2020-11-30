;;; init-abbr.el --- Abbreviations.
;;; Commentary:
;;; Code:

(use-package abbrev
  :config
  (clear-abbrev-table global-abbrev-table)
  (define-abbrev-table 'global-abbrev-table
    '(
      ("sout" "System.out.println")
      ("jm" "public static void main(String[] args) ")

      ("frm" "float: right; margin-left:")
      ("flm" "float: left; margin-right:")

      ("emr" "✔")
      ("emw" "✘")

      ("usp" "use-package")
      ("jss" "JavaScript")
      ("ccc" "#+CAPTION:")
      ("uuu" "↑")
      ("rrr" "→")
      ("ddd" "↓")
      ("lll" "←")
      ("uud" "↑↓")
      ("ddu" "↓↑")
      ("bgqi" "☰")
      ("bgku" "☷")
      ("bgli" "☲")
      ("bgka" "☵")
      ("bgdu" "☱")
      ("bgxu" "☴")
      ("bgzh" "☳")
      ("bgge" "☶")
    ))
  (setq-default abbrev-mode t)
  (setq save-abbrevs nil)
  :diminish)

(provide 'init-abbr)
;;; init-abbr.el ends here
