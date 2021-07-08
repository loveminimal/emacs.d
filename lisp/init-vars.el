;;; init-vars.el --- Some predefined variables. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defvar *is-mac* (eq system-type 'darwin) "Current system is mac.")
(defvar *is-win* (eq system-type 'windows-nt) "Current system is windows.")
(defvar *is-nux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)) "Current system is gnu/linux.")

(defvar custom-blue "#718591" "Set cursor color.")

(provide 'init-vars)
;;; init-vars.el ends here
