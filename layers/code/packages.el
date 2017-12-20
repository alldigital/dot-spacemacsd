;; packages.el --- code layer packages file for Spacemacs.
;;; Code:

(defconst code-packages
  '(
    evil-smartparens
    )
  "The list of Lisp packages required by the code layer.")

(defun display/init-evil-smartparens ()
  (use-package evil-smartparens))

;;; packages.el ends here
