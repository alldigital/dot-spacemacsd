;; packages.el --- display layer packages file for Spacemacs.
;;; Code:

(defconst display-packages
  '(
    material-theme
    darkroom
    highlight-indent-guides
    fontawesome
    )
  "The list of Lisp packages required by the display layer.")

(defun display/init-material-theme ()
  (use-package material-theme))

(defun display/init-darkroom ()
  (use-package darkroom))

(defun display/init-highlight-indent-guides ()
  (use-package highlight-indent-guides
    :config
    (setq highlight-indent-guides-method 'character)
    (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)))

(defun display/init-fontawesome ()
  (use-package fontawesome))

;; Misc display related settings

;; material colors for pdf-view night mode
(setq pdf-view-midnight-colors '("#FFFFFF" . "#263238" ))

(setq custom-safe-themes t)
(load-theme 'material)

;;; packages.el ends here
