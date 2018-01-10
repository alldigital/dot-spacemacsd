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

(setq ed/original-height 105)

(defun ed/setup-fonts ()
  ;; default font and variable-pitch fonts
  (set-face-attribute 'default nil
                      :family "Hack")
  (dolist (face '(mode-line mode-line-inactive minibuffer-prompt))
    (set-face-attribute face nil :family "Hack"))
  (set-face-attribute 'variable-pitch nil
                      :family "Fira Sans")
  (set-face-attribute 'fixed-pitch nil :family "Hack")
  ;; font for all unicode characters
  ;; (set-fontset-font t 'unicode "DejaVu Sans Mono" nil 'prepend)
  )

(add-hook 'after-init-hook #'ed/setup-fonts)

(setq ed/variable-org-enabled t)

(when ed/variable-org-enabled
  (add-hook 'org-mode-hook 'variable-pitch-mode)
  (add-hook 'markdown-mode-hook 'variable-pitch-mode)

  (defun ed/adjoin-to-list-or-symbol (element list-or-symbol)
    (let ((list (if (not (listp list-or-symbol))
                    (list list-or-symbol)
                  list-or-symbol)))
      (require 'cl-lib)
      (cl-adjoin element list)))

  ;; Fontify certain org things with fixed-width
  (eval-after-load "org"
    '(mapc
      (lambda (face)
        (set-face-attribute
         face nil
         :inherit
         (ed/adjoin-to-list-or-symbol
          'fixed-pitch
          (face-attribute face :inherit))))
      (list 'org-code 'org-block 'org-table
            'org-verbatim 'org-formula 'org-macro)))

  ;; Fontify certain markdown things with fixed-width
  (eval-after-load "markdown-mode"
    '(mapc
      (lambda (face)
        (set-face-attribute
         face nil
         :inherit
         (ed/adjoin-to-list-or-symbol
          'fixed-pitch
          (face-attribute face :inherit))))
      (list 'markdown-pre-face 'markdown-inline-code-face))))

(setq custom-safe-themes t)
(load-theme 'material)

;;; packages.el ends here
