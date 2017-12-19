;; packages.el --- ed-misc layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: ED <butshesagirl@rousette.org.uk>
;; URL: https://github.com/ed
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `ed-misc-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `ed-misc/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `ed-misc/pre-init-PACKAGE' and/or
;;   `ed-misc/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst ed-misc-packages
  '()
  "The list of Lisp packages required by the ed-misc layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

;; Some color related settings

;; material colors for pdf-view night mode
(setq pdf-view-midnight-colors '("#FFFFFF" . "#263238" ))

;; SPC h d k workaround, causes error otherwise
;; (require 'ansible-doc)

(require 'org-protocol)
;; (require 'org-protocol-capture-html)

;; (require 'darkroom)

;; backups-mode
(add-to-list 'load-path "~/.spacemacs.d/local-packages/backups-mode")
(require 'backups-mode)
;; (defvar backup-directory "~/.emacs-backups/backups/")
;; (defvar tramp-backup-directory "~/.emacs-backups/tramp-backups/")
;; keep all versions forever
(setq delete-old-versions 1)
(backups-minor-mode)

  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  ;; Add line numbers
  ;;(global-linum-mode)
  ;;(with-eval-after-load 'linum
  ;; (linum-relative-toggle))

  ;; Copy text selected with the mouse to kill ring and clipboard
  (setq mouse-drag-copy-region t)

  ;; Org mode alternative bullets
  (setq org-bullets-bullet-list '("■" "◆" "▲" "▶"))
  ;; All files in this directory should activate org-mode
  (add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
  (add-to-list 'auto-mode-alist `(,(expand-file-name "~/.notes/") . org-mode))

  ;; Default browser
  ;; (setq browse-url-browser-function 'browse-url-generic
  ;;       browse-url-generic-program "google-chrome-stable")

  ;; From Sacha Chua's config http://pages.sachachua.com/.emacs.d/Sacha.html
  ;; Backup settings
  ;; (setq backup-directory-alist '(("." . "~/.emacs.d/private/backups")))
  ;; (setq delete-old-versions -1)
  ;; (setq version-control t)
  ;; (setq vc-make-backup-files t)

  (setq auto-save-file-name-transforms '((".*" "~/.emacs.local.d/auto-save-list/" t)))

  ;; History settings
  (setq savehist-file "~/.emacs.local.d/savehist")
  (savehist-mode 1)
  (setq history-length t)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history 1)
  (setq savehist-additional-variables
        '(kill-ring
          search-ring
          regexp-search-ring))

  ;; GNU Smalltalk mode
  ;; (load-file "/usr/share/emacs/site-lisp/site-start.d/smalltalk-mode-init.el")
  (setq auto-mode-alist
        (append  '(("\\.st\\'" . smalltalk-mode))
                 auto-mode-alist))

  (autoload 'smalltalk-mode "~/.emacs.d/private/local/smalltalk/smalltalk-mode.el")

  ;; Global keybindings

  (evil-leader/set-key
    "fx" '(lambda() (interactive)(switch-to-buffer "*scratch*"))
    )

  (evil-leader/set-key
    "oa" 'org-agenda
    "od" 'ed/org-agenda-day
    "oc" 'org-capture
    )

  (defun ed/org-agenda-day ()
    (interactive)
    (org-agenda-list nil nil 'day nil)
    )

  ;; replaces URL with Org-mode link including description

  (defun my-www-get-page-title (url)
    "retrieve title of web page. from: http://www.opensubscriber.com/message/help-gnu-emacs@gnu.org/14332449.html"
    (let ((title))
      (with-current-buffer (url-retrieve-synchronously url)
        (goto-char (point-min))
        (re-search-forward "<title>\\([^<]*\\)</title>" nil t 1)
        (setq title (match-string 1))
        (goto-char (point-min))
        (re-search-forward "charset=\\([-0-9a-zA-Z]*\\)" nil t 1)
        (decode-coding-string title (intern (match-string 1)))))
    )


  (defun my-url-linkify ()
    "Make URL at cursor point into an Org-mode link.
If there's a text selection, use the text selection as input.

Example: http://example.com/xyz.htm
becomes
\[\[http://example.com/xyz.htm\]\[Source example.com\]\]

Adapted code from: http://ergoemacs.org/emacs/elisp_html-linkify.html"
    (interactive)
    (let (resultLinkStr bds p1 p2 domainName)
      ;; get the boundary of URL or text selection
      (if (region-active-p)
	  (setq bds (cons (region-beginning) (region-end)) )
	(setq bds (bounds-of-thing-at-point 'url))
	)
      ;; set URL
      (setq p1 (car bds))
      (setq p2 (cdr bds))
      (let (
	    (url (buffer-substring-no-properties p1 p2))
	    )
	;; retrieve title
	(let ((title (my-www-get-page-title url)))
	  (message (concat "title is: " title))
	  ;;(setq url (replace-regexp-in-string "&" "&amp;" url))
	  (let ((resultLinkStr (concat "[[" url "][" title "]]")))
	    ;; delete url and insert the link
	    (delete-region p1 p2)
	    (insert resultLinkStr)
	    )
	  ))))

  ;; Org Capture
  (defun ed/configure-org-capture ()

    ;; Capture support functions

    ;; Capture templates

    (setq org-capture-templates
          (quote (
                  ("a" "Appointment" entry (file+headline "gcal.org" "Appointments")
                   "* TODO  %?\nAppointment created: %U\nDeadline: %^T\n%a\n" :prepend t)
                  ("l" "Link" entry (file+headline "links.org" "Unsorted Links")
                  "* %? %^L %^g \n%T" :prepend t)
                  ("n" "Note" entry (file+headline org-default-notes-file "Unsorted Notes")
                   "*  %? :NOTE:\n%U\n%a\n" :prepend t)
                  ("N" "Note with clipboard contents" entry (file+headline org-default-notes-file "Unsorted Notes")
                   "*  %? :NOTE:\n%U\n#+BEGIN_QUOTE\n%x\n#+END_QUOTE\n" :prepend t)
                  ("t" "Todo" entry (file+headline org-default-notes-file "Unsorted Todo")
                   "* TODO %?\n%U\n%a\n" :prepend t)
                  ("m" "Movies to see" entry (file+headline "movies.org" "To Download")
                   "* ToDownload %? \n  :PROPERTIES:\n  :DATE: %t\n  :URL: %c\n  :END:")
                  ("i" "idea" entry (file org-default-notes-file)
                   "* %? :IDEA:\n%U\n%a\n")
                  )))
  )

  ;; Clojure additional settings

  (with-eval-after-load 'clojure-mode
    (put-clojure-indent 'cond #'indent-cond)
    (set-face-italic 'clojure-keyword-face t))

  (add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
  (add-to-list 'magic-mode-alist '(".* boot" . clojure-mode))

  ;; cider customizations
  (setq cider-eldoc-display-context-dependent-info t)
  (setq cider-eval-result-prefix ";;=> ")
  (setq cider-repl-display-help-banner nil)
  (setq cider-repl-history-file "~/.lein/cider-repl-history")
  (setq cider-show-error-buffer nil)

  ;; lisp wrap-around fix
  (spacemacs/set-leader-keys "kw" nil)
  (spacemacs/set-leader-keys "kw(" 'paredit-wrap-round)
  (spacemacs/set-leader-keys "kw[" 'paredit-wrap-square)
  (spacemacs/set-leader-keys "kw{" 'paredit-wrap-curly)
  (spacemacs/set-leader-keys "kw<" 'paredit-wrap-angled)
  (spacemacs/set-leader-keys "kwr" 'sp-rewrap-sexp)

  ;; Paredit keybindings

  (with-eval-after-load 'paredit
    (message "Bindings for paredit")
    (dolist (binding '(("C-<left>" . paredit-backward-slurp-sexp)
                       ("C-<right>" . paredit-backward-barf-sexp)
                       ("C-M-<left>" . paredit-forward-barf-sexp)
                       ("C-M-<right>" . paredit-forward-slurp-sexp)
                       ("M-<up>" . paredit-splice-sexp-killing-backward)
                       ("M-<down>" . paredit-splice-sexp-killing-forward)))
      (define-key paredit-mode-map (kbd (car binding)) (cdr binding))))

  (with-eval-after-load 'smartparens
    (message "Bindings for smartparens")
    (dolist (binding '(("C-<left>" . sp-backward-slurp-sexp)
                     ("C-<right>" . sp-backward-barf-sexp)
                     ("C-M-<left>" . sp-forward-barf-sexp)
                     ("C-," . sp-forward-barf-sexp)
                     ("C-M-<right>" . sp-forward-slurp-sexp)
                     ("C-." . sp-forward-slurp-sexp)
                     ("M-<up>" . sp-splice-sexp-killing-backward)
                     ("M-<down>" . sp-splice-sexp-killing-forward)
                     (";" . (lambda ()
                              (interactive)
                              (if (member major-mode '(clojure-mode
                                                       clojurescript-mode
                                                       emacs-lisp-mode))
                                  (sp-comment)
                                (self-insert-command 1))))))
    (define-key smartparens-mode-map (kbd (car binding)) (cdr binding))))

  ;; Bunch of useful settings from internets
  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)


  (defun ed/configure-org-mode ()
    ;; (require 'org-checklist)
    (ed/configure-org-capture)

    ;;  Org file paths
    (setq ed/home-dir (expand-file-name "~"))
    (setq org-directory (concat ed/home-dir "/org"))
    (setq org-default-notes-file (concat org-directory "/notes.org"))

    ;; agenda

    (setq org-agenda-files (list org-directory))
    (setq org-agenda-skip-scheduled-if-done t)
    (setq org-agenda-skip-deadline-if-done t)

    ;; keybindings
    ;; (spacemacs/set-leader-keys-for-major-mode 'org-mode "z" 'org-add-note)
    ;; (spacemacs/set-leader-keys-for-major-mode 'org-mode "F" 'org-attach)
    ;; (spacemacs/set-leader-keys-for-major-mode 'org-mode "g" 'org-mac-grab-link)

    ;; todos
    (setq org-todo-keywords
      (quote
       ((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "CANCELLED" "DONE"))))

    (setq org-todo-keyword-faces
          (quote
           (("TODO" . "black")
            ("IN-PROGRESS" . "green")
            ("WAITING" . "blue")
            ("DONE" :foreground "white" :weight bold)
            ("CANCELLED" :foreground "purple" :weight bold :strike-through t))))
    )

  ;; (custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  ;; '(default ((t (:background nil))))
  ;;  '(helm-ff-directory ((t (:background "#263238" :foreground "#81d4fa"))))
  ;;  '(helm-ff-file ((t (:background "#263238" :foreground "#ffffff"))))

  ;;  '(slime-repl-inputed-output-face ((t (:foreground "Green")))))

  (ed/configure-org-mode)

  (load-theme 'material)


  ;; Highlight indents
  ;; (setq highlight-indent-guides-method 'character)
  ;; (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  ;; Replace "sbcl" with the path to your implementation
  (setq inferior-lisp-program "/usr/bin/sbcl")

  ;; EXWM stuff

  ;; Whether exwm should be started (until I figure out why exwm layer fails)

  (defvar ed/use-exwm nil)

  (when (and ed/use-exwm window-system)

    (add-to-list 'load-path "~/.emacs.local.d/xelb")
    (add-to-list 'load-path "~/.emacs.local.d/exwm")

    (require 'exwm)
    (require 'exwm-config)
    (exwm-config-default)

    ;; exwm vars

    (defvar exwm--terminal-command "termite"
      "Terminal command to run.")

    (defvar exwm--locking-command "i3lock-fancy"
      "Command to run when locking session")

    (defvar exwm-app-launcher--prompt "$ "
      "Prompt for the EXWM application launcher")

    (defvar exwm--rofi-command "rofi -show window -font \"Input Mono Compressed Bold 10\""
      "Command to start rofi launcher")

    (defvar exwm--hide-tiling-modeline t
      "Whether to hide modeline.")

    (defun spacemacs/exwm-bind-command (key command &rest bindings)
      (while key
        (exwm-input-set-key (kbd key)
                            `(lambda ()
                               (interactive)
                               (start-process-shell-command ,command nil ,command)))
        (setq key     (pop bindings)
              command (pop bindings))))

    (spacemacs/exwm-bind-command "<s-return>"  exwm--terminal-command)

    (spacemacs/exwm-bind-command "s-d"  exwm--rofi-command)
    (spacemacs/exwm-bind-command "s-s"  "synapse")

    (setq exwm-workspace-number 9
          exwm-workspace-show-all-buffers t
          exwm-layout-show-all-buffers t)

    (require 'exwm-systemtray)
    (exwm-systemtray-enable)
    (display-time-mode 1)
    (setq display-time-string-forms '((format-time-string "%H:%M" now)))

    ;; + Application launcher ('M-&' also works if the output buffer does not
    ;;   bother you). Note that there is no need for processes to be created by
    ;;   Emacs.
    (defun spacemacs/exwm-application-launcher (command)
      "Launches an application in your PATH.
Can show completions at point for COMMAND using helm or ido"
      (interactive (list (read-shell-command exwm-app-launcher--prompt)))
      (start-process-shell-command command nil command))

    (exwm-input-set-key (kbd "s-SPC") #'spacemacs/exwm-application-launcher)
    (exwm-input-set-key (kbd "s-p") #'spacemacs/exwm-application-launcher)

    ;; lock screen
    (exwm-input-set-key (kbd "<s-escape>")
                        (lambda () (interactive) (start-process "" nil exwm--locking-command)))

    ;; Workspace helpers

    (defvar exwm-workspace-switch-wrap t
      "Whether `spacemacs/exwm-workspace-next' and `spacemacs/exwm-workspace-prev' should wrap.")

    (defun spacemacs/exwm-workspace-next ()
      "Switch to next exwm-workspaceective (to the right)."
      (interactive)
      (let* ((only-workspace? (equal exwm-workspace-number 1))
             (overflow? (= exwm-workspace-current-index
                           (1- exwm-workspace-number))))
        (cond
         (only-workspace? nil)
         (overflow?
          (when exwm-workspace-switch-wrap
            (exwm-workspace-switch 0)))
         (t (exwm-workspace-switch  (1+ exwm-workspace-current-index))))))
    (defun spacemacs/exwm-workspace-prev ()
      "Switch to next exwm-workspaceective (to the right)."
      (interactive)
      (let* ((only-workspace? (equal exwm-workspace-number 1))
             (overflow? (= exwm-workspace-current-index 0)))
        (cond
         (only-workspace? nil)
         (overflow?
          (when exwm-workspace-switch-wrap
            (exwm-workspace-switch (1- exwm-workspace-number))))
         (t (exwm-workspace-switch  (1- exwm-workspace-current-index))))))

    ;; Quick swtiching between workspaces
    (defvar exwm-toggle-workspace 0
      "Previously selected workspace. Used with `exwm-jump-to-last-exwm'.")
    (defun exwm-jump-to-last-exwm ()
      (interactive)
      (exwm-workspace-switch exwm-toggle-workspace))
    (defadvice exwm-workspace-switch (before save-toggle-workspace activate)
      (setq exwm-toggle-workspace exwm-workspace-current-index))

    ;; Rename buffer to window title
    (defun exwm-rename-buffer-to-title () (exwm-workspace-rename-buffer exwm-title))
    (add-hook 'exwm-update-title-hook 'exwm-rename-buffer-to-title)

    ;; no mode line for floating windows
    (add-hook 'exwm-floating-setup-hook 'exwm-layout-hide-mode-line)
    (add-hook 'exwm-floating-exit-hook 'exwm-layout-show-mode-line)

    ;; per app settings

    ;; (defun exwm-start-in-char-mode ()
    ;;   (when (or (string= exwm-instance-name "emacs")
    ;;             (string= exwm-class-name "Termite")
    ;;             (string= exwm-class-name "URxvt")
    ;;             (string= exwm-class-name "XTerm")
    ;;             (string= exwm-class-name "libreoffice-startcenter"))
    ;;     (exwm-input-release-keyboard (exwm--buffer->id (window-buffer)))))

    ;; (add-hook 'exwm-manage-finish-hook 'exwm-start-in-char-mode)

    ;;make exwm windows default to char instead of line mode

    (add-hook 'exwm-manage-finish-hook
              (lambda () (call-interactively #'exwm-input-release-keyboard)
                (exwm-layout-hide-mode-line)))

                                        ;send all keypresses to emacs in line mode
    (setq exwm-input-line-mode-passthrough t)



    (defun exwm-input-line-mode ()
      "Set exwm window to line-mode and show mode line"
      (call-interactively #'exwm-input-grab-keyboard)
      (exwm-layout-show-mode-line))

    (defun exwm-input-char-mode ()
      "Set exwm window to char-mode and hide mode line"
      (call-interactively #'exwm-input-release-keyboard)
      (exwm-layout-hide-mode-line))

    (defun exwm-input-toggle-mode ()
      "Toggle between line- and char-mode"
      (with-current-buffer (window-buffer)
        (when (eq major-mode 'exwm-mode)
          (if (equal (second (second mode-line-process)) "line")
              (exwm-input-char-mode)
            (exwm-input-line-mode)))))

    (exwm-input-set-key (kbd "s-i")
                        (lambda () (interactive)
                          (exwm-input-toggle-mode)))

    (exwm-input-set-key (kbd "s-o")
                        (lambda ()
                          (interactive)
                          (exwm-input-toggle-mode)
                          (org-capture)))
    ;; Quick keys

    ;; `exwm-input-set-key' allows you to set a global key binding (available in
    ;; any case). Following are a few examples.
    ;; + We always need a way to go back to line-mode from char-mode
    (exwm-input-set-key (kbd "s-r") 'exwm-reset)

    (exwm-input-set-key (kbd "s-f") #'exwm-layout-toggle-fullscreen)
    (exwm-input-set-key (kbd "<s-tab>") #'exwm-jump-to-last-exwm)
    ;; + Bind a key to switch workspace interactively
    (exwm-input-set-key (kbd "s-w") 'exwm-workspace-switch)

    ;; Preserve the habit
    (exwm-input-set-key (kbd "s-:") 'helm-M-x)
    (exwm-input-set-key (kbd "s-;") 'evil-ex)
    ;; Shell (not a real one for the moment)
    (exwm-input-set-key (kbd "C-'") #'spacemacs/default-pop-shell)
    ;; Undo window configurations
    (exwm-input-set-key (kbd "s-u") #'winner-undo)
    (exwm-input-set-key (kbd "S-s-U") #'winner-redo)
    ;; Change buffers
    (exwm-input-set-key (kbd "s-b") #'helm-mini)
    ;; Focusing windows
    (exwm-input-set-key (kbd "s-h") #'evil-window-left)
    (exwm-input-set-key (kbd "s-j") #'evil-window-down)
    (exwm-input-set-key (kbd "s-k") #'evil-window-up)
    (exwm-input-set-key (kbd "s-l") #'evil-window-right)
    ;; Moving Windows
    (exwm-input-set-key (kbd "s-H") #'evil-window-move-far-left)
    (exwm-input-set-key (kbd "s-J") #'evil-window-move-very-bottom)
    (exwm-input-set-key (kbd "s-K") #'evil-window-move-very-top)
    (exwm-input-set-key (kbd "s-L") #'evil-window-move-far-right)
    ;; Resize
    (exwm-input-set-key (kbd "M-s-h") #'spacemacs/shrink-window-horizontally)
    (exwm-input-set-key (kbd "M-s-j") #'spacemacs/shrink-window)
    (exwm-input-set-key (kbd "M-s-k") #'spacemacs/enlarge-window)
    (exwm-input-set-key (kbd "M-s-l") #'spacemacs/enlarge-window-horizontally)

    ;; (exwm-enable)
    ;; )

    )


;; Add create dir to ranger
;; (define-key ranger-normal-mode-map (kbd "+") #'dired-create-directory)
;; Reading a PDF while taking notes in another window (2 window setup)
;; Hit M-[/M-] to go up/down while keeping cursor in current window.
;; http://www.idryman.org/blog/2013/05/20/emacs-and-pdf/
(fset 'doc-prev "\C-xo\C-u\C-xo")
(fset 'doc-next "\C-xo\C-d\C-xo")
(global-set-key (kbd "M-[") 'doc-prev)
(global-set-key (kbd "M-]") 'doc-next)

;;; packages.el ends here
