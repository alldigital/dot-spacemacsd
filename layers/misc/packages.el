;; packages.el --- misc layer packages file for Spacemacs.

(defconst misc-packages
  '(
    nov
    evil-collection
    )
  "The list of Lisp packages required by the ed-misc layer." )

(defun misc/init-nov ()
  (use-package nov))

(setq evil-want-keybinding nil)

(defun misc/init-evil-collection ()
  (use-package evil-collection
    :after evil
    :ensure t
    :config
    (evil-collection-init)))

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(setq-default evil-escape-key-sequence "jk")

;; Change eshell completion framework from company to helm
(defun setup-eshell-helm-completion ()
  (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete))

(add-hook 'eshell-mode-hook #'setup-eshell-helm-completion)
(when (eq system-type 'gnu/linux)
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "firefox")
  )

;; SPC h d k workaround, causes error otherwise
;; (require 'ansible-doc)

;; backups-mode
;; (add-to-list 'load-path "~/.emacs.d/private/local/backups-mode")
;; (require 'backups-mode)
;; (defvar backup-directory "~/.emacs-backups/backups/")
;; (defvar tramp-backup-directory "~/.emacs-backups/tramp-backups/")
;; keep all versions forever
;; (setq delete-old-versions 1)
;; (backups-minor-mode)

;; Copy text selected with the mouse to kill rin/home/ed/.spacemacs.d/layers/misc/packages.elg and clipboard
(setq mouse-drag-copy-region t)

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
(autoload 'smalltalk-mode "~/.emacs.d/private/local/smalltalk/smalltalk-mode.el")
(setq auto-mode-alist
      (append  '(("\\.st\\'" . smalltalk-mode))
               auto-mode-alist))


;; Global keybindings

(evil-leader/set-key
  "fx" '(lambda() (interactive)
          (switch-to-buffer "*scratch*")
          (emacs-lisp-mode))
  )

(evil-leader/set-key
  "oa" 'org-agenda
  "od" 'ed/org-agenda-day
  "oc" 'org-capture
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

;; (custom-set-faces
;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
;; '(default ((t (:background nil))))
;;  '(helm-ff-directory ((t (:background "#263238" :foreground "#81d4fa"))))
;;  '(helm-ff-file ((t (:background "#263238" :foreground "#ffffff"))))

;;  '(slime-repl-inputed-output-face ((t (:foreground "Green")))))

;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
;; (setq inferior-lisp-program "/usr/bin/sbcl")

(with-eval-after-load 'org

  ;; When capturing go into insert mode

  (add-hook 'org-capture-mode-hook 'evil-insert-mode)

  ;; Capture templates

  (setq org-capture-templates
        (quote (
                ("a" "Appointment" entry (file+headline "gcal.org" "Appointments")
                 "* TODO  %?\nAppointment created: %U\nDeadline: %^T\n%a\n" :prepend t)
                ("l" "Link" entry (file+headline "links.org" "Unsorted Links")
                 "* %? %^L %^g ;; \n%T" :prepend t) ;
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
                ("j" "Journal" entry (file+olp+datetree "~/org/journal.org" "Journal")
                 "* %<%Y-%m-%d %H:%M>  %?")
                )))

  ;; Org mode alternative bullets
  (setq org-bullets-bullet-list '("■" "◆" "▲" "▶"))
  ;; All files in this directory should activate org-mode
  (add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
  (add-to-list 'auto-mode-alist `(,(expand-file-name "~/.notes/") . org-mode))

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

(defun ed/org-agenda-day ()
  (interactive)
  (org-agenda-list nil nil 'day nil)
  )

(defun nolinum ()
  (interactive)
  (message "Deactivated linum mode")
  (global-linum-mode 0)
  (linum-mode 0)
  )

(add-hook 'org-mode-hook 'nolinum)


(defun org-set-line-checkbox (arg)
  (interactive "P")
  (let ((n (or arg 1)))
    (when (region-active-p)
      (setq n (count-lines (region-beginning)
                           (region-end)))
      (goto-char (region-beginning)))
    (dotimes (i n)
      (beginning-of-line)
      (insert "- [ ] ")
      (forward-line))
    (beginning-of-line)))

)

;;; Revert buffer to the file contents on disk
(global-set-key (kbd "C-c r") (lambda ()
                                (interactive)
                                (revert-buffer t t t)
                                (message "buffer is reverted")))

;;; packages.el ends here
