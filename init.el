;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of aTry calling it in the REPLdditional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     auto-completion
     better-defaults
     helm
     spell-checking
     syntax-checking
     ;; Programming languages
     c-c++
     (clojure :variables clojure-enable-fancify-symbols t)
     (colors variables: colors-enable-nyan-cat-progress-bar t)
     common-lisp
     elm
     emacs-lisp
     gtags
     haskell
     java
     javascript
     lua
     php
     python
     scheme
     racket
     (ruby :variables ruby-version-manager 'rvm)
     ruby-on-rails
     rust
     (scala :variables scala-enable-eldoc t scala-auto-insert-asterisk-in-comments t)
     vimscript
     ;; Version control
     git
     github
     version-control
     ;; Markup
     ansible
     command-log
     csv
     docker
     html
     markdown
     nginx
     pdf-tools
     yaml
     (org :variables org-enable-github-support t)
     (spacemacs-layouts :variables layouts-enable-autosave t layouts-autosave-delay 300)
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     (ranger :variables ranger-show-preview t
             ranger-show-hidden t
             ranger-cleanup-eagerly t
             ranger-ignored-extensions '("mkv" "avi" "iso" "mp3" "mp4"))
     (elfeed :variables
             rmh-elfeed-org-files (list "~/rss/rssfeeds.org"))
     ;; Utility layers
     floobits
     ibuffer
     (spacemacs-purpose)
     (treemacs :variables treemacs-use-follow-mode t
               treemacs-use-filewatch-mode t
               treemacs-use-collapsed-directories 3)
     ;; (exwm :variables
     ;;       exwm-app-launcher--prompt "$ "
     ;;       exwm--locking-command "i3lock-fancy"
     ;;       exwm--hide-tiling-modeline nil
     ;;       exwm--terminal-command "termite")

     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages
   '(
     ;; (org-protocol-capture-html :local (recipe :fetcher github :repo "alphapapa/org-protocol-capture-html"))
     ;; (sunrise-commander :location (recipe :fetcher github :repo "escherdragon/sunrise-commander"))
     ;; solarized-theme
     clojure-snippets
     color-theme-solarized
     darkroom
     dockerfile-mode
     fontawesome
     highlight-indent-guides
     langtool
     magithub
     material-theme
     plan9-theme
     symon
     )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be ins talled and loaded.
   dotspacemacs-excluded-packages '()
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update t
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         material
                         solarized
                         solarized-light
                         spacemacs-dark
                         spacemacs-light
                         )
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Input Mono Compressed"
                               :size 14
                               :weight normal
                               :width normal
                               :style Medium
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers '(:relative t
                               :size-limit-kb 1024)
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis t
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  (setq-default evil-escape-key-sequence "jk")

  (when (eq system-type 'gnu/linux)
    ;; tweak for using brew emacs (not emacs-mac which doesn't support running
    ;; in a terminal)
    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program "google-chrome")
    )
  ;; Avoid the dreaded $PATH env var warning durint initialization
  (setq exec-path-from-shell-check-startup-files nil)

)

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  ;; Some color related settings

  ;; material colors for pdf-view night mode
  (setq pdf-view-midnight-colors '("#FFFFFF" . "#263238" ))

  ;; SPC h d k workaround, causes error otherwise
  (require 'ansible-doc)

  (require 'org-protocol)
  ;; (require 'org-protocol-capture-html)

  (require 'darkroom)

  ;; Add create dir to ranger
  ;; (define-key ranger-normal-mode-map (kbd "+") #'dired-create-directory)

  ;; backups-mode
  (add-to-list 'load-path "~/.emacs.d/private/local/backups-mode")
  (require 'backups-mode)
  ;; (defvar backup-directory "~/.emacs-backups/backups/")
  ;; (defvar tramp-backup-directory "~/.emacs-backups/tramp-backups/")
  ;; keep all versions forever
  (setq delete-old-versions 1)
  (backups-minor-mode)


  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  ;; Add line numbers
  (global-linum-mode)
  (with-eval-after-load 'linum
  (linum-relative-toggle))

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
    (require 'org-checklist)
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
    (spacemacs/set-leader-keys-for-major-mode 'org-mode "z" 'org-add-note)
    (spacemacs/set-leader-keys-for-major-mode 'org-mode "F" 'org-attach)
    (spacemacs/set-leader-keys-for-major-mode 'org-mode "g" 'org-mac-grab-link)

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
  (setq highlight-indent-guides-method 'character)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

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

  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#ffffff" "#f36c60" "#8bc34a" "#fff59d" "#4dd0e1" "#b39ddb" "#81d4fa" "#262626"))
 '(custom-safe-themes
   (quote
    ("a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" default)))
 '(fci-rule-color "#3a3a3a" t)
 '(hl-sexp-background-color "#121212")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f36c60")
     (40 . "#ff9800")
     (60 . "#fff59d")
     (80 . "#8bc34a")
     (100 . "#81d4fa")
     (120 . "#4dd0e1")
     (140 . "#b39ddb")
     (160 . "#f36c60")
     (180 . "#ff9800")
     (200 . "#fff59d")
     (220 . "#8bc34a")
     (240 . "#81d4fa")
     (260 . "#4dd0e1")
     (280 . "#b39ddb")
     (300 . "#f36c60")
     (320 . "#ff9800")
     (340 . "#fff59d")
     (360 . "#8bc34a"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Input Mono Compressed" :foundry "FBI " :slant normal :weight normal :height 105 :width extra-condensed))))
 )
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
))
