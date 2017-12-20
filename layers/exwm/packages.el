;;; packages.el --- exwm Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.

(setq exwm-packages
    '(cl-generic
      (xelb :location (recipe :fetcher github
                              :repo "ch11ng/xelb")
            :step pre)
      (exwm :location (recipe :fetcher github
                              :repo "ch11ng/exwm")
            :step pre)))

(defun exwm/init-cl-generic ()
  (use-package cl-generic
    :demand))

(defun exwm/init-xelb ()
  (use-package xelb
    :demand))

(defun exwm/init-exwm ()
  (use-package exwm
    :demand
    :init
    ;; Disable dialog boxes since they are unusable in EXWM
    (setq use-dialog-box nil)

    ;; 10 Worskpaces please
    (setq exwm-workspace-number 9
          exwm-workspace-show-all-buffers t
          exwm-layout-show-all-buffers t)

    (require 'exwm-systemtray)
    (exwm-systemtray-enable)

    (display-time-mode t)
    (when exwm--hide-tiling-modeline
      (add-hook 'exwm-mode-hook #'hidden-mode-line-mode))
    :config

    ;; Bind some keys
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

    (defun spacemacs/exwm-application-launcher (command)
      "Launches an application in your PATH.
Can show completions at point for COMMAND using helm or ido"
      (interactive (list (read-shell-command exwm-app-launcher--prompt)))
      (start-process-shell-command command nil command))

    (exwm-input-set-key (kbd "s-SPC") #'spacemacs/exwm-application-launcher)

    ;; lock screen
    (exwm-input-set-key (kbd "<s-escape>")
                        (lambda () (interactive) (start-process "" nil exwm--locking-command)))

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
    (defvar exwm-workspace-switch-wrap t
      "Whether `spacemacs/exwm-workspace-next' and `spacemacs/exwm-workspace-prev' should wrap.")

    (defun spacemacs/exwm-layout-toggle-fullscreen ()
      "Togggles full screen for Emacs and X windows"
      (interactive)
      (if exwm--id
          (if exwm--fullscreen
              (exwm-reset)
            (exwm-layout-set-fullscreen))
        (spacemacs/toggle-maximize-buffer)))

    ;; Quick swtiching between workspaces
    (defvar exwm-toggle-workspace 0
      "Previously selected workspace. Used with `exwm-jump-to-last-exwm'.")
    (defun exwm-jump-to-last-exwm ()
      (interactive)
      (exwm-workspace-switch exwm-toggle-workspace))
    (defadvice exwm-workspace-switch (before save-toggle-workspace activate)
      (setq exwm-toggle-workspace exwm-workspace-current-index))

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
    ;; Undo window configurations
    (exwm-input-set-key (kbd "s-u") #'winner-undo)
    (exwm-input-set-key (kbd "S-s-u") #'winner-redo)
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


    (exwm-input-set-key (kbd "s-o")
                        (lambda ()
                          (interactive)
                          (exwm-input-toggle-mode)
                          (org-capture)))
    ))
