;; MacOS Settings
(when (equal system-type 'darwin)
    (setq mac-command-modifier 'meta
          mac-option-modifier 'meta))

;; Setting font
(set-face-attribute 'default nil
                    :family "JetBrainsMono Nerd Font"
                    :height 130)


(add-hook 'focus-out-hook 'garbage-collect)

;; Setting defualt variables
(setq-default line-spacing 0.10
              indent-tabs-mode nil
              recentf-max-saved-items 50
              frame-title-format
              '((:eval (if (buffer-file-name)
                           (abbreviate-file-name (buffer-file-name))
                         "%b"))))

;; Better default variables
(setq auto-mode-case-fold                   nil
      fast-but-imprecise-scrolling          t
      ffap-machine-p-known                  'reject
      inhibit-compacting-font-caches        t
      redisplay-skip-fontification-on-input t
      default-input-method                  nil
      gc-cons-threshold                     100000000
      read-process-output-max               (* 1024 1024)
      inhibit-startup-screen                t
      ring-bell-function                    'ignore
      history-length                        20
      scroll-margin                         8
      ivy-initial-inputs-alist              nil
      backup-directory-alist                `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms        `((".*" ,temporary-file-directory t))
      create-lockfiles                      nil
      windmove-wrap-around                  t
      dashboard-footer-icon                 ""
      dashboard-footer-messages             '()
      trash-directory                       "~/.Trash/"
      display-time-default-load-average     nil
      electric-pair-preserve-balance        nil
      frame-resize-pixelwise                t
      auto-window-vscroll                   nil
      load-prefer-newer                     t
      custom-file                           (expand-file-name "custom.el" user-emacs-directory))

;; Enabling and disabling default modes
(scroll-bar-mode               -1)
(tool-bar-mode                 -1)
(auto-revert-mode              1)
(global-auto-revert-mode       1)
(electric-pair-mode            1)
(display-battery-mode          1)
(global-visual-line-mode       1)

(windmove-default-keybindings)
(display-time)

;; Misc
(set-language-environment "UTF-8")

(dolist (entry '(("/LICENSE\\'" . text-mode)
                 ("\\.log\\'" . text-mode)
                 ("rc\\'" . conf-mode)))
  (push entry auto-mode-alist))

;; After init
(defun after-init ()
  (save-place-mode 1)
  (savehist-mode 1)
  (recentf-mode 1)

  ;; Transparent titlebar
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

(add-hook 'after-init-hook 'after-init)

(defun display-startup-echo-area-message ()
  (message ""))
