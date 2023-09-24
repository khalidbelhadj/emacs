;; Package setup

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (eval-when-compile
    (unless (bound-and-true-p package--initialized)
      (package-initialize))
    (require 'use-package)))

(require 'use-package)
(setq use-package-always-ensure t
      use-package-compute-statistics t)

;; Try
(use-package try
  :commands try)

;; Path
(use-package exec-path-from-shell)

;; (when (memq window-system '(mac ns x))
;;   (exec-path-from-shell-initialize))

;; Custom file
(load (expand-file-name "custom.el" user-emacs-directory) nil t)

;; Emacs 29
(unless (< emacs-major-version 29)
  (pixel-scroll-precision-mode))


;; Custom functions --------------------------

(defun finder ()
  "Opens file directory in Finder."
  (interactive)
  (let ((file (buffer-file-name)))
    (if file
        (shell-command
         (format "%s %s" (executable-find "open") (file-name-directory file)))
      (error "Buffer is not attached to any file."))))

;; Behave like vi's o command
(defun my/open-next-line (ARG)
  "Move to the next line and then opens a line.  See also `newline-and-indent' (not sure what ARG is for)."
  (interactive "p")
  (end-of-line)
  (open-line ARG)
  (forward-line 1)
  (indent-according-to-mode))

;; Behave like vi's O command
(defun my/open-previous-line (ARG)
  "Open a new line before the current one (not sure what ARG is for).
    See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line ARG)
  (indent-according-to-mode))

(defun my/scroll-half-page-forward-center (ARG)
  "Scroll half a page forward and center the cursor (not sure what ARG is for)."
  (interactive "p")
  (View-scroll-half-page-forward)
  (recenter))

(defun my/scroll-half-page-backward-center (ARG)
  "Scroll half a page forward and center the cursor (not sure what ARG is for)."
  (interactive "p")
  (View-scroll-half-page-backward)
  (recenter))

(defun my/duplicate-line ()
  "Duplicate current line."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

(defun my/kill-to-bol ()
  "Kill from point to beginning of line."
  (interactive)
  (kill-line 0)
  (setq kill-ring (cdr kill-ring)))

(defun my/kill-backward-to-first-char ()
  "Delete line, then all whitespace backward until a non-whitespace\n char is reached on the previous line."
  (interactive)
  (kill-line 0)
  (if (equal (current-column) 0)
      (delete-backward-char 1)
    (message "Unreachable in my/kill-backward-to-first-char")))

(defun my/copy-current-line-or-region ()
  "Copy the current line if there is no selection, otherwise copy normally."
  (interactive)

  (if (use-region-p)
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (+ 1 (line-end-position)))
    (message "Line copied")))

(defun open-config ()
  (interactive)
  (dired user-emacs-directory))

(defun my/info-hover ()
  (interactive)
  (if (flycheck-overlay-errors-at (point))
      (flycheck-display-error-at-point)
    (if lsp-ui-doc-mode
        (if lsp-ui-doc-show-with-cursor
            (message "Disable lsp-ui-doc-show-with-cursor for info-hover to work")
          (lsp-ui-doc-glance))
      (message "Enable lsp-ui-doc-mode for hover documentation"))))

(autoload 'View-scroll-half-page-forward "view")
(autoload 'View-Scroll-half-page-backward "view")

;; Keymaps

;; General
(bind-key* "C-c c" 'comment-or-uncomment-region)
(bind-key* "C-c w" 'count-words)
(bind-key* "C-c g" 'go-to-char-forward)
(bind-key* "C-c G" 'go-to-char-backward)
(bind-key* "C-c t" 'vterm)
(bind-key* "C-=" 'text-scale-increase)
(bind-key* "C--" 'text-scale-decrease)
(bind-key* "C-0" 'text-scale-adjust)
(bind-key* "C-x C-r" 'consult-recent-file)
(bind-key* "C-x c" 'open-config)
(bind-key* "M-q" 'save-buffers-kill-terminal)
(bind-key* "C-M-j" 'join-line)
(bind-key* "C-c n" 'next-buffer)
(bind-key* "C-c p" 'previous-buffer)


;; Defined functions
(bind-key* "C-c d" 'my/duplicate-line)
(bind-key* "C-o" 'my/open-next-line)
(bind-key* "M-o" 'my/open-previous-line)
(bind-key* "C-<backspace>" 'my/kill-to-bol)
(bind-key* "C-o" 'my/open-next-line)
(bind-key* "C-M-<backspace>" 'my/kill-backward-to-first-char)
(bind-key* "M-w" 'my/copy-current-line-or-region)
(bind-key* "C-v" 'my/scroll-half-page-forward-center)
(bind-key* "M-v" 'my/scroll-half-page-backward-center)
(bind-key* "C-S-k" 'my/info-hover)

(use-package expand-region
  :commands er/expand-region
  :init
  (bind-key* "C-M-=" 'er/expand-region)
  (bind-key* "C-M--" 'er/contract-region))

;; Navigation
(bind-key* "M-H" 'windmove-left)
(bind-key* "M-L" 'windmove-right)
(bind-key* "M-K" 'windmove-up)
(bind-key* "M-J" 'windmove-down)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Icons
(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :config (setq all-the-icons-dired-monochrome nil))

(use-package all-the-icons-completion
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup))

;; Theme
(use-package doom-themes)
(setq custom-theme-directory (expand-file-name "themes/" user-emacs-directory))
(load-theme 'doom-fleet t) ;; t for NO-CONFIRM

;; Neotree
(use-package neotree
  :commands (neotree neotree-toggle)
  :init (setq neo-window-width 25)
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(bind-key* "C-c C-SPC" 'neotree-toggle)

(defun large-line-spacing ()
  (setq-local line-spacing 0.3))

(add-hook 'neotree-mode-hook 'large-line-spacing)

;; Modeline
(use-package minions
  :init (minions-mode 1))

;; Highlight indent guides
(use-package highlight-indent-guides
  ;;  :hook (prog-mode . highlight-indent-guides-mode)
  :config (setq highlight-indent-guides-method 'character))

;; Whitespace
(setq whitespace-style '(face
                         tabs
                         spaces
                         space-before-tab
                         indentation
                         space-after-tab
                         space-mark
                         tab-mark))

(add-hook 'after-save-hook 'delete-trailing-whitespace)

;; Line number
;; Not sure where I should put this
(add-hook 'prog-mode-hook 'hl-line-mode)

(global-display-line-numbers-mode)
;; (setq display-line-numbers-type 'relative)
(dolist (mode '(
                neotree-mode-hook
                org-mode-hook
                org-agenda-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                doc-view-mode-hook
                pdf-view-mode-hook
                undo-tree-visualizer-hook
                markdown-mode-hook
                lsp-ui-imenu-mode-hook
                helpful-mode-hook
                treemacs-mode-hook
                compilations-mode-hook
                emacs-lisp-compilation-mode-hook
                shell-maker-mode-hook
                vterm-mode-hook
                dired-mode-hook
                haskell-interactive-mode-hook
                ))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :config (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                              :foreground "red"
                              :inherit 'error
                              :box t))
;; HL TODO
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

;; Multiple cursors
(use-package multiple-cursors
  :commands multiple-cursors-mode
  :init (setq mc/always-run-for-all t)
  :config
  (multiple-cursors-mode t)
  (bind-key* "C-c SPC" 'mc/edit-lines))

(use-package drag-stuff
  :config
  (bind-key* "M-N" 'drag-stuff-down)
  (bind-key* "M-P" 'drag-stuff-up))

(use-package vterm
  :commands vterm vterm-mode)

;; Swiper
(use-package swiper
  :commands (swiper-isearch swiper))
(bind-key* "C-s" 'swiper-isearch)

;; Vertico Constult
(use-package vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :init (progn (vertico-mode 1) (setq vertico-count 15))
  :config (setq consult-preview-excluded-files '("README\.org")))

(use-package consult
  :commands (consult-recent-file consult-buffer consult-yank-from-kill-ring))
(global-set-key (kbd "M-y") 'consult-yank-from-kill-ring)
(global-set-key (kbd "C-x b") 'consult-buffer)

;; Marginalia
(use-package marginalia
  :after vertico
  :init (marginalia-mode 1))

;; Orderless
(use-package orderless
  :init (setq completion-styles '(orderless)))

;; Emnbark
(use-package embark
  :commands embark-act
  :config (bind-key* "C-." 'embark-act))

(use-package embark-consult)

(use-package embark-consult
  :after embark)

;; Company
(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :init (setq
         company-minimum-prefix-length 1
         company-idle-delay 0
         company-tooltip-idle-delay 0
         company-tooltip-align-annotations t
         company-tooltip-offset-display 'lines
         company-tooltip-maximum-width 50)
  :config
  (setq company-files-path-caches '("~/Desktop" "~/Documents" "~/Khalid's Vault"))
  (add-to-list 'company-backends 'company-files))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :init (setq company-box-scrollbar nil))

(use-package company-statistics
  :commands prog-mode
  :hook (company-mode . company-statistics-mode)
  :config
  (setq company-transformers '(company-sort-by-backend-importance
                               company-sort-by-statistics
                               company-sort-prefer-same-case-prefix
                               company-sort-by-occurrence)))

;; Treesitter
(use-package tree-sitter-langs
  :init (setq tree-sitter-langs-git-dir (concat user-emacs-directory "tree-sitter-langs/"))
  :commands tree-sitter-mode)

(use-package tree-sitter
  :after tree-sitter-langs
  :hook (lsp-mode . global-tree-sitter-mode)
  :init (setq tree-sitter-debug-jump-buttons t)
  :config (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(dolist (mode '(python-mode-hook
                c-mode-hook
                rustic-mode-hook
                java-mode-hook
                js-jsx-mode-hook
                typescript-mode-hook))
  (add-hook mode 'tree-sitter-mode))

;; Tree sitter tsx
;; (setq tree-sitter-major-mode-language-alist (assq-delete-all 'typescript-mode tree-sitter-major-mode-language-alist))
;; (setq tree-sitter-major-mode-language-alist (cons '(typescript-mode . tsx) tree-sitter-major-mode-language-alist))

;; (setq treesit-language-source-alist
;;       '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src" nil nil)
;;         (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src" nil nil)))

;; LSP
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init (setq lsp--prefix "C-M-l")
  :config
  (if (package-installed-p 'which-key)
      (lsp-enable-which-key-integration 1))
   (setq
   lsp-prefer-flymake nil
   lsp-headerline-breadcrumb-enable nil
   lsp-signature-render-documentation nil
   lsp-pylsp-plugins-flake8-enabled t
   lsp-pylsp-plugins-pylint-enabled t
   lsp-pylsp-plugins-autopep8-enabled t
   lsp-pylsp-plugins-pydocstyle-enabled nil)
   )

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable nil
        lsp-ui-doc-position 'at-point)
  (lsp-ui-sideline-mode -1)
  (lsp-signature-mode -1)
  (bind-key* (concat lsp-keymap-prefix " u i") 'lsp-ui-imenu))

;; (dolist (mode '(python-mode-hook
;;                 c-mode-hook
;;                 c++-mode-hook
;;                 typescript-mode-hook
;;                 web-mode-hook
;;                 haskell-mode-hook
;;                 java-mode-hook
;;                 rustic-mode-hook))
;;   (add-hook mode 'lsp-deferred))


;; Flycheck
(use-package flycheck
  :commands flycheck-mode
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit
        flycheck-idle-change-delay 6969 ;; HACK
        flycheck-buffer-switch-check-intermediate-buffers t
        flycheck-display-errors-delay 6969) ;; HACK
  (delq 'new-line flycheck-check-syntax-automatically))

(use-package flycheck-posframe
  :commands flycheck-posframe-mode
  :hook (flycheck-mode . flycheck-posframe-mode))


;; Langs

;; Rust
(use-package rustic
  :commands rustic-mode
  :init (setq rust-match-angle-brackets nil))

;; Haskell
(use-package haskell-mode
  :commands haskell-mode
  :config
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (setq haskell-interactive-popup-errors nil))

(use-package lsp-haskell
  :commands lsp-mode)

;; Python
(use-package python-mode
  :commands python-mode
  :config
  (setq
   python-shell-interpreter "python3"
   dap-python-executable "python3"
   dap-python-debugger 'debugpy))

(use-package lsp-pyright
  ;;  :after lsp-mode
  :custom (lsp-pyright-typechecking-mode "basic"))

;; Web
(use-package json-mode
  :commands json-mode)

(use-package web-mode
  :commands (web-mode typescript-mode)
  :init (setq web-mode-code-indent-offset 2
              web-mode-auto-close-style 2
              web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")
                                             ("jsx" . "\\.ts[x]?\\'"))))

;; Typescript
;; (use-package typescript-mode
;;   :commands typescript-mode
;;   :init (setq typescript-indent-level 2))

;; Tailwind
(use-package lsp-tailwindcss
  :config (setq lsp-tailwindcss-add-on-mode t))

;; Auto rename tag
(use-package auto-rename-tag
  :hook
  (js2-mode    . auto-rename-tag-mode)
  (js-jsx-mode . auto-rename-tag-mode)
  (js-mode     . auto-rename-tag-mode)
  (mhtml-mode  . auto-rename-tag-mode)
  (css-mode    . auto-rename-tag-mode))

;; Java
(use-package lsp-java
  :commands java-mode)

;; Yaml
(use-package yaml-mode)

;; LaTex
(use-package latex-mode
  :ensure nil
  :hook (latex-mode . toggle-word-wrap))
;; :init (setq font-latex-fontify-sectioning 'color)))

;; Git
(use-package magit
  :commands magit-mode)

(use-package magit-todos
  :after magit
  :config (magit-todos-mode 1))

(use-package git-gutter
  :hook ((prog-mode . git-gutter-mode) (org-mode .git-gutter-mode)))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated)))

;; Dired
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;; Which key
(use-package which-key
  :init (which-key-mode 1))

;; Helpful
(use-package helpful
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

;; Popper
(use-package popper
  :bind* (("C-c :" . popper-toggle-latest)
          ("C-\\"  . popper-cycle))
  :bind (("C-`"   . popper-toggle-latest)
         ("C-M-`" . popper-toggle-type))
  :init (setq popper-group-function #'popper-group-by-project)
  :config
  (popper-mode 1)
  (popper-echo-mode 1)
  :custom
  (popper-window-height 24)
  (popper-reference-buffers '(
                              ;; "\\*Messages\\*"
                              ;; "magit:.\*"
                              "Output\\*$"
                              "\\*Async Shell Command\\*"
                              "\\*rustic-compilation\\*"
                              "\\o*deadgrep.\*"
                              "\\*eldoc.\*"
                              "\\*Codespaces\\*"
                              "\\*xref\\*"
                              "\\*org-roam\\*"
                              "\\*direnv\\*"
                              "\\*tidal\\*"
                              "\\*Checkdoc Status\\*"
                              "\\*Warnings\\*"
                              "\\*Go Test\\*"
                              "\\*Bookmark List\\*"
                              vterm-mode
                              help-mode
                              prodigy-mode
                              haskell-compilation-mode
                              compilation-mode
                              )))

;; PDF
(use-package pdf-tools
  :commands pdf-tools-install
  :init
  (local-set-key (kbd "c") 'pdf-view-center-in-window)
  (setq pdf-view-resize-factor 1.025
        doc-view-continuous t
        pdf-view-display-size 'fit-height))

(add-hook 'doc-view-mode-hook 'pdf-tools-install)

;; (add-hook 'after-init-hook 'pdf-tools-install)

;; AI
(use-package chatgpt-shell
  :commands chatgpt-shell-mode
  :config
  (exec-path-from-shell-copy-env "OPENAI_API_KEY")
  (setq chatgpt-shell-openai-key (getenv "OPENAI_API_KEY")))


(use-package typescript-mode
  :init
  (when (require 'web-mode nil 'noerror)
    (autoload 'typescript-tsx-mode "typescript-mode" nil t))

  ;; REVIEW We associate TSX files with `typescript-tsx-mode' derived from
  ;;        `web-mode' because `typescript-mode' does not officially support
  ;;        JSX/TSX. See emacs-typescript/typescript.el#4
  (add-to-list 'auto-mode-alist
               (cons "\\.tsx\\'"
                     (if (require 'web-mode nil 'noerror)
                         #'typescript-tsx-mode
                       #'typescript-mode)))
  :config
  (when (fboundp 'web-mode)
    (define-derived-mode typescript-tsx-mode web-mode "TypeScript-TSX")
    (when (require 'lsp-mode nil 'noerror)
      (add-to-list 'lsp--formatting-indent-alist '(typescript-tsx-mode . typescript-indent-level)))
    (when (require 'tree-sitter-mode nil 'noerror)
      (pushnew! tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx))
      ;; HACK: the tsx grammer doesn't work with the hightlighting provided by
      ;;   font-lock-keywords. See emacs-tree-sitter/tree-sitter-langs#23
      (setq-hook! 'typescript-tsx-mode-hook
                  tree-sitter-hl-use-font-lock-keywords nil))))
