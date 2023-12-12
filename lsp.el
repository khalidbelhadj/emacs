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
   lsp-signature-render-documentation nil))

;; LSP UI
(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable nil
        lsp-ui-doc-position 'at-point)
  (lsp-ui-sideline-mode -1)
  (lsp-signature-mode -1)
  (bind-key* (concat lsp-keymap-prefix " u i") 'lsp-ui-imenu))

(dolist (mode '(python-mode-hook
                c-mode-hook
                c++-mode-hook
                typescript-mode-hook
                web-mode-hook
                haskell-mode-hook
                java-mode-hook
                rustic-mode-hook))
  (add-hook mode 'lsp-deferred))

;; Pyright
(use-package lsp-pyright
  :after lsp-mode
  :custom (lsp-pyright-typechecking-mode "basic"))

;; Tailwind
(use-package lsp-tailwindcss
  :after lsp-mode
  :config (setq lsp-tailwindcss-add-on-mode t))

;; HLS
(use-package lsp-haskell
  :commands lsp-mode)
