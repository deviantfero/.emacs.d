;;; package -- deviantfero's lsp.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; File to configure lsp related packages without a specific language
;;; Code:

(use-package lsp-mode
  :commands lsp
  :config
  (setq lsp-headerline-breadcrumb-enable nil))

(use-package lsp-ui
  :hook
  (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-sideline-enable nil))

(use-package dap-mode
  :after lsp-mode
  :hook
  (lsp-diagnostics-mode . (lambda () (setq-default lsp-diagnostics-attributes nil)))
  :config (dap-auto-configure-mode))

;;; lsp.el ends here
