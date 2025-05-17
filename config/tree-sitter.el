;;; package -- deviantfero's tree-sitter.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; File to configure Tree-Sitter related packages
;;; features
;;; Code:

(use-package tree-sitter
  :hook
  (ruby-mode . tree-sitter-hl-mode)
  (python-mode . tree-sitter-hl-mode))

(use-package tree-sitter-langs)
(use-package treesit-auto)

;;; tree-sitter.el ends here
