;;; package -- deviantfero's python.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; File to configure Python related packages
;;; features
;;; Code:

;;; Python
(use-package python-mode
  :hook
  (python-mode . lsp)
  (python-mode . tree-sitter-hl-mode)
  :config
  (outline-minor-mode 0))

(use-package python-black
  :bind (("C-c b" . python-black-buffer)))

(use-package anaconda-mode
  :bind (("C-c C-x" . next-error))
  :hook
  (python-mode . anaconda-mode))

(use-package pyvenv
  :hook
  (anaconda-mode . pyvenv-mode))

;;; python.el ends here
