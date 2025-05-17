;;; package -- deviantfero's ruby.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; File to configure Ruby related packages
;;; features
;;; Code:

;;; Ruby
(use-package ruby-mode
  :ensure nil
  :config
  (setq ruby-insert-encoding-magic-comment nil)
  :hook
  (ruby-mode . lsp))

(use-package projectile-rails
  :hook (ruby-mode . projectile-rails-mode)
  :config (define-key projectile-rails-mode-map (kbd "C-c r") 'projectile-rails-command-map))

(use-package rvm
  :hook (ruby-mode . rvm-activate-corresponding-ruby))

(use-package rubocop
  :hook (ruby-mode . rubocop-mode))
;;; ruby.el ends here
