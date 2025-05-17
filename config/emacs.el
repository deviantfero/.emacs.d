;;; package -- deviantfero's emacs.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; File to configure EMACS packages that provide nice editor
;;; features
;;; Code:

(use-package emacs
  :ensure nil
  :init
  (column-number-mode 1)
  (show-paren-mode 1)
  (electric-pair-mode 1)
  (winner-mode 1)
  (add-hook 'after-save-hook #'garbage-collect)
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  (setenv "SHELL" "/usr/bin/bash")
  (load-theme 'wpgtk t)
  (setq-default ispell-local-dictionary-alist
		        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)
		          ("es_ES" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)))
  (setq-default
   explicit-shell-file-name "/usr/bin/bash"
   shell-file-name "bash"
   explicit-bash-args '("--login")
   backup-directory-alist '(("." . "~/.emacs.d/saves"))
   backup-by-copying t
   auto-save-default nil
   create-lockfiles nil
   dired-listing-switches "-ahl"
   tab-width 4
   blink-matching-paren nil
   ispell-program-name (executable-find "hunspell")
   ispell-dictionary "en_US"
   help-window-select t))

(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package rainbow-mode)
(use-package edit-indirect)
(use-package multi-line
  :after evil
  :bind (:map global-map
	          ("C-c d" . multi-line)
	          :map evil-normal-state-map
	          ("M-j" . multi-line)))

(use-package projectile
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
	          ("<f7>" . projectile-run-vterm)
	          ("C-c p" . projectile-command-map))
  :config
  (setq projectile-completion-system 'ivy)
  (setq projectile-switch-project-action 'projectile-dired))

(use-package flycheck
  :hook
  (web-mode . my/set-local-eslint)
  :init
  (setq flycheck-python-ruff-executable (executable-find "ruff"))
  (setq flycheck-python-pycompile-executable (executable-find "python3"))
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (global-flycheck-mode +1))

(use-package flycheck-pkg-config
  :after (flycheck))

(use-package magit
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :bind (:map evil-normal-state-map
	          ("<f9>" . magit-status)))

(use-package dired+
  :ensure nil
  :after dired
  :init (setq diredp-hide-details-initially-flag nil)
  :load-path "~/.emacs.d/packages/dired+"
  :bind (:map dired-mode-map
	          ("M-Y" . diredp-copy-abs-filenames-as-kill)))

(use-package asdf
  :ensure nil
  :load-path "~/.emacs.d/packages/"
  :config
  (asdf-enable))

(use-package too-long-lines-mode
  :ensure nil
  :load-path "~/.emacs.d/packages")

(use-package dtrt-indent
  :config
  (setq dtrt-indent-min-quality 90.0)
  (dtrt-indent-global-mode 1))

(use-package xclip
  :config (xclip-mode 1))

(use-package vterm
  :hook
  (vterm-copy-mode . meliache/evil-normal-in-vterm-copy-mode)
  :config
  (defun meliache/evil-normal-in-vterm-copy-mode ()
    (if (bound-and-true-p vterm-copy-mode)
        (evil-normal-state)
      (evil-insert-state)))
  (setq vterm-shell "/bin/zsh")
  (setq vterm-buffer-name-string "vterm: %s"))

(use-package eterm-256color)

(use-package which-key
  :config (which-key-mode 1))

(use-package smartparens
  :config
  (setq sp-show-pair-from-inside nil)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-cancel-autoskip-on-backward-movement t)
  (require 'smartparens-config)
  (smartparens-global-mode t))

(use-package undo-tree
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (global-undo-tree-mode))
(use-package add-node-modules-path)
(use-package minions
  :config (minions-mode 1))

(use-package ace-window
  :config
  (setq aw-scope 'frame)
  :bind (:map global-map
	          ("C-x o" . ace-window)))

(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

(use-package string-inflection)
;;; emacs.el ends here
