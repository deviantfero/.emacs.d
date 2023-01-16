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
  (menu-bar-mode -1)
  (toggle-tool-bar-mode-from-frame -1)
  (winner-mode 1)
  (add-hook 'after-save-hook #'garbage-collect)
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  (setenv "SHELL" "/usr/bin/bash")
  (load-theme 'wpgtk t)
  (add-to-list 'default-frame-alist '(font . "sf Mono-10"))
  (setq-default ispell-local-dictionary-alist
				'(("es_ES" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)
				  ("en_EN" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)))
  (setq-default
   gc-cons-threshold 100000000
   read-process-output-max (* 1024 1024)
   native-comp-async-report-warnings-errors nil
   explicit-shell-file-name "/usr/bin/bash"
   package-native-compile t
   shell-file-name "bash"
   explicit-bash-args '("--login")
   backup-directory-alist '(("." . "~/.emacs.d/saves"))
   backup-by-copying t
   auto-save-default nil
   create-lockfiles nil
   dired-listing-switches "-ahl"
   inhibit-startup-message t
   tab-width 4
   blink-matching-paren nil
   toggle-scroll-bar nil
   ispell-program-name (executable-find "hunspell")
   ispell-dictionary "es_ES"
   help-window-select t))

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

(use-package magit
  :after evil
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :bind (:map evil-normal-state-map
			  ("<f9>" . magit-status)))

(use-package dired+
  :ensure nil
  :after dired
  :init (setq diredp-hide-details-initially-flag nil)
  :load-path "~/.emacs.d/packages/dired+")

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
;;; emacs.el ends here
