;;; package -- deviantfero's .init.el -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(require 'package)
(require 'mouse)
(require 'dired-x)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(package-initialize)

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))

(dolist (basic-offset '(c-basic-offset sh-basic-offset))
  (defvaralias basic-offset 'tab-width))

;;; Custom functions
(defun sudo-write ()
  "Use TRAMP to open a file with write access using sudo."
  (interactive)
  (if (not buffer-file-name)
	  (write-file (concat "/sudo:root@localhost:" (ido-read-file-name)))
	(write-file (concat "/sudo:root@localhost:" (buffer-file-name)))))

(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
	  (setq deactivate-mark  t)
	(when (get-buffer "*Completions*") (delete-windows-on "*Completions*")) (abort-recursive-edit)))

;;; Feature Packages
(use-package emacs
  :init
  (load-theme 'wpgtk t)
  (column-number-mode 1)
  (show-paren-mode 1)
  (menu-bar-mode -1)
  (toggle-tool-bar-mode-from-frame -1)
  (winner-mode 1)
  (global-undo-tree-mode)
  (add-hook 'focus-out-hook #'garbage-collect)
  (add-hook 'after-save-hook #'garbage-collect)
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  (setenv "SHELL" "/usr/bin/bash")
  (setq-default ispell-local-dictionary-alist
				'(("es_ES" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)
				  ("en_EN" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)))
  (setq-default
   explicit-shell-file-name "/usr/bin/bash"
   shell-file-name "bash"
   explicit-bash-args '("--login")
   backup-directory-alist '(("." . "~/.emacs.d/saves"))
   backup-by-copying t
   auto-save-default nil
   create-lockfiles nil
   inhibit-startup-message t
   tab-width 4
   blink-matching-paren nil
   toggle-scroll-bar nil
   ispell-program-name (executable-find "hunspell")
   ispell-dictionary "es_ES"
   help-window-select t))

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
  (setq projectile-switch-project-action 'magit-status))

(use-package magit
  :after evil
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :bind (:map evil-normal-state-map
			  ("<f9>" . magit-status)))

(use-package dired+
  :after dired
  :init (setq diredp-hide-details-initially-flag nil)
  :load-path "~/.emacs.d/packages/dired+")

(use-package dtrt-indent
  :config
  (setq dtrt-indent-min-quality 90.0)
  (dtrt-indent-global-mode 1))

(use-package xclip
  :config (xclip-mode 1))

(use-package vterm
  :config
  (setq vterm-shell "/bin/zsh"))

(use-package yasnippet
  :config
  (yas-global-mode 1)
  :bind (:map yas-keymap
			  ("TAB" . nil)
			  ("C-o" . yas-next-field-or-maybe-expand)))

(use-package which-key
  :config (which-key-mode 1))

(use-package smartparens
  :config
  (setq sp-show-pair-from-inside nil)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-cancel-autoskip-on-backward-movement t)
  (require 'smartparens-config)
  (smartparens-global-mode t))

(use-package undo-tree)
(use-package add-node-modules-path)
(use-package minions
  :config (minions-mode 1))

;;; Tramp
(use-package docker-tramp)
(use-package tramp
  :defer 5
  :config
  (with-eval-after-load 'tramp-cache
	(setq tramp-persistency-file-name "~/.emacs.d/tramp"))
  (setq-default projectile-mode-line "Projectile")
  (setq tramp-use-ssh-controlmaster-options nil)
  (setf tramp-persistency-file-name
		(concat temporary-file-directory "tramp-" (user-login-name))))

;;; Evil
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-tree)
  (define-key evil-normal-state-map (kbd "<f5>") 'compile)
  (define-key evil-normal-state-map (kbd "-") 'dired-jump)
  (define-key evil-normal-state-map (kbd "ga") 'align-regexp)
  (define-key evil-normal-state-map (kbd "gt") 'other-frame)
  (define-key evil-normal-state-map (kbd "C-w n") 'make-frame-command)
  (define-key evil-normal-state-map (kbd "C-w C-s") 'frameset-to-register)
  (define-key evil-normal-state-map (kbd "C-w C-r") 'jump-to-register)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "M-u") 'winner-undo)
  (define-key evil-normal-state-map (kbd "M-r") 'winner-redo)
  (evil-define-key 'normal lsp-mode-map (kbd "K") 'lsp-describe-thing-at-point)
  (evil-define-key 'normal lsp-mode-map (kbd "\\") lsp-command-map))

(use-package evil-collection
  :custom (evil-collection-setup-minibuffer t)
  :init (evil-collection-init))

(use-package evil-org
  :hook
  (org-mode . evil-org-mode)
  (evil-org-mode . (lambda () (evil-org-set-key-theme)))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode 1))

(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode 1))

(use-package evil-matchit
  :config (global-evil-matchit-mode 1))

(use-package evil-visualstar
  :config (global-evil-visualstar-mode))

(use-package vimish-fold
  :after evil)

(use-package evil-vimish-fold
  :after vimish-fold
  :config
  (global-evil-vimish-fold-mode))

;;; Helm
(defun my/helm-open-split-vrt (_candidate)
  "Opens a file or a buffer in a vertically split window to the right."
  (require 'winner)
  (dolist (buf (helm-marked-candidates))
	(select-window (split-window-right))
	(if (stringp buf)
		(find-file buf)
	  (switch-to-buffer buf)))
  (balance-windows))

(defun my/helm-open-split-hor (_candidate)
  "Opens a file or a buffer in a horizontally split window bellow."
  (require 'winner)
  (dolist (buf (helm-marked-candidates))
	(select-window (split-window-below))
	(if (stringp buf)
		(find-file buf)
	  (switch-to-buffer buf)))
  (balance-windows))

(defun helm-open-split-horizontal ()
  "Keybinded function to call horizontal split on helm."
  (interactive)
  (with-helm-alive-p
	(helm-quit-and-execute-action 'my/helm-open-split-hor)))

(defun helm-open-split-vertical ()
  "Keybinded function to call vertical split on helm."
  (interactive)
  (with-helm-alive-p
	(helm-quit-and-execute-action 'my/helm-open-split-vrt)))

(use-package helm
  :after evil
  :bind (:map evil-normal-state-map
			  ("M-k" . helm-do-ag)
			  ("C-f" . helm-find-files)
			  ("C-o" . helm-buffers-list)
			  :map helm-map
			  ("TAB" . helm-execute-persistent-action)
			  ("C-x v" . helm-open-split-vertical)
			  ("C-x x" . helm-open-split-horizontal)
			  :map global-map
			  ("M-x" . helm-M-x))
  :config
  (helm-mode 1)
  (setq helm-completion-style 'flex)
  (setq helm-split-window-inside-p t)
  (setq helm-display-header-line nil)
  (setq helm-full-frame nil)
  (helm-autoresize-mode t)
  (add-to-list 'display-buffer-alist
			   `(,(rx bos "*helm" (* not-newline) "*" eos)
				 (display-buffer-in-side-window)
				 (inhibit-same-window . t)))
  (setq helm-autoresize-min-height 20)
  (setq helm-autoresize-max-height 20)
  (dolist (action
		   '(("Display buffer(s) in new window(s) `C-x v'" . my/helm-open-split-vrt)
			("Display buffer(s) in new window(s) `C-x x'" . my/helm-open-split-hor)))
		   (add-to-list 'helm-type-buffer-actions action 'append))
  (dolist (action
		   '(("Display buffer(s) in new window(s) `C-x v'" . my/helm-open-split-vrt)
			("Display buffer(s) in new window(s) `C-x x'" . my/helm-open-split-hor)))
		   (add-to-list 'helm-find-files-actions action 'append)))

(use-package helm-ag)
(use-package helm-swoop)

(use-package helm-projectile
  :after evil
  :bind (:map evil-normal-state-map
			  ("C-g" . helm-projectile))
  :config
  (helm-projectile-on))

;;; lsp
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

;;; Javascript/HTML/REST
(defun my/activate-tide-mode ()
  "Use hl-identifier-mode only on js or ts buffers."
  (when (and (stringp buffer-file-name)
			 (string-match "\\.[tj]sx?\\'" buffer-file-name))
	(tide-setup)
	(tide-hl-identifier-mode)))

(defun my/set-local-eslint ()
  "Use local node_modules."
  (add-node-modules-path)
  (setq-local flycheck-javascript-eslint-executable (executable-find "eslint")))

(defun my/activate-prettier-mode ()
  "Try to find a prettier configuration if found, load prettier-mode."
  (let ((config-files '(".prettierrc" ".prettierrc.json" ".prettierrc.yaml" ".prettierrc.js")))
	(when (seq-some (lambda (elt) (file-exists-p (concat (projectile-project-root) elt))) config-files)
	  (prettier-mode))))

(use-package web-mode
  :mode
  ("\\.ejs\\'" "\\.vue\\'" "\\.erb\\'" "\\.hbs\\'" "\\.html\\'" "\\.php\\'" "\\.[jt]sx?\\'")
  :config
  (setq web-mode-content-types-alist '(("jsx" . "\\.[jt]sx?\\'")))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-script-padding 2)
  (setq web-mode-block-padding 2)
  (setq web-mode-style-padding 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-current-element-highlight t))

(use-package js2-mode
  :init
  (setq js2-mode-show-strict-warnings nil)
  (setq js2-mode-show-parse-errors 1))

(use-package tide
  :config
  (flycheck-add-next-checker 'tsx-tide 'javascript-eslint)
  :hook (web-mode . my/activate-tide-mode))

(use-package company-web
  :hook
  (mhtml-mode . (lambda () (add-to-list 'company-backends 'company-web-html))))

;;; GoLang
(use-package go-mode
  :mode ("\\.go\\'")
  :hook (go-mode . lsp)
  :config
  (setq lsp-gopls-codelens nil)
  (setq compile-command "echo Building... && go build -v && echo Testing... && go test -v && echo Linter... && golint")
  (setq  compilation-read-command nil))

;;; GdScript
(use-package gdscript-mode
  :config (setq warning-minimum-level :emergency)
  :hook (gdscript-mode . lsp))

;;; Elixir
(use-package elixir-mode)
(use-package alchemist
  :hook
  (elixir-mode . (lambda () (add-to-list 'company-backends 'alchemist-company))))

;;; C++/C
(use-package cmake-mode)
(use-package irony
  :hook
  (c++-mode . irony-mode)
  (c-mode . irony-mode)
  (objc-mode . irony-mode)
  :config
  (add-to-list 'irony-supported-major-modes 'bison-mode))

(use-package company-irony
  :after irony
  :hook
  (irony-mode . (lambda () (add-to-list 'company-backends 'company-irony)))
  (irony-mode . irony-cdb-autosetup-compile-options))

(use-package flycheck-irony
  :after (flycheck company-irony)
  :config
  (flycheck-irony-setup))

;;; Rust
(use-package rust-mode
  :hook (rust-mode . lsp))

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :hook
  (rust-mode . flycheck-rust-setup))

;;; Ruby
(use-package projectile-rails
  :hook (ruby-mode . projectile-rails-mode)
  :config (define-key projectile-rails-mode-map (kbd "C-c r") 'projectile-rails-command-map))

(use-package ruby-mode
  :config
  (setq ruby-insert-encoding-magic-comment nil)
  :hook (ruby-mode . lsp))

(use-package rvm
  :hook (ruby-mode . rvm-activate-corresponding-ruby))

;;; graphql
(use-package graphql-mode)

;;; Python
(use-package elpy
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))

;;; Clojure
(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
		 ("\\.edn\\'" . clojure-mode)))

(use-package cider
  :hook (clojure-mode . cider-mode))

;;; Conf-files
(use-package conf-mode
  :mode (("\\.env\\'" . conf-mode))
  :config
  (setq outline-regexp "[#\f]+"))

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package yaml-mode)

;;; Company
(use-package company
  :after evil
  :init
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (setq company-dabbrev-other-buffers nil)
  (company-tng-mode)
  :bind (:map evil-insert-state-map
              ("C-y" . company-yasnippet)
              ("C-f" . company-files)
              ("C-M-y" . yas-expand))
  :config
  (setq company-backends
		'((company-capf :with company-dabbrev-code :separate)
		  (company-files :with company-dabbrev-code)
		  (company-nxml company-dabbrev-code company-keywords :with company-yasnippet)
		  (company-oddmuse :with company-yasnippet)
		  (company-dabbrev :with company-yasnippet)))
  (global-company-mode 1))

;;; Flycheck
(use-package flycheck
  :hook (web-mode . my/set-local-eslint)
  :init
  (setq flycheck-python-flake8-executable (executable-find "flake8"))
  (setq flycheck-python-pycompile-executable (executable-find "python3"))
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (global-flycheck-mode 1))

(use-package flycheck-pkg-config
  :after (flycheck))

;;; Org
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c a" . org-agenda))
  :hook (org-mode . visual-line-mode)
  :config
  (setq org-agenda-files
		(directory-files-recursively "~/org/" "\.org$"))
  (org-babel-do-load-languages
   'org-babel-load-languages '((shell . t)
							   (python . t)
							   (ruby . t)
							   (C . t)))
  (add-to-list 'org-export-backends 'taskjuggler))


;;; Note taking
(use-package pdf-tools)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
		 ("\\.md\\'" . markdown-mode)
		 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config (setq outline-regexp "[#\f]+"))

(use-package tex
  :ensure auctex
  :config
  (use-package latex
	:ensure f
	:config
	(setq TeX-auto-save t)
	(setq TeX-parse-self t)
	(setq-default TeX-master nil)
	(setq-default TeX-engine "xetex")
	(setq-default TeX-save-query nil)
	(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
	(add-hook 'LaTeX-mode-hook 'flyspell-mode)
	(add-to-list 'TeX-view-program-list
				 '("zathura viewer" ("zathura" " %o" (mode-io-correlate " -P %(outpage)")))
				 (setcdr (assq 'output-pdf TeX-view-program-selection) '("zathura viewer")))))

;; (use-package ox-pandoc
;;   :after org)


;;; AUTOMATICALLY GENERATED
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-engine 'xetex)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#080808" "#d70000" "#67b11d" "#875f00" "#268bd2" "#af00df" "#00ffff" "#b2b2b2"])
 '(custom-safe-themes
   '("38e9cfe67b00e3c06b790dcacf80063fec1a69cd412e8c06c56394e622b91c49"))
 '(evil-collection-setup-minibuffer t)
 '(flycheck-javascript-flow-args nil)
 '(font-lock-maximum-decoration '((t . t) (dired-mode)))
 '(global-evil-mc-mode t)
 '(global-origami-mode t)
 '(helm-completion-style 'emacs)
 '(helm-minibuffer-history-key "M-p")
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
	 ("NEXT" . "#dc752f")
	 ("THEM" . "#2aa198")
	 ("PROG" . "#268bd2")
	 ("OKAY" . "#268bd2")
	 ("DONT" . "#d70000")
	 ("FAIL" . "#d70000")
	 ("DONE" . "#86dc2f")
	 ("NOTE" . "#875f00")
	 ("KLUDGE" . "#875f00")
	 ("HACK" . "#875f00")
	 ("TEMP" . "#875f00")
	 ("FIXME" . "#dc752f")
	 ("XXX" . "#dc752f")
	 ("XXXX" . "#dc752f")
	 ("???" . "#dc752f")))
 '(magit-diff-section-arguments '("--no-ext-diff"))
 '(org-agenda-files '("/home/fernando/org/todo.org"))
 '(org-latex-compiler "xelatex")
 '(package-selected-packages
   '(meson-mode prettier origami-mode helm-swoop vterm cider plan9-theme rvm gdscript-mode smartparens evil-vimish-fold cargo winner-mode lsp-ui lsp-mode lsp flycheck-rust graphql-mode multi-line helm-ag add-node-modules-path swiper dockerfile-mode ox-taskjuggler powerline evil-visualstar elpy evil-matchit smart-parens go-mode web-mode html-mode company-web dired dired-x minions moody which-key tide flycheck-irony space-line flycheck-pkg-config cmake-mode evil-magit async with-editor mmm-mode ssass-mode edit-indirect bind-key undo-tree tablist rich-minority s faceup quelpa dash f pythonic deferred python-environment epl pkg-info pos-tip popup markdown-mode magit-popup ghub git-commit json-snatcher json-reformat concurrent ctable epc jedi-core helm-core goto-chg evil-org dash-functional auctex shell-mode pdf-tools eshell yaml-mode latex company-irony irony company-quickhelp quelpa-use-package helm-projectile xclip use-package nlinum evil-commentary json-mode projectile evil-surround dtrt-indent 0blayout flycheck auto-org-md magit company-jedi yasnippet-classic-snippets alchemist elixir-mode helm-mode-manager seoul256-theme python-mode react-snippets helm yasnippet-snippets company slime evil))
 '(pdf-view-midnight-colors '("#b2b2b2" . "#262626"))
 '(safe-local-variable-values '((engine . php)))
 '(scroll-bar-mode nil)
 '(setq ansi-term-color-vector)
 '(tool-bar-mode nil)
 '(which-key-mode t)
 '(xterm-mouse-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
(put 'narrow-to-region 'disabled nil)
;;; init.el ends here
