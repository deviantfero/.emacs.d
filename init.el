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
(define-globalized-minor-mode global-outline-minor-mode
  outline-minor-mode outline-minor-mode)

(defun projectile-get-term ()
  "Get dedicated multi-term in project root."
  (interactive)
  (let ((projectile--proj-term-name
		 (concat "*" multi-term-buffer-name "[" projectile-project-name "]*")))
	(if (not (eq nil (get-buffer projectile--proj-term-name)))
		(switch-to-buffer projectile--proj-term-name)
	  (projectile-with-default-dir (projectile-project-root)
		(multi-term)
		(rename-buffer projectile--proj-term-name)))))

(defun sudo-write ()
  "Use TRAMP to open a file with write access using sudo."
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo:root@localhost:" (ido-read-file-name)))
    (write-file (concat "/sudo:root@localhost:" (buffer-file-name)))))

(defun my/multi-term ()
  "Opens a terminal in current projectile root or in current dir."
  (interactive)
  (if (projectile-project-p)
	(cd (projectile-project-root)))
  (multi-term))

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
  (global-outline-minor-mode 1)
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

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package multi-line
  :after evil
  :bind (:map global-map
			  ("C-c d" . multi-line)
			  :map evil-normal-state-map
			  ("M-j" . multi-line)))

(use-package projectile
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)
			  ("<f7>" . projectile-get-term))
  :config
  (projectile-mode))

(use-package magit
  :after evil
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

(use-package multi-term
  :config
  (setq multi-term-program "/bin/zsh")
  (setq multi-term-program-switches "--login"))

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
  (define-key evil-normal-state-map (kbd "<f7>") 'projectile-get-term)
  (define-key evil-normal-state-map (kbd "-") 'dired-jump)
  (define-key evil-normal-state-map (kbd "ga") 'align-regexp)
  (define-key evil-normal-state-map (kbd "gt") 'other-frame)
  (define-key evil-normal-state-map (kbd "C-w n") 'make-frame-command)
  (define-key evil-normal-state-map (kbd "C-w C-s") 'frameset-to-register)
  (define-key evil-normal-state-map (kbd "C-w C-r") 'jump-to-register)
  (define-key evil-normal-state-map (kbd "TAB") 'org-cycle)
  (define-key evil-normal-state-map (kbd "<backtab>") 'org-global-cycle)
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

(use-package evil-magit
  :after (evil magit))

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

(use-package restclient
  :mode (("\\.http\\'" . restclient-mode)
         ("\\.rest\\'" . restclient-mode))
  :config
  (setq outline-regexp "#[*\f]+"))

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
  (setq outline-regexp "[#\f]+")
  (define-key evil-normal-state-map (kbd "TAB") 'org-cycle))

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package yaml-mode)

;;; Misc
(use-package restclient
  :mode (("\\.http\\'" . restclient-mode)
         ("\\.rest\\'" . restclient-mode))
  :config
  (setq outline-regexp "#[*\f]+"))



(use-package company-restclient
  :hook
  (restclient-mode . (lambda () (add-to-list 'company-backends 'company-restclient))))

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

(use-package ox-pandoc
  :after org)


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
   '("0f9f1936eb2d50584d379b80a4ecaad6cdab7ccd0f3f71cb5abfddf9f38c4ddb" "541a353cc2219e2cd61650984a6428f7768d6c839dfc40bb98dc1d91ce9cc774" "1b80df94f27ba2a9ac8d00b2f2ba898005c4e563c222b6abaed00c914f89aa98" "18cd5a0173772cdaee5522b79c444acbc85f9a06055ec54bb91491173bc90aaa" "d9b89317cfdc13ef0000ab6f12736fb3752077f3aa1af5a8d2d1f605e89efa4f" "98dbb15efa9656c1042e641f43b4bf6f61e817b7c4f64e4dfc91bf3267fad766" "4850c9d8d2311dba5aed4bb4de3fd3560d93bea5f36aa7ef10a37099519e1189" "39e4b0903f633cd262c4f11c3efc3149dce99d37d258894e3cfb1b05d007c1e0" "a0278d2c62aede4e0a5a4c80a50bd104ed9c98f5232e1b7499348a8ccb01505f" "ed618350fc9404d3e4ec439c375c27500d228351ce23b65bf24b9a2a02a89a40" "e522d0e4bb4585b52bf5bcace03cf9eb0841022acbd1b4127851ab62b44db627" "dd68f800be522119f6e6a2eda244efd26f03d9a7aa10adf202e602612839b5e1" "7691c695463f0c0970325e258a8280e032b88fd9e3742da0b3a803d957415d30" "271f448eb77299ce5b14928ec365eaa15167bb036dae853bddc872876c82a743" "0a833d5c313e7bbfd14e6ddb4c1e94e7093bdbc88a8822365df683ebda34efca" "4dac83e929268295602b39f905283538935ae595207e29a293f2f7a4e5503eeb" "e4982755f7c49c5365b69c29cd4fbf0010fb89288ed1ecf4f4a2315d87c7f437" "2d9844d894c015c8fdb55d2816a572263ed11cca8d88130789bebcc48a321c47" "e423b60ad793c0f83a43b9d44bf95e9e47601399b79c8a42a3928cef97a12ab7" "8be034e6df3595310c54f23ebdd3d31bb8246a22fec3e2b3353a946c4f56691b" "c1c3d95abba38b98a7f31d9cbaf950881eed6205aef8e176bb7950def61cd6de" "7e13dae26544cdfb7f78f6a0e01a032c350b76d9846e7420e40e7f1a02d0ffd9" "4c9366a045fff3296b1b0565c181732bc9b103fe2a94dbe1f1159e6c4dfa9463" "15006b639ac371eaaf6fa63e59f0078c6ec2f989d5cd15a91de2dab2d1d9ebf4" "ab0e54d683d251ceee2b7c9de7cb486a960a29fb84b56acbff86a355c7d96ed8" "d6fc1ed94fb90614e90f4f1250705111a04f439c770288ea45420d9625f601b7" "6b86476efcda72a60779635b2865c49afc4ccccbb304bbbed78273d3d0ee3f96" "2b3ef163ca415977b240b10945df66b695cc5ccceae9d62b09aa6ce6c54b9fb8" "f13eab84067c004741b04b75496a8966003e651fa5a26db3c59b0ba5c0de7bf7" "cd6244b8c3be8c5c5aa722ebcc7e77862b46a02201b075b2e08d8d2cc634593d" "e71049fbd53cd366415ac4ba856fb3f16b2a2ecb44ab8379a75628d2478d1367" "33ac7a95b47b62dd8432fc506d5e14d68b79f3d05706f9afa4b67d52d8138715" "0855e1554e97ae5825c933e024f1753e3f8f776f0543a1f705bba2e2089bc8d2" "de637c659c9d9c267193c93cb25d6f77d4474a7789bc6275bf28cfce6c47060a" "6fd6ca49d4f59fe858093af21dc9920f905c731c9bb76d2ff86c55e402bd0005" "1546e95c27a1435db430f51e29b56e25dc7ac728e8d93a321b335e4a4b2a776b" "938450b22094ad5ce27fc0f34b02ce3b59ed657f465c2def69978de1d01441f9" "c95dcc93c2461f4797e6bd31c02a161f8873553bcc7ad1fb1c2d2d01e9fb9d17" "4f929481b1a1e2805f6b0605c2e6d5f738bfd641e87043c944004aef1dda452d" "8e6bed35c3b64caf840ee621ad3b67294682963b2bde64badd75a02a5a989b47" "020ba2575ec85c1959c2cfe9b56aec5b60d8be59e272e188f81b3547387d03d9" "49409afa74774e29b3f0d301538393ad1345f2fa1a8159474def9d0827cba70f" "afad584649462d8f2962f023b553e302576a944bfd56322204a7b35520b5e7f7" "0e537a9f625048f039177d31bb326b88f67a7c6ceff8462a0b5c820f6eff4351" "e452329d7c2bf500539e3b27bda350f208d17e81afdd56f29338ddc31b699cb3" "a157801211a196be85df0b7d5166f4146ff516927ac1580dfd5a37ff46f1e939" "9e6308cb388d0bcd7a6324c2e40f522808b7ef1f91bfded267260a062cac70a3" "14a4bbd2207617728ea504ea9aa48416999a456db9f10e7d74baab896301d8a3" "543810bda3d88d3172bea79fd4c1446a0f3f7bf2027fb3433283f00c1771b915" "433d465598b5e87b7be36a8b5abc426b056b6b2003e9730af9e4dd93f32a852d" "bb79e50c08c4892a8f4590bb7407de8cfab0dd58be50da6580decb9a28b40ce9" "0c4b0a603732e4fa03d08d44b779267a085161ec97924a0dcaa633484563923f" "1b21a3c27955fc9bcd6d0788fe229c6f33414adda263a15e9382595af4464666" "92b581ca44ae189ae308feb1387634e8798c8f893e0600fc191fe2ddbff0a709" "d3c2c0cdd152897c69597b802b053315531745a5de7415a84b70aca0acf5bd50" "28490858748e0d2ae3d87c8830ec06fc1c4b412939d43d8fe684201550c52741" "12f13ab77ae93adf97039bc8c3f5719eb77b9d9da916374b7c448f2bf176fba6" "7c3baae1d43dee474e1f542a96d2aa65b430338165d5dc5da2e8bffb9e053b4b" "af74d38925129f8be585f85bba5d18fa594ad8ca366a8c892dec6ea615d05be8" "e53f26bf4e436383b3f1015ce0fc73f6049e56594ba754f1fc249b5a24bec277" "c291db4bf2f189ef64314929fcf8df55b3d0b7d05892ba68650d7ae122e2e06d" "b33d7b8560d95aadd7143a06594a960888efff2c755d8fd02a2bd89e097ec81f" "f94b10ce2d675be8406b65d65a5f6e5998abe11a2d3bf41a23ca89e119069598" "6e3e4c96c9badf05266e793f3f33d22d8baeb01762303005c95e954424d3d3b1" "4c6daa4c9ad1239dff90e0d7a5c52ce1304c2a8217c1d89163f1f058537a2728" "ee999c5423403471d1cc197419cace81baa37f4c437fde5cdc68161455411322" "1135c68d913bc0c1c495d61e958782ebccbcfcfd25e9c56f768a739c1359013b" "700a92223ceb4af47e26adf31a2efdf1d282d77b3eadbf11cdbc1b23d9025405" "3d272799b73437e219fb840a7fb9df7df086a5e327694106a6866e6020737e22" "e0a1a9823d9ec86057e258d60c68218b0e6b31f4e197d56554140bdd5d9caa71" "50bda9f8157e84caa7a2edcfae789f090682113e919bd6790d02dc797427c9df" "3791210eaf10dd3cfb5a6fe595b7825750b7bc0ece71f15d8fdf49090c18aa7b" "b04e98efb80c08473b6687253a9f97669fbf0f63f34f2ad51cca77cc6a1756a0" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "d56ab147037efedd59e62451ca370b6d6573f10a07c0114d376d1a1a32563491" "696c7ad6e700206aa3d86a2555e90283f47c521d9188439804cf1a85a0aec758" "78d54007f52f745bb55e95f91390c4b947af59f777e1cd3cbe88204590ae25a0" "2c95cd2a529def31d6a67f87f4a03e967d330be6cb818ef94c068ff4708230db" "325c152851c1038848c4cb04dfa5184f46023d749aafaa7109b9289516831199" "c304e593666ca09232b1b4785876a03415efe834f7cc9af8f3afb6fd43c5ad33" "676fc0d75e7e9f54260c10e388cb925b4ce554fbc93b25da86ad6db47547bed7" "8acff68bb168fa0f5c3d09a4bf338ed8fec9ee464cc6e1cefef441a171a6381b" "6ca2c4437d37cea1cb151749c3585d1ea29fd483b831f7013beeac90081101c2" "07839198ca42eaaa51a28432cb8658ec5fbadbd8a6e6da231f116f7841fee0cb" "51517dac0b5f6b9839b8c2e01e07ada1c9c2440de21eb1fa534718f9aaf13f19" "4fdd3bae4ef37b5b67dccc91557bf069a8ba804723f49b462d69e5a64e298dee" "1caa46cdd3573072d3ac92a2c21474c2e1dad65de9b6d0c7ede64c79e9879f1a" "0d4da8e06207a8ac31c6e8335bcf1481f44f9eb30c065649ae8a9d46803f10ea" "b13ffeb4813ebc7b964dcc39b7c0d77704005acbaf1fcb4ca42e9bb942c82b7a" "ac2ce00fab91c5799e8289dc6be38696efcbf4489eab689e7debfc3e8e3ecc93" "bb6ce5371e43d4fefc4613af95fb2bdca1846c8475df1ee6e5fc520f930a2150"))
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
   '(helm-swoop vterm cider plan9-theme rvm gdscript-mode smartparens evil-vimish-fold cargo winner-mode lsp-ui lsp-mode lsp flycheck-rust graphql-mode multi-line helm-ag add-node-modules-path swiper dockerfile-mode ox-taskjuggler highlight-numbers powerline evil-visualstar elpy evil-matchit smart-parens go-mode web-mode html-mode company-web company-restclient dired dired-x minions moody which-key tide flycheck-irony space-line flycheck-pkg-config cmake-mode evil-magit async with-editor mmm-mode ssass-mode edit-indirect bind-key undo-tree tablist rich-minority s faceup quelpa dash f pythonic deferred python-environment epl pkg-info pos-tip popup markdown-mode magit-popup ghub git-commit json-snatcher json-reformat concurrent ctable epc jedi-core helm-core goto-chg evil-org dash-functional auctex shell-mode pdf-tools eshell yaml-mode latex restclient company-irony irony company-quickhelp quelpa-use-package helm-projectile xclip use-package nlinum evil-commentary json-mode projectile evil-surround dtrt-indent 0blayout flycheck auto-org-md magit company-jedi yasnippet-classic-snippets alchemist elixir-mode helm-mode-manager seoul256-theme python-mode react-snippets helm yasnippet-snippets company slime evil))
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
