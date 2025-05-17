;;; package -- deviantfero's .init.el -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(require 'package)
(require 'mouse)
(require 'dired-x)
(require 'find-lisp)

(defun load-directory (directory)
  "Load every elisp file under the indicated DIRECTORY."
  (mapcar
   (lambda (fn) (load (file-name-sans-extension fn)))
   (find-lisp-find-files directory "\\.el\\'")))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'term-file-aliases '("alacritty" . "xterm"))

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))

(dolist (basic-offset '(c-basic-offset sh-basic-offset))
  (defvaralias basic-offset 'tab-width))

;;; Feature Packages
;;; YASnippet
(use-package yasnippet
  :after evil company
  :config
  (yas-global-mode t)
  :bind (:map yas-keymap
			  ("TAB" . nil)
			  ("C-o" . yas-next-field-or-maybe-expand)
			  :map evil-insert-state-map
			  ("C-y" . company-yasnippet)
			  ("C-M-y" . yas-expand)))

(use-package yasnippet-classic-snippets)

;;; Javascript/HTML/REST
(defun my/activate-tide-mode ()
  "Use hl-identifier-mode only on set file extensions."
  (when (and (stringp buffer-file-name)
			 (cl-some (lambda (re) (string-match-p re buffer-file-name))
					  '("\\.m?[tj]sx?\\'" "\\.vue\\'" "\\.ejs\\'")))
	(tide-setup)
	(tide-hl-identifier-mode)))

(defun my/set-local-eslint ()
  "Use local node_modules."
  (add-node-modules-path)
  (setq-local flycheck-javascript-eslint-executable (executable-find "eslint")))

(use-package add-node-modules-path
  :custom (add-node-modules-path-command '("yarn bin" "npm bin" "pnpm bin")))

(use-package web-mode
  :mode
  ("\\.ejs\\'" "\\.vue\\'" "\\.erb\\'" "\\.hbs\\'" "\\.html\\'" "\\.php\\'" "\\.m?[jt]sx?\\'")
  :config
  (setq web-mode-content-types-alist '(("jsx" . "\\.m?[jt]sx?\\'")))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-script-padding 2)
  (setq web-mode-block-padding 2)
  (setq web-mode-style-padding 2)
  (setq web-mode-enable-auto-pairing nil)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-enable-current-element-highlight t))

(use-package tide
  :config
  (flycheck-define-generic-checker 'tsx-ts-tide
	"A TSX syntax checker using tsserver."
	:start #'tide-flycheck-start
	:verify #'tide-flycheck-verify
	:modes '(web-mode typescript-mode)
	:predicate (lambda ()
				 (and
				  (or (tide-file-extension-p "tsx") (tide-file-extension-p "ts"))
				  (tide-flycheck-predicate))))
  (add-to-list 'flycheck-checkers 'tsx-ts-tide t)
  (flycheck-add-next-checker 'tsx-tide 'javascript-eslint)
  (flycheck-add-next-checker 'tsx-ts-tide 'javascript-eslint)
  :hook (web-mode . my/activate-tide-mode))

(use-package apheleia
  :hook ((prog-mode . apheleia-mode))
  :commands (apheleia-mode apheleia-global-mode)
  :config
  (setf (alist-get 'ruby-mode apheleia-mode-alist)
		'(rubocop))
  (apheleia-global-mode))

(use-package scss-mode
  :config
  (setq scss-compile-at-save nil))

;;; GoLang
(use-package go-mode
  :mode ("\\.go\\'")
  :hook ((go-mode . lsp))
  :config
  (setq lsp-gopls-codelens nil)
  (setq compile-command "echo Building... && go build -v && echo Testing... && go test -v && echo Linter... && golint")
  (setq compilation-read-command nil))

;;; GdScript
(use-package gdscript-mode
  :config (setq warning-minimum-level :emergency)
  :hook (gdscript-mode . lsp))

;;; Elixir
(use-package elixir-mode
  :init (add-to-list 'exec-path "~/.emacs.d/elixir")
  :hook (elixir-mode . lsp))

;;; C++/C
(use-package cmake-mode)
(use-package meson-mode)

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

;;; graphql
(use-package graphql-mode)

;;; Clojure
(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
		 ("\\.edn\\'" . clojure-mode)))

;;; Conf-files
(use-package conf-mode
  :ensure nil
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
										; disable icons from autocomplete
  (setq company-format-margin-function 'company-text-icons-margin)
  (company-tng-mode)
  :bind (:map evil-insert-state-map
			  ("C-f" . company-files))
  :config
  (setq company-dabbrev-other-buffers nil)
  (setq company-dabbrev-code-other-buffers t)
  (setq company-backends
		'((company-capf :with company-dabbrev-code :separate)
		  (company-files :with company-dabbrev-code)
		  (company-etags :with company-dabbrev-code :separate)
		  (company-oddmuse :with company-yasnippet)
		  (company-dabbrev :with company-yasnippet)))
  (global-company-mode 1))

;;; Org
(use-package org
  :ensure nil
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c a" . org-agenda))
  :hook (org-mode . visual-line-mode)
  :config
  (setq org-image-actual-width nil)
  (setq org-agenda-files
		(directory-files-recursively "~/org/" "\.org$"))
  (org-babel-do-load-languages
   'org-babel-load-languages '((shell . t)
							   (python . t)
							   (ruby . t)
							   (C . t)
							   (dot . t)))
  (add-to-list 'org-export-backends 'taskjuggler))

(use-package org-tree-slide)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;;; Note taking
(use-package pdf-tools)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
		 ("\\.md\\'" . markdown-mode)
		 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc")
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

(use-package graphviz-dot-mode
  :config
  (setq graphviz-dot-indent-width 4))

(use-package csv-mode)

(load-directory "~/.emacs.d/config")

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
   ["#080808" "#d70000" "#67b11d" "#875f00" "#268bd2" "#af00df" "#00ffff"
	"#b2b2b2"])
 '(auth-source-save-behavior nil)
 '(connection-local-criteria-alist
   '(((:application tramp :protocol "kubernetes")
	  tramp-kubernetes-connection-local-default-profile)
	 ((:application eshell) eshell-connection-default-profile)
	 ((:application tramp :protocol "flatpak")
	  tramp-container-connection-local-default-flatpak-profile)
	 ((:application tramp)
	  tramp-connection-local-default-system-profile
	  tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((tramp-kubernetes-connection-local-default-profile
	  (tramp-config-check . tramp-kubernetes--current-context-data)
	  (tramp-extra-expand-args 97
							   (tramp-kubernetes--container
								(car tramp-current-connection))
							   104
							   (tramp-kubernetes--pod
								(car tramp-current-connection))
							   120
							   (tramp-kubernetes--context-namespace
								(car tramp-current-connection))))
	 (eshell-connection-default-profile (eshell-path-env-list))
	 (tramp-container-connection-local-default-flatpak-profile
	  (tramp-remote-path "/app/bin" tramp-default-remote-path "/bin"
						 "/usr/bin" "/sbin" "/usr/sbin"
						 "/usr/local/bin" "/usr/local/sbin"
						 "/local/bin" "/local/freeware/bin"
						 "/local/gnu/bin" "/usr/freeware/bin"
						 "/usr/pkg/bin" "/usr/contrib/bin" "/opt/bin"
						 "/opt/sbin" "/opt/local/bin"))
	 (tramp-connection-local-darwin-ps-profile
	  (tramp-process-attributes-ps-args "-acxww" "-o"
										"pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
										"-o" "state=abcde" "-o"
										"ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
	  (tramp-process-attributes-ps-format (pid . number)
										  (euid . number)
										  (user . string)
										  (egid . number) (comm . 52)
										  (state . 5) (ppid . number)
										  (pgrp . number)
										  (sess . number)
										  (ttname . string)
										  (tpgid . number)
										  (minflt . number)
										  (majflt . number)
										  (time . tramp-ps-time)
										  (pri . number)
										  (nice . number)
										  (vsize . number)
										  (rss . number)
										  (etime . tramp-ps-time)
										  (pcpu . number)
										  (pmem . number) (args)))
	 (tramp-connection-local-busybox-ps-profile
	  (tramp-process-attributes-ps-args "-o"
										"pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
										"-o" "stat=abcde" "-o"
										"ppid,pgid,tty,time,nice,etime,args")
	  (tramp-process-attributes-ps-format (pid . number)
										  (user . string)
										  (group . string) (comm . 52)
										  (state . 5) (ppid . number)
										  (pgrp . number)
										  (ttname . string)
										  (time . tramp-ps-time)
										  (nice . number)
										  (etime . tramp-ps-time)
										  (args)))
	 (tramp-connection-local-bsd-ps-profile
	  (tramp-process-attributes-ps-args "-acxww" "-o"
										"pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
										"-o"
										"state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
	  (tramp-process-attributes-ps-format (pid . number)
										  (euid . number)
										  (user . string)
										  (egid . number)
										  (group . string) (comm . 52)
										  (state . string)
										  (ppid . number)
										  (pgrp . number)
										  (sess . number)
										  (ttname . string)
										  (tpgid . number)
										  (minflt . number)
										  (majflt . number)
										  (time . tramp-ps-time)
										  (pri . number)
										  (nice . number)
										  (vsize . number)
										  (rss . number)
										  (etime . number)
										  (pcpu . number)
										  (pmem . number) (args)))
	 (tramp-connection-local-default-shell-profile
	  (shell-file-name . "/bin/sh") (shell-command-switch . "-c"))
	 (tramp-connection-local-default-system-profile
	  (path-separator . ":") (null-device . "/dev/null"))))
 '(custom-safe-themes
   '("b935a5b122b72942cede089a90421b5c8c114d792bcda28e9b33a2714e5ea93e"
	 "85d0f4bcc143d99685f42d5a21b1a13d12746b630f20f827d29a6fbc84b84611"
	 "d89eaa6474421baa7ee18da6e591d2b716eea127a8433606c5683f5b92f04583"
	 "9477e358369287961a7bbeb5236054975c0e05891b05248379d894dd4c4e2043"
	 "80a3b75a81b19a9797bab8986972c1f95465191a286531a62f1609a3a2383dca"
	 "39d495721f3ffafff7b0bac9ce314e0909087c08e6c79aee7d7e2f03afcb29c3"
	 "07a39404844003374ec125c42facb1bf65455c389dabc7712c5239a8285e8160"
	 "efc4e242d1263009eb40a431400a7db64f4f0c14e5b6c1eb19e71d0fa541b045"
	 "6ab4eb7465301cb8caed5f5a6463defb6a67be1e68281bb41fbca9e36cb89aa5"
	 "4e07b00d71423594cc850d298336b6b9f3c23b0f00bf9ef5c090664d0ffd2a1a"
	 "0c9a774781dad152c7574a71686a4efacb56567d15c71b1ae38574c1b43dc387"
	 "dde643b0efb339c0de5645a2bc2e8b4176976d5298065b8e6ca45bc4ddf188b7"
	 "fae90032fa5ec1d5423d44b78b2f1d8003f6b3b96171be36c2316b025671ea15"
	 "f9a104eb43e8649e4aeda32c178153ba2e13e8e77fe4ba4fd8cd4599fb37de93"
	 "3325b6788863fcbc18df1a488ec91ab583068995350eec4948e3bfe956c28315"
	 "c1d54a6776fd6a0bcf46ae7822311badde4c58f312658299bede827a2976f730"
	 "acc27d1aa54e609757b096d1a47610b8562e04e301f60398371816aecb886fde"
	 "941e5a94256bc2291a5c34e85a68fe8c59bac76d70d719b8b0aa6aa9b35f2c97"
	 "befcd5b52d43a8f16a84d8b28a3e9f0ec972ca1369c9db949c81b956184976b5"
	 "cc830a772fbd6766fc45bb1b7abea0e73a44d2d774dd5288edc6cccffc7ae593"))
 '(flycheck-javascript-flow-args nil)
 '(font-lock-maximum-decoration '((t . t) (dired-mode)))
 '(global-evil-mc-mode t)
 '(global-origami-mode t)
 '(helm-minibuffer-history-key "M-p")
 '(magit-diff-section-arguments '("--no-ext-diff"))
 '(org-agenda-files '("/home/fernando/org/todo.org"))
 '(org-latex-compiler "xelatex")
 '(package-selected-packages
   '(add-node-modules-path anaconda-mode apheleia auctex cargo
						   clojure-mode cmake-mode company-irony
						   counsel-edit-mode counsel-projectile
						   csv-mode dockerfile-mode dtrt-indent
						   dumb-jump edit-indirect editorconfig
						   elixir-mode eterm-256color evil-collection
						   evil-commentary evil-leader evil-matchit
						   evil-org evil-surround evil-visualstar
						   exec-path-from-shell flutter flycheck-irony
						   flycheck-pkg-config flycheck-rust
						   gdscript-mode go-mode gptel graphql-mode
						   graphviz-dot-mode iter2 ivy-rich lsp-dart
						   lsp-ui magit meson-mode minions multi-line
						   nvm org-bullets org-tree-slide ox-pandoc
						   pdf-tools projectile-rails python-black
						   python-mode pyvenv rainbow-mode rubocop
						   rust-mode rvm scss-mode smartparens
						   string-inflection tide tree-sitter-langs
						   treesit-auto undo-tree vterm web-mode
						   which-key xclip yaml-mode
						   yasnippet-classic-snippets))
 '(pdf-view-midnight-colors '("#b2b2b2" . "#262626"))
 '(safe-local-variable-values '((engine . php)))
 '(scroll-bar-mode nil)
 '(setq ansi-term-color-vector)
 '(tool-bar-mode nil)
 '(warning-suppress-types '((use-package) (emacs) (emacs) (emacs)))
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
