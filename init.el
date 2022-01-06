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

(load-directory "~/.emacs.d/config")

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
					  '("\\.[tj]sx?\\'" "\\.vue\\'" "\\.ejs\\'")))
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

(use-package add-node-modules-path)
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

(use-package prettier
  :hook (web-mode . my/activate-prettier-mode))

(use-package company-web
  :hook
  (mhtml-mode . (lambda () (add-to-list 'company-backends 'company-web-html))))

;;; GoLang
(use-package go-mode
  :mode ("\\.go\\'")
  :hook ((go-mode . lsp))
  :config
  (setq lsp-gopls-codelens nil)
  (setq compile-command "echo Building... && go build -v && echo Testing... && go test -v && echo Linter... && golint")
  (setq compilation-read-command nil))

;;; Java
(use-package lsp-java
  :hook (java-mode . lsp))

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

;;; Ruby
(use-package projectile-rails
  :hook (ruby-mode . projectile-rails-mode)
  :config (define-key projectile-rails-mode-map (kbd "C-c r") 'projectile-rails-command-map))

(use-package ruby-mode
  :ensure nil
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
  (setq elpy-modules
		(seq-difference
		 elpy-modules '(elpy-module-flymake elpy-module-highlight-indentation))))

;;; Clojure
(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
		 ("\\.edn\\'" . clojure-mode)))

(use-package cider
  :hook (clojure-mode . cider-mode))

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
  (company-tng-mode)
  :bind (:map evil-insert-state-map
			  ("C-f" . company-files))
  :config
  (setq company-dabbrev-other-buffers nil)
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
  :ensure nil
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
 '(custom-safe-themes 'nil)
 '(flycheck-javascript-flow-args nil)
 '(font-lock-maximum-decoration '((t . t) (dired-mode)))
 '(global-evil-mc-mode t)
 '(global-origami-mode t)
 '(helm-completion-style 'emacs)
 '(helm-minibuffer-history-key "M-p")
 '(magit-diff-section-arguments '("--no-ext-diff"))
 '(org-agenda-files '("/home/fernando/org/todo.org"))
 '(org-latex-compiler "xelatex")
 '(package-selected-packages
   '(## csv-mode lsp-java dap-java evil-leader evil-collection meson-mode prettier origami-mode helm-swoop vterm cider plan9-theme rvm gdscript-mode smartparens cargo winner-mode lsp-ui lsp-mode lsp flycheck-rust graphql-mode multi-line helm-ag add-node-modules-path dockerfile-mode ox-taskjuggler powerline evil-visualstar elpy evil-matchit smart-parens go-mode web-mode html-mode company-web dired dired-x minions moody which-key tide flycheck-irony space-line flycheck-pkg-config cmake-mode evil-magit async with-editor mmm-mode ssass-mode edit-indirect bind-key undo-tree tablist rich-minority s faceup quelpa dash f pythonic deferred python-environment epl pkg-info pos-tip popup markdown-mode magit-popup ghub git-commit json-snatcher json-reformat concurrent ctable epc jedi-core helm-core goto-chg evil-org dash-functional auctex shell-mode pdf-tools eshell latex company-irony irony company-quickhelp quelpa-use-package helm-projectile xclip use-package nlinum evil-commentary json-mode projectile evil-surround dtrt-indent 0blayout flycheck auto-org-md magit company-jedi yasnippet-classic-snippets helm-mode-manager seoul256-theme python-mode react-snippets yasnippet-snippets company slime evil))
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
