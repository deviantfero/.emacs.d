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
  (require 'use-package))

(define-globalized-minor-mode global-outline-minor-mode
  outline-minor-mode outline-minor-mode)

(column-number-mode 1)
(show-paren-mode 1)
(menu-bar-mode -1)
(toggle-tool-bar-mode-from-frame -1)
(winner-mode 1)
(global-outline-minor-mode 1)
(add-hook 'focus-out-hook #'garbage-collect)

(setq-default
 backup-directory-alist '(("." . "~/.emacs.d/saves"))
 backup-by-copying t
 auto-save-default nil
 create-lockfiles nil
 inhibit-startup-message t
 tab-width 4
 blink-matching-paren nil
 toggle-scroll-bar nil
 ispell-program-name (executable-find "hunspell")
 ispell-dictionary "es_ES")

(setq-default ispell-local-dictionary-alist
      '(("es_ES" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)
        ("en_EN" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)))
(dolist (basic-offset '(c-basic-offset sh-basic-offset))
  (defvaralias basic-offset 'tab-width))


(load-theme 'wpgtk t)

;;; FUNCTION DEFINITIONS.
(defun my/use-eslint-from-node-modules ()
  "Use local eslint from node_modules before global."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(defun my/activate-tide-mode ()
  "Use hl-identifier-mode only on js or ts buffers."
  (when (and (stringp buffer-file-name)
             (string-match "\\.[tj]sx?\\'" buffer-file-name))
    (tide-setup)
    (tide-hl-identifier-mode)))

(defun sudo-write ()
  "Use TRAMP to open a file with write access using sudo."
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo:root@localhost:" (ido-read-file-name)))
    (write-file (concat "/sudo:root@localhost:" (buffer-file-name)))))

(defun line-number-toggle ()
  "Toggle line numbers based on Emacs version."
  (interactive)
  (if (version<= "26.0.50" emacs-version)
      (display-line-numbers-mode)
    (nlinum-mode)))

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

(define-globalized-minor-mode global-outline-minor-mode
  outline-minor-mode outline-minor-mode)

;;; ORG MODE
(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c a" . org-agenda))
  :hook (org-mode . visual-line-mode)
  :config
  (setq org-agenda-files
        (directory-files-recursively "~/org/" "\.org$"))
  (org-babel-do-load-languages
   'org-babel-load-languages '((python . t)
                               (ruby . t)
                               (js . t)
                               (C . t)
                               (haskell . t))))

(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode))

;;; STATUS LINE
(use-package minions
  :ensure t
  :config (minions-mode 1))

;;; HELM
(use-package helm
  :after evil
  :ensure t
  :bind (:map evil-normal-state-map
              ("M-k" . helm-do-ag)
              ("C-f" . helm-find-files)
              ("C-o" . helm-buffers-list)
              :map helm-map
              ("TAB" . helm-execute-persistent-action)
              :map global-map
              ("M-x" . helm-M-x))
  :config
  (helm-mode 1)
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t))

(use-package helm-ag
  :ensure t)

(use-package helm-projectile
  :ensure t
  :after projectile)

;;; BASIC
(use-package highlight-numbers
  :ensure t
  :config (highlight-numbers-mode 1))

(use-package projectile
  :after (evil helm)
  :ensure t
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)
              :map evil-normal-state-map
              ("C-g" . helm-projectile))
  :config
  (projectile-mode +1))

(use-package magit
  :after evil
  :ensure t
  :bind (:map evil-normal-state-map
              ("<f9>" . magit-status)))

(use-package nlinum
  :ensure t
  :hook
  (prog-mode . line-number-toggle)
  (prog-mode . xterm-mouse-mode)
  :config
  (setq nlinum-format "%3d  "))

(use-package dtrt-indent
  :ensure t
  :config (dtrt-indent-global-mode 1))

(use-package xclip
  :ensure t
  :config (xclip-mode 1))

(use-package multi-term
  :ensure t
  :config
  (setq multi-term-program "/bin/zsh"))

(use-package yasnippet
  :ensure t
  :config (yas-global-mode 1)
  :bind (:map yas-keymap
              ("TAB" . nil)
              ("C-o" . yas-next-field-or-maybe-expand)))

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

(use-package tramp
  :defer 5
  :config
  (with-eval-after-load 'tramp-cache
    (setq tramp-persistency-file-name "~/.emacs.d/tramp"))
  (setenv "SHELL" "/bin/bash")
  (setq projectile-mode-line "Projectile")
  (setq tramp-use-ssh-controlmaster-options nil)
  (setf tramp-persistency-file-name
        (concat temporary-file-directory "tramp-" (user-login-name))))

;;; EVIL
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :ensure t
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "<f5>") 'compile)
  (define-key evil-normal-state-map (kbd "<f7>") 'my/multi-term)
  (define-key evil-normal-state-map (kbd "-") 'dired-jump)
  (define-key evil-normal-state-map (kbd "ga") 'align-regexp)
  (define-key evil-normal-state-map (kbd "gt") 'other-frame)
  (define-key evil-normal-state-map (kbd "C-w n") 'make-frame-command)
  (define-key evil-normal-state-map (kbd "C-w u") 'winner-undo)
  (define-key evil-normal-state-map (kbd "C-w C-r") 'winner-redo)
  (define-key evil-normal-state-map (kbd "TAB") 'org-cycle)
  (define-key evil-normal-state-map (kbd "<backtab>") 'org-global-cycle)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down))

(use-package evil-collection
  :ensure t
  :custom (evil-collection-setup-minibuffer t)
  :init (evil-collection-init))

(use-package evil-org
  :ensure t
  :hook
  (org-mode . evil-org-mode)
  (evil-org-mode . (lambda () (evil-org-set-key-theme)))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-magit
  :after (evil magit)
  :ensure t)

(use-package evil-surround
  :after evil
  :ensure t
  :config (global-evil-surround-mode 1))

(use-package evil-commentary
  :after evil
  :ensure t
  :config (evil-commentary-mode 1))

(use-package evil-matchit
  :ensure t
  :config (global-evil-matchit-mode 1))

(use-package evil-visualstar
  :ensure t
  :config (global-evil-visualstar-mode))

;;; LANGUAGES.
(use-package web-mode
  :ensure t
  :mode (("\\.ejs\\'" . web-mode)
         ("\\.json\\'" . web-mode))
  :config
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-script-padding 2)
  (setq web-mode-block-padding 2)
  (setq web-mode-style-padding 2))

(use-package js2-mode
  :ensure t
  :init
  (setq js2-mode-show-strict-warnings nil)
  (setq js2-mode-show-parse-errors 1))

(use-package flow-minor-mode
  :ensure t
  :hook (js-mode . flow-minor-enable-automatically))

(use-package flow-js2-mode
  :load-path "./modes/"
  :hook (flow-minor-mode . flow-js2-mode))

(use-package elpy
  :ensure t
  :hook ((python-mode . elpy-mode)
         (python-mode . flycheck-mode)))

(use-package restclient
  :ensure t
  :mode (("\\.http\\'" . restclient-mode)
         ("\\.rest\\'" . restclient-mode))
  :config
  (setq outline-regexp "#[*\f]+"))

(use-package go-mode
  :ensure t
  :mode ("\\.go\\'"))

(use-package haskell-mode
  :ensure t)

(use-package elixir-mode
  :ensure t)

(use-package cmake-mode
  :ensure t)

(use-package rjsx-mode
  :after js2-mode
  :mode "\\.js\\'"
  :interpreter "node"
  :ensure t)

(use-package indium
  :ensure t
  :config
  (setq indium-chrome-executable "google-chrome-stable"))

(use-package haml-mode
  :ensure t
  :mode "\\.haml\\'")

(use-package irony
  :ensure t
  :hook
  (c++-mode . irony-mode)
  (c-mode . irony-mode)
  (objc-mode . irony-mode))

(use-package conf-mode
  :ensure t
  :mode (("\\.env\\'" . conf-mode))
  :config
  (setq outline-regexp "[#\f]+")
  (define-key evil-normal-state-map (kbd "TAB") 'org-cycle))

(use-package sh-script
  :config
  (setq outline-regexp "[#\f]+"))

(use-package yaml-mode
  :ensure t)

(use-package grammar-mode
  :load-path "./modes/"
  :mode "\\.grm\\'" )

;;; COMPANY
(use-package company
  :ensure t
  :after evil
  :init
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (company-tng-configure-default)
  :bind (:map evil-insert-state-map
              ("C-y" . company-yasnippet)
              ("C-f" . company-files)
              ("C-M-y" . yas-expand))
  :config
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-code-everywhere t)
  (global-company-mode 1))

(use-package tide
  :ensure t
  :hook ((js-mode . tide-setup)
         (js-mode . tide-hl-identifier-mode)
         (js-mode . my/use-eslint-from-node-modules)
         (js2-mode . tide-setup)
         (js2-mode . tide-hl-identifier-mode)
         (js2-mode . my/use-eslint-from-node-modules)))

(use-package company-irony
  :ensure t
  :after irony
  :hook
  (irony-mode . (lambda () (add-to-list 'company-backends 'company-irony)))
  (irony-mode . irony-cdb-autosetup-compile-options))

(use-package alchemist
  :ensure t
  :hook
  (elixir-mode . (lambda () (add-to-list 'company-backends 'alchemist-company))))

(use-package company-web
  :ensure t
  :hook
  (mhtml-mode . (lambda () (add-to-list 'company-backends 'company-web-html))))

(use-package company-restclient
  :ensure t
  :hook
  (restclient-mode . (lambda () (add-to-list 'company-backends 'company-restclient))))

(use-package company-go
  :ensure t
  :hook
  (go-mode . (lambda () (add-to-list 'company-backends 'company-go))))


;;; FLYCHECK
(use-package flycheck
  :ensure t
  :init
  (setq flycheck-python-flake8-executable (executable-find "flake8"))
  (setq flycheck-python-pycompile-executable (executable-find "python3"))
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (global-flycheck-mode 1))

(use-package flycheck-pkg-config
  :after (flycheck)
  :ensure t)

(use-package flycheck-irony
  :after (flycheck company-irony)
  :ensure t
  :config
  (flycheck-irony-setup))

(use-package flycheck-flow
  :after (flow-minor-mode)
  :ensure t)

;;; NOTE-TAKING
(use-package pdf-tools
  :ensure t)

(use-package markdown-mode
  :ensure t
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
  :after org
  :ensure t)


;;; AUTOMATICALLY GENERATED
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-engine (quote xetex))
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (wpgtk)))
 '(custom-safe-themes
   (quote
    ("6ca2c4437d37cea1cb151749c3585d1ea29fd483b831f7013beeac90081101c2" "07839198ca42eaaa51a28432cb8658ec5fbadbd8a6e6da231f116f7841fee0cb" "51517dac0b5f6b9839b8c2e01e07ada1c9c2440de21eb1fa534718f9aaf13f19" "4fdd3bae4ef37b5b67dccc91557bf069a8ba804723f49b462d69e5a64e298dee" "1caa46cdd3573072d3ac92a2c21474c2e1dad65de9b6d0c7ede64c79e9879f1a" "0d4da8e06207a8ac31c6e8335bcf1481f44f9eb30c065649ae8a9d46803f10ea" "b13ffeb4813ebc7b964dcc39b7c0d77704005acbaf1fcb4ca42e9bb942c82b7a" "ac2ce00fab91c5799e8289dc6be38696efcbf4489eab689e7debfc3e8e3ecc93" "bb6ce5371e43d4fefc4613af95fb2bdca1846c8475df1ee6e5fc520f930a2150")))
 '(electric-pair-mode t)
 '(evil-collection-setup-minibuffer t)
 '(flycheck-javascript-flow-args nil)
 '(global-evil-mc-mode t)
 '(global-origami-mode t)
 '(magit-diff-section-arguments (quote ("--no-ext-diff")))
 '(org-agenda-files
   (quote
    ("/home/fernando/org/reggie.org" "/home/fernando/org/todo.org")))
 '(package-selected-packages
   (quote
    (highlight-numbers powerline evil-visualstar elpy flycheck-flow evil-matchit smart-parens elixir-yasnippets go-mode web-mode haml-mode flow-minor-mode intero haskell-mode indium html-mode company-web company-restclient dired dired-x minions moody which-key org-bullets tide flycheck-irony space-line flycheck-pkg-config cmake-mode evil-magit yasnippet async with-editor mmm-mode ssass-mode edit-indirect bind-key undo-tree tablist rich-minority s faceup quelpa dash f pythonic deferred python-environment epl pkg-info pos-tip popup markdown-mode magit-popup ghub git-commit json-snatcher json-reformat concurrent ctable epc jedi-core helm-core goto-chg evil-org dash-functional auctex shell-mode pdf-tools eshell yaml-mode latex restclient company-irony irony company-quickhelp quelpa-use-package helm-projectile xclip use-package nlinum evil-commentary json-mode rjsx-mode projectile helm-ag evil-surround dtrt-indent 0blayout flycheck auto-org-md magit js2-mode company-jedi yasnippet-classic-snippets alchemist elixir-mode helm-mode-manager company-go seoul256-theme python-mode react-snippets helm yasnippet-snippets company slime evil)))
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

;;; init.el ends here
