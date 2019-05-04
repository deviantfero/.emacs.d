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


;;; BASIC
(use-package highlight-numbers
  :ensure t
  :hook (prog-mode . highlight-numbers-mode))

(use-package projectile
  :ensure t
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :config
  (projectile-mode))

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

;;; STATUS LINE
(use-package minions
  :ensure t
  :config (minions-mode 1))

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
   'org-babel-load-languages '((shell . t)
                               (python . t)
                               (ruby . t)
                               (C . t)
                               (haskell . t)))
  (add-to-list 'org-export-backends 'taskjuggler))

(use-package org-bullets
  :ensure t
  :after org
  :config (setq org-bullets-bullet-list '("*" "○" "◆" "•"))
  :hook (org-mode . org-bullets-mode))


;;; TRAMP
(use-package docker-tramp
  :ensure t)

(use-package tramp
  :defer 5
  :config
  (with-eval-after-load 'tramp-cache
    (setq tramp-persistency-file-name "~/.emacs.d/tramp"))
  (setenv "SHELL" "/bin/bash")
  (setq-default projectile-mode-line "Projectile")
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
  (setq helm-split-window-default-side 'same)
  (setq helm-full-frame nil)
  (setq helm-completion-in-region-fuzzy-match t))

(use-package helm-ag
  :ensure t)

(use-package helm-projectile
  :after evil
  :ensure t
  :bind (:map evil-normal-state-map
              ("C-g" . helm-projectile))
  :config
  (helm-projectile-on))

;;; LANGUAGES.
(use-package web-mode
  :ensure t
  :mode ("\\.ejs\\'" "\\.html\\'" "\\.[jt]sx?\\'")
  :config
  (setq web-mode-content-types-alist '(("jsx" . "\\.[jt]sx?\\'")))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-script-padding 2)
  (setq web-mode-block-padding 2)
  (setq web-mode-style-padding 2))
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-current-element-highlight t)

(use-package js2-mode
  :ensure t
  :init
  (setq js2-mode-show-strict-warnings nil)
  (setq js2-mode-show-parse-errors 1))

(use-package flow-minor-mode
  :ensure t
  :hook (web-mode . flow-minor-enable-automatically))

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

(use-package dockerfile-mode
  :ensure t
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package sh-script
  :config
  (setq outline-regexp "[#\f]+"))

(use-package yaml-mode
  :ensure t)

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
  :hook (web-mode . my/activate-tide-mode)
  :ensure t)

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
 '(ansi-color-names-vector
   ["#080808" "#d70000" "#67b11d" "#875f00" "#268bd2" "#af00df" "#00ffff" "#b2b2b2"])
 '(custom-safe-themes
   (quote
    ("14a4bbd2207617728ea504ea9aa48416999a456db9f10e7d74baab896301d8a3" "543810bda3d88d3172bea79fd4c1446a0f3f7bf2027fb3433283f00c1771b915" "433d465598b5e87b7be36a8b5abc426b056b6b2003e9730af9e4dd93f32a852d" "bb79e50c08c4892a8f4590bb7407de8cfab0dd58be50da6580decb9a28b40ce9" "0c4b0a603732e4fa03d08d44b779267a085161ec97924a0dcaa633484563923f" "1b21a3c27955fc9bcd6d0788fe229c6f33414adda263a15e9382595af4464666" "92b581ca44ae189ae308feb1387634e8798c8f893e0600fc191fe2ddbff0a709" "d3c2c0cdd152897c69597b802b053315531745a5de7415a84b70aca0acf5bd50" "28490858748e0d2ae3d87c8830ec06fc1c4b412939d43d8fe684201550c52741" "12f13ab77ae93adf97039bc8c3f5719eb77b9d9da916374b7c448f2bf176fba6" "7c3baae1d43dee474e1f542a96d2aa65b430338165d5dc5da2e8bffb9e053b4b" "af74d38925129f8be585f85bba5d18fa594ad8ca366a8c892dec6ea615d05be8" "e53f26bf4e436383b3f1015ce0fc73f6049e56594ba754f1fc249b5a24bec277" "c291db4bf2f189ef64314929fcf8df55b3d0b7d05892ba68650d7ae122e2e06d" "b33d7b8560d95aadd7143a06594a960888efff2c755d8fd02a2bd89e097ec81f" "f94b10ce2d675be8406b65d65a5f6e5998abe11a2d3bf41a23ca89e119069598" "6e3e4c96c9badf05266e793f3f33d22d8baeb01762303005c95e954424d3d3b1" "4c6daa4c9ad1239dff90e0d7a5c52ce1304c2a8217c1d89163f1f058537a2728" "ee999c5423403471d1cc197419cace81baa37f4c437fde5cdc68161455411322" "1135c68d913bc0c1c495d61e958782ebccbcfcfd25e9c56f768a739c1359013b" "700a92223ceb4af47e26adf31a2efdf1d282d77b3eadbf11cdbc1b23d9025405" "3d272799b73437e219fb840a7fb9df7df086a5e327694106a6866e6020737e22" "e0a1a9823d9ec86057e258d60c68218b0e6b31f4e197d56554140bdd5d9caa71" "50bda9f8157e84caa7a2edcfae789f090682113e919bd6790d02dc797427c9df" "3791210eaf10dd3cfb5a6fe595b7825750b7bc0ece71f15d8fdf49090c18aa7b" "b04e98efb80c08473b6687253a9f97669fbf0f63f34f2ad51cca77cc6a1756a0" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "d56ab147037efedd59e62451ca370b6d6573f10a07c0114d376d1a1a32563491" "696c7ad6e700206aa3d86a2555e90283f47c521d9188439804cf1a85a0aec758" "78d54007f52f745bb55e95f91390c4b947af59f777e1cd3cbe88204590ae25a0" "2c95cd2a529def31d6a67f87f4a03e967d330be6cb818ef94c068ff4708230db" "325c152851c1038848c4cb04dfa5184f46023d749aafaa7109b9289516831199" "c304e593666ca09232b1b4785876a03415efe834f7cc9af8f3afb6fd43c5ad33" "676fc0d75e7e9f54260c10e388cb925b4ce554fbc93b25da86ad6db47547bed7" "8acff68bb168fa0f5c3d09a4bf338ed8fec9ee464cc6e1cefef441a171a6381b" "6ca2c4437d37cea1cb151749c3585d1ea29fd483b831f7013beeac90081101c2" "07839198ca42eaaa51a28432cb8658ec5fbadbd8a6e6da231f116f7841fee0cb" "51517dac0b5f6b9839b8c2e01e07ada1c9c2440de21eb1fa534718f9aaf13f19" "4fdd3bae4ef37b5b67dccc91557bf069a8ba804723f49b462d69e5a64e298dee" "1caa46cdd3573072d3ac92a2c21474c2e1dad65de9b6d0c7ede64c79e9879f1a" "0d4da8e06207a8ac31c6e8335bcf1481f44f9eb30c065649ae8a9d46803f10ea" "b13ffeb4813ebc7b964dcc39b7c0d77704005acbaf1fcb4ca42e9bb942c82b7a" "ac2ce00fab91c5799e8289dc6be38696efcbf4489eab689e7debfc3e8e3ecc93" "bb6ce5371e43d4fefc4613af95fb2bdca1846c8475df1ee6e5fc520f930a2150")))
 '(electric-pair-mode t)
 '(evil-collection-setup-minibuffer t)
 '(flycheck-javascript-flow-args nil)
 '(global-evil-mc-mode t)
 '(global-origami-mode t)
 '(hl-todo-keyword-faces
   (quote
    (("TODO" . "#dc752f")
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
     ("???" . "#dc752f"))))
 '(magit-diff-section-arguments (quote ("--no-ext-diff")))
 '(org-agenda-files (quote ("/home/fernando/org/todo.org")))
 '(package-selected-packages
   (quote
    (flycheck-flow dockerfile-mode ox-taskjuggler highlight-numbers powerline evil-visualstar elpy evil-matchit smart-parens elixir-yasnippets go-mode web-mode haml-mode flow-minor-mode haskell-mode indium html-mode company-web company-restclient dired dired-x minions moody which-key org-bullets tide flycheck-irony space-line flycheck-pkg-config cmake-mode evil-magit yasnippet async with-editor mmm-mode ssass-mode edit-indirect bind-key undo-tree tablist rich-minority s faceup quelpa dash f pythonic deferred python-environment epl pkg-info pos-tip popup markdown-mode magit-popup ghub git-commit json-snatcher json-reformat concurrent ctable epc jedi-core helm-core goto-chg evil-org dash-functional auctex shell-mode pdf-tools eshell yaml-mode latex restclient company-irony irony company-quickhelp quelpa-use-package helm-projectile xclip use-package nlinum evil-commentary json-mode projectile helm-ag evil-surround dtrt-indent 0blayout flycheck auto-org-md magit company-jedi yasnippet-classic-snippets alchemist elixir-mode helm-mode-manager company-go seoul256-theme python-mode react-snippets helm yasnippet-snippets company slime evil)))
 '(pdf-view-midnight-colors (quote ("#b2b2b2" . "#262626")))
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
