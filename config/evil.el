;;; package -- deviantfero's evil.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; File to configure EVIL packages related
;;; features
;;; Code:
(defun my/toggle-window-size-fixed ()
  "Toggle the `window-size-locked' variable between nil and t."
  (interactive)
  (setq window-size-fixed (if window-size-fixed nil t))
  (message "Window size is now %s" (if window-size-fixed "fixed" "flexible")))

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
  :custom
  (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

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
  :after evil
  :config (global-evil-matchit-mode 1))

(use-package evil-visualstar
  :after evil
  :config (global-evil-visualstar-mode))

(use-package evil-leader
  :config
  (global-evil-leader-mode)
  (setq evil-leader/in-all-states t)
  (evil-leader/set-key
    "b" 'switch-to-buffer
    "r" 'reload-wpgtk
    "k" 'kill-buffer
    "p" 'projectile-switch-project
    "'" 'counsel-evil-marks
    "l" 'magit-log-buffer-file
    "gt" 'other-frame
    "we" 'web-mode-set-engine
    "ws" 'evil-window-split
    "wv" 'evil-window-vsplit
    "wl" 'my/toggle-window-size-fixed
    "s" 'string-inflection-cycle))

;;; evil.el ends here
