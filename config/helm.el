;;; package -- deviantfero's helm.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; File to configure HELM related packages
;;; features
;;; Code:

(defun my/helm-open-split (split-window-function)
  (require 'winner)
  (lambda (_candidate)
	(dolist (buf (helm-marked-candidates))
	  (select-window (funcall split-window-function))
	  (if (stringp buf)
		  (find-file buf)
		(switch-to-buffer buf)))
	(balance-windows)))

(defun helm-open-split-horizontal ()
  "Keybinded function to call horizontal split on helm."
  (interactive)
  (with-helm-alive-p
	(helm-quit-and-execute-action (my/helm-open-split 'split-window-below))))

(defun helm-open-split-vertical ()
  "Keybinded function to call vertical split on helm."
  (interactive)
  (with-helm-alive-p
	(helm-quit-and-execute-action (my/helm-open-split 'split-window-right))))

(defun helm-vterm-buffers-list ()
  (interactive)
  (require 'helm)
  (helm :sources helm-source-vterm-buffers-list
        :keymap helm-buffer-map
        :truncate-lines helm-buffers-truncate-lines))

(use-package helm
  :bind (:map evil-normal-state-map
			  ("M-k" . helm-do-ag)
			  ("C-f" . helm-find-files)
			  ("C-o" . helm-buffers-list)
			  :map helm-map
			  ("TAB" . helm-execute-persistent-action)
			  ("C-x v" . helm-open-split-vertical)
			  ("C-x x" . helm-open-split-horizontal)
			  :map global-map
			  ("M-x" . helm-M-x)
			  ("C-x t" . helm-vterm-buffers-list))
  :config
  (helm-mode 1)
  (setq helm-source-vterm-buffers-list
		(helm-make-source "Vterm Buffers" 'helm-source-buffers
		  :buffer-list
		  (lambda ()
			(mapcar #'buffer-name
					(cl-remove-if-not
					 (lambda (buf)
					   (with-current-buffer buf
						 (eq major-mode 'vterm-mode)))
					 (buffer-list))))))
  (setq helm-completion-style 'emacs
		helm-split-window-inside-p nil
		helm-display-header-line nil
		helm-display-buffer-reuse-frame t
		helm-full-frame nil
		helm-split-window-default-side 'same))

(use-package helm-ag
  :after evil)

(use-package helm-swoop
  :after evil)

(use-package helm-projectile
  :after evil
  :bind (:map evil-normal-state-map
			  ("C-g" . helm-projectile))
  :config
  (helm-projectile-on))

;;; helm.el ends here
