;;; package -- deviantfero's tramp.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; File to configure TRAMP packages related
;;; features
;;; Code:

(use-package tramp
  :ensure nil
  :defer 5
  :config
  (with-eval-after-load 'tramp-cache
	(setq tramp-persistency-file-name "~/.emacs.d/tramp"))
  (setq-default projectile-mode-line "Projectile")
  (setq tramp-use-ssh-controlmaster-options nil)
  (setf tramp-persistency-file-name
		(concat temporary-file-directory "tramp-" (user-login-name))))

;;; tramp.el ends here
