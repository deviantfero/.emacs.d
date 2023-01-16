;;; package -- deviantfero's ivy.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; File to configure ivy related packages
;;; features
;;; Code:

(defun counsel-vterm-sources (str pred _)
  "Provides vterm buffers STR and PRED used by ivy."
  (mapcar #'buffer-name
          (cl-remove-if-not
           (lambda (buf)
             (with-current-buffer buf
               (eq major-mode 'vterm-mode)))
           (buffer-list))))

(defun counsel-vterm ()
  "Switch buffer to one terminal buffer."
  (interactive)
  (ivy-read "Switch to Buffer: "
            #'counsel-vterm-sources
            :action 'switch-to-buffer))

(use-package ivy
  :after evil
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (ivy-mode 1)
  :bind
  (:map evil-normal-state-map
        ("C-f" . counsel-find-file))
  (:map global-map
        ("M-x" . counsel-M-x)
        ("C-f" . counsel-find-file)
        ("C-x C-f" . counsel-find-file)
        ("C-x t" . counsel-vterm)
        ("C-g" . counsel-projectile-find-file)
        ("M-k" . counsel-projectile-rg)
        ("C-x B" . ivy-switch-buffer-other-window)))

(defun my-ivy-posframe-get-size ()
  "Set the ivy-posframe size according to the current frame."
  (let ((height (or ivy-posframe-height (or ivy-height 10)))
        (width (min (or ivy-posframe-width 200) (round (* .75 (frame-width))))))
    (list :height height :width width :min-height height :min-width width)))

(use-package ivy-posframe
  :after ivy
  :init
  (ivy-posframe-mode 1)
  :config
  (setq ivy-posframe-size-function 'my-ivy-posframe-get-size))

(use-package counsel
  :after ivy)

(use-package counsel-projectile
  :after ivy)

(use-package counsel-edit-mode
  :after ivy
  :config
  (counsel-edit-mode-setup-ivy))

(use-package ivy-rich
  :after ivy
  :custom
  (ivy-rich-mode 1)
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev))

  ;; (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  ;; (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  ;; (global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
  ;; (global-set-key (kbd "<f1> l") 'counsel-find-library)
  ;; (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  ;; (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  ;; (global-set-key (kbd "C-c g") 'counsel-git)
  ;; (global-set-key (kbd "C-c j") 'counsel-git-grep)
  ;; (global-set-key (kbd "C-c k") 'counsel-ag)
  ;; (global-set-key (kbd "C-x l") 'counsel-locate)
  ;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox))

;;; ivy.el ends here
