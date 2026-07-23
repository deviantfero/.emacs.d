;;; package -- deviantfero's org.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; File to configure org related packages
;;; Code:

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

(use-package org-transclusion
  :after org
  :bind (:map org-mode-map
              ("C-c t a" . org-transclusion-add)
              ("C-c t m" . org-transclusion-transient-menu)
              ("C-c t t" . org-transclusion-mode)))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;;; org.el ends here
