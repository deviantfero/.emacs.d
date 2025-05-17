;;; package -- deviantfero's utils.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; File to define custom utility functions that affect EMACS globaly
;;; Code:
(defun sudo-write ()
  "Use TRAMP to open a file with write access using sudo."
  (interactive)
  (if (not buffer-file-name)
	  (write-file (concat "/sudo:root@localhost:" (ido-read-file-name)))
	(write-file (concat "/sudo:root@localhost:" (buffer-file-name)))))

(defun reload-wpgtk ()
  "Reload current theme."
  (interactive)
  (load-theme 'wpgtk t))

(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
	  (setq deactivate-mark  t)
	(when (get-buffer "*Completions*")
	  (delete-windows-on "*Completions*")) (abort-recursive-edit)))
;;; utils.el ends here
