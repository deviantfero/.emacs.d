;;; grammar-mode.el --- sample major mode for editing GRM files. -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2017, by deviantfero

;; Author: deviantfero
;; Version: 1.0.0
;; Created: 26 Jun 2015
;; Keywords: languages
;; Homepage: https://github.com/deviantfero/ucc

;; This file is not part of GNU Emacs.

;;; License:
;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.

;;; Commentary:
;; A small mode to highlight UCCs grammar syntax in EMACS

;;; Code:

(defvar grammar-mode-font-lock-defaults nil "Value for 'font-lock-defaults'.")
(defvar grammar-mode-syntax-table nil "Syntax table for grammar-mode.")

(setq grammar-mode-font-lock-defaults
	  '(("\\(^.*\\) ->" . (1 font-lock-function-name-face))
		("None\\|->\\||" . font-lock-keyword-face)
	  	))

(setq grammar-mode-syntax-table
	  (let ((syn-table (make-syntax-table)))
		(modify-syntax-entry ?# "<" syn-table)
		(modify-syntax-entry ?\n ">" syn-table)
		syn-table))

(define-derived-mode grammar-mode prog-mode "Grammar"
  (setq font-lock-defaults '(grammar-mode-font-lock-defaults))
  (set-syntax-table grammar-mode-syntax-table))

(provide 'grammar-mode)
;;; grammar-mode.el ends here
