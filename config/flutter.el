;;; package -- deviantfero's flutter.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; File to configure Flutter related packages
;;; features
;;; Code:

(use-package dart-mode
  :hook (dart-mode . flutter-test-mode))

(use-package flutter
  :after dart-mode
  :bind (:map dart-mode-map
			  ("C-M-x" . #'flutter-test-mode)
              ("C-M-r" . #'flutter-run-or-hot-reload)))

(use-package lsp-dart
  :hook (dart-mode . lsp))
  ;; (dap-register-debug-template "Flutter :: Custom debug"
  ;; 							   (list :flutterPlatform "x86_64"
  ;; 									 :program "lib/main_debug.dart"
  ;; 									 :args '("--flavor" "customer_a"))))

;;; flutter.el ends here
