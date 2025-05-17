(toggle-tool-bar-mode-from-frame -1)
(add-to-list 'default-frame-alist '(font . "iosevka 10"))
(add-to-list 'default-frame-alist '(internal-border-width . 8))
(menu-bar-mode -1)

(setq-default toggle-scroll-bar nil)
(setq pgtk-use-im-context-on-new-connection nil)

;; Performance optimizations
(setq-default
 gc-cons-threshold 100000000
 read-process-output-max (* 1024 1024)
 native-comp-async-report-warnings-errors nil
 package-native-compile t)

(setq-default inhibit-startup-message t)

(if (daemonp)
    (add-hook 'after-make-frame-functions
			  (lambda (frame)
				(with-selected-frame frame
				  (load-theme 'wpgtk t)))))
