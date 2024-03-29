;;; wpgtk-theme.el ---  Dynamic color theme, specially made for wpgtk

;; based on: <https://github.com/warreq/xres-theme>
;;
;; Version: 0.1
;; Keywords: color, theme
;; Package-Requires: ((emacs "24"))

;; Initially with the help of emacs-theme-generator, <https://github.com/mswift42/theme-creator>.
;; Modified directly from Nasser Alshammari's spacemacs theme

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of Emacs.

;;; Code:

(defgroup wpgtk-theme nil
  "Xres-theme options."
  :group 'faces)

(defcustom wpgtk-theme-comment-bg nil
  "Use a background for comment lines."
  :type 'boolean
  :group 'wpgtk-theme)

(defcustom wpgtk-theme-org-height t
  "Use varying text heights for org headings."
  :type 'boolean
  :group 'wpgtk-theme)

(defun ui-or-true-color ()
  (or (display-graphic-p)
	  (> (tty-display-color-cells) 256)))

(defun get-hex-or-term (n)
  "Gets N hex or a term color depending on whether we're using an GUI or not."
  (cond ((eq n 0)  (if (ui-or-true-color) "#241f0f"  "black"))
        ((eq n 1)  (if (ui-or-true-color) "#A5503C"  "red"))
        ((eq n 2)  (if (ui-or-true-color) "#7B7C4C"  "green"))
        ((eq n 3)  (if (ui-or-true-color) "#DAA961"  "yellow"))
        ((eq n 4)  (if (ui-or-true-color) "#594443"  "blue"))
        ((eq n 5)  (if (ui-or-true-color) "#CBAB88"  "magenta"))
        ((eq n 6)  (if (ui-or-true-color) "#A05F50"  "cyan"))
        ((eq n 7)  (if (ui-or-true-color) "#f5f0c2"  "white"))
        ((eq n 8)  (if (ui-or-true-color) "#453b1c"  "brightblack"))
        ((eq n 9)  (if (ui-or-true-color) "#e55e3f"  "brightred"))
        ((eq n 10) (if (ui-or-true-color) "#acae55" "brightgreen"))
        ((eq n 11) (if (ui-or-true-color) "#ffdf69" "brightyellow"))
        ((eq n 12) (if (ui-or-true-color) "#7d4f4d" "brightblue"))
        ((eq n 13) (if (ui-or-true-color) "#ffdf9a" "brightmagenta"))
        ((eq n 14) (if (ui-or-true-color) "#df7158" "brightcyan"))
        ((eq n 15) (if (ui-or-true-color) "#ffffe1" "brightwhite"))
		((eq n 16) (if (ui-or-true-color) "#898b44" "brightcyan"))
		((eq n 17) (if (ui-or-true-color) "#5e5f2e" "cyan"))))

(defun create-wpgtk-theme (variant theme-name)
  (let ((class '((class color) (min-colors 16)))
        (base            (get-hex-or-term 15))
        (white           (get-hex-or-term 7))
        (cursor          (get-hex-or-term 7))
        (bg1             (get-hex-or-term 0))
        (bg2             (get-hex-or-term 8))
        (bg3             (get-hex-or-term 8))
        (bg4             (get-hex-or-term 8))
        (key1            (get-hex-or-term 14))
        (key2            (get-hex-or-term 14))
        (builtin         (get-hex-or-term 13))
        (keyword         (get-hex-or-term 12))
        (const           (get-hex-or-term 14))
        (comment         (get-hex-or-term 3))
        (comment-bg      (get-hex-or-term 0))
        (func            (get-hex-or-term 13))
        (str             (get-hex-or-term 11))
        (type            (get-hex-or-term 14))
        (comp            (get-hex-or-term 13))
        (var             (get-hex-or-term 10))
        (err             (get-hex-or-term 9))
        (war             (get-hex-or-term 11))
        (inf             (get-hex-or-term 11))
        (suc             (get-hex-or-term 10))
        (green           (get-hex-or-term 10))
        (yellow          (get-hex-or-term 11))
        (cyan            (get-hex-or-term 14))
        (violet          (get-hex-or-term 13))
        (red             (get-hex-or-term 9))
        (active1         (get-hex-or-term 14))
        (active2         (get-hex-or-term 6))
        (inactive        (get-hex-or-term 8))
        (m-line-brdr     (get-hex-or-term 8))
        (org-block-bg    (get-hex-or-term 8))
        (org-h1-bg       (get-hex-or-term 8))
        (org-h2-bg       (get-hex-or-term 0))
        (org-h3-bg       (get-hex-or-term 0))
        (org-h4-bg       (get-hex-or-term 0))
        (highlight       (get-hex-or-term 14)))

    (custom-theme-set-faces
     theme-name

;;;;; basics
     `(cursor ((,class (:background ,cursor))))
     `(default ((,class (:background ,bg1 :foreground ,base))))
     `(default-italic ((,class (:italic t))))
     `(error ((,class (:foreground ,err))))
     `(eval-sexp-fu-flash ((,class (:background ,suc :foreground ,bg1))))
     `(eval-sexp-fu-flash-error ((,class (:background ,err :foreground ,bg1))))
     `(font-lock-builtin-face ((,class (:foreground ,builtin))))
     `(font-lock-comment-face ((,class (:foreground ,comment :italic t))))
     `(font-lock-constant-face ((,class (:foreground ,const))))
     `(font-lock-doc-face ((,class (:foreground ,comment))))
     `(font-lock-function-name-face ((,class (:foreground ,func :bold t))))
     `(font-lock-keyword-face ((,class (:bold ,class :foreground ,keyword))))
     `(font-lock-negation-char-face ((,class (:foreground ,const))))
     `(font-lock-preprocessor-face ((,class (:foreground ,func))))
     `(font-lock-reference-face ((,class (:foreground ,const))))
     `(font-lock-string-face ((,class (:foreground ,str))))
     `(font-lock-type-face ((,class (:foreground ,type :bold t))))
     `(font-lock-variable-name-face ((,class (:foreground ,var))))
     `(font-lock-warning-face ((,class (:foreground ,war :background ,bg1))))
     `(fringe ((,class (:background ,bg2 :foreground ,base))))
     `(highlight ((,class (:foreground ,base :background ,bg3))))
	 `(highlight-indentation-face ((,class (:background ,bg2))))
     `(hl-line ((,class (:background ,bg2))))
     `(isearch ((,class (:bold t :foreground ,bg1 :background ,inf))))
     `(lazy-highlight ((,class (:foreground ,bg1 :background ,inf :weight normal))))
     `(link ((,class (:foreground ,comment :underline t))))
     `(link-visited ((,class (:foreground ,comp :underline t))))
     `(match ((,class (:background ,bg1 :foreground ,inf :weight bold))))
     `(minibuffer-prompt ((,class (:bold t :foreground ,keyword))))
     `(page-break-lines ((,class (:foreground ,active2))))
     `(region ((,class (:background ,highlight :foreground ,bg1))))
     `(secondary-selection ((,class (:background ,bg3))))
     `(show-paren-match ((,class (:background ,active2))))
     `(success ((,class (:foreground ,suc))))
     `(vertical-border ((,class (:foreground ,bg1 :background, bg2))))
     `(warning ((,class (:foreground ,war ))))

;;;;; anzu-mode
     `(anzu-mode-line ((,class (:foreground ,yellow :weight bold))))

;;;;; company
     `(company-echo-common ((,class (:background ,base :foreground ,bg1))))
     `(company-preview ((,class (:background ,bg1 :foreground ,key1))))
     `(company-preview-common ((,class (:background ,bg2 :foreground ,keyword))))
     `(company-preview-search ((,class (:background ,bg2 :foreground ,green))))
     `(company-scrollbar-bg ((,class (:background ,bg2))))
     `(company-scrollbar-fg ((,class (:background ,comp))))
     `(company-template-field ((,class (:inherit region))))
     `(company-tooltip ((,class (:background ,bg2 :foreground ,base))))
     `(company-tooltip-annotation ((,class (:background ,bg2 :foreground ,active1))))
     `(company-tooltip-common ((,class (:background ,active2 :foreground ,bg1))))
     `(company-tooltip-common-selection ((,class (:foreground ,bg1))))
     `(company-tooltip-mouse ((,class (:inherit highlight))))
     `(company-tooltip-search ((,class (:inherit match))))
     `(company-tooltip-selection ((,class (:background ,active1 :foreground, bg1))))

;;;;; diff
     `(diff-added             ((,class :background nil :foreground ,green)))
     `(diff-changed           ((,class :background nil :foreground ,inf)))
     `(diff-indicator-added   ((,class :background nil :foreground ,green)))
     `(diff-indicator-changed ((,class :background nil :foreground ,inf)))
     `(diff-indicator-removed ((,class :background nil :foreground ,red)))
     `(diff-refine-added      ((,class :background ,green :foreground ,bg4)))
     `(diff-refine-changed    ((,class :background ,inf :foreground ,bg4)))
     `(diff-refine-removed    ((,class :background ,red :foreground ,bg4)))
     `(diff-removed           ((,class :background nil :foreground ,red)))

;;;;; dired
     `(dired-directory ((,class (:foreground ,key1 :background ,bg1 :weight bold))))
     `(dired-flagged ((,class (:foreground ,red))))
     `(dired-header ((,class (:foreground ,comp :weight bold))))
     `(dired-ignored ((,class (:inherit shadow))))
     `(dired-mark ((,class (:foreground ,comp :weight bold))))
     `(dired-marked ((,class (:foreground ,violet :weight bold))))
     `(dired-perm-write ((,class (:foreground ,base :underline t))))
     `(dired-symlink ((,class (:foreground ,cyan :background ,bg1 :weight bold))))
     `(dired-warning ((,class (:foreground ,war))))

;;;;; ediff
     `(ediff-current-diff-A ((,class(:background ,org-h1-bg :foreground ,inf))))
     `(ediff-current-diff-Ancestor ((,class(:background ,org-h2-bg :foreground ,str))))
     `(ediff-current-diff-B ((,class(:background ,org-h4-bg :foreground ,yellow))))
     `(ediff-current-diff-C ((,class(:background ,org-h3-bg :foreground ,green))))
     `(ediff-even-diff-A ((,class(:background ,bg3))))
     `(ediff-even-diff-Ancestor ((,class(:background ,bg3))))
     `(ediff-even-diff-B ((,class(:background ,bg3))))
     `(ediff-even-diff-C ((,class(:background ,bg3))))
     `(ediff-fine-diff-A ((,class(:background nil :bold t :underline t))))
     `(ediff-fine-diff-Ancestor ((,class(:background nil :bold t :underline t))))
     `(ediff-fine-diff-B ((,class(:background nil :bold t :underline t))))
     `(ediff-fine-diff-C ((,class(:background nil :bold t :underline t))))
     `(ediff-odd-diff-A ((,class(:background ,bg4))))
     `(ediff-odd-diff-Ancestor ((,class(:background ,bg4))))
     `(ediff-odd-diff-B ((,class(:background ,bg4))))
     `(ediff-odd-diff-C ((,class(:background ,bg4))))

;;;;; ein
     `(ein:cell-input-area((,class (:background ,bg2))))
     `(ein:cell-input-prompt ((,class (:foreground ,(if (eq variant 'dark) suc green)))))
     `(ein:cell-output-prompt ((,class (:foreground ,err))))
     `(ein:notification-tab-normal ((,class (:foreground ,builtin))))
     `(ein:notification-tab-selected ((,class (:foreground ,(if (eq variant 'dark) suc green) :bold t))))

;;;;; eldoc
     `(eldoc-highlight-function-argument ((,class (:foreground ,(if (eq variant 'dark) suc red) :bold t))))

;;;;; erc
     `(erc-input-face ((,class (:foreground ,func))))
     `(erc-my-nick-face ((,class (:foreground ,key1))))
     `(erc-nick-default-face ((,class (:foreground ,inf))))
     `(erc-nick-prefix-face ((,class (:foreground ,yellow))))
     `(erc-notice-face ((,class (:foreground ,str))))
     `(erc-prompt-face ((,class (:foreground ,(if (eq variant 'dark) suc green) :bold t))))
     `(erc-timestamp-face ((,class (:foreground ,builtin))))

;;;;; eshell
     `(eshell-ls-archive ((,class (:foreground ,red :weight bold))))
     `(eshell-ls-backup ((,class (:inherit font-lock-comment-face))))
     `(eshell-ls-clutter ((,class (:inherit font-lock-comment-face))))
     `(eshell-ls-directory ((,class (:foreground ,inf :weight bold))))
     `(eshell-ls-executable ((,class (:foreground ,suc :weight bold))))
     `(eshell-ls-missing ((,class (:inherit font-lock-warning-face))))
     `(eshell-ls-product ((,class (:inherit font-lock-doc-face))))
     `(eshell-ls-special ((,class (:foreground ,yellow :weight bold))))
     `(eshell-ls-symlink ((,class (:foreground ,cyan :weight bold))))
     `(eshell-ls-unreadable ((,class (:foreground ,base))))
     `(eshell-prompt ((,class (:foreground ,keyword :weight bold))))

;;;;; flycheck/flymake
     `(flycheck-error ((,class (:foreground ,bg1 :background ,err))))
     `(flycheck-error-list-checker-name ((,class (:foreground ,keyword))))
     `(flycheck-fringe-error ((,class (:foreground ,err :weight bold))))
     `(flycheck-fringe-info ((,class (:foreground ,inf :weight bold))))
     `(flycheck-fringe-warning ((,class (:foreground ,war :weight bold))))
     `(flycheck-info ((,class (:foreground ,bg1 :background ,yellow :bold t :underline t))))
     `(flycheck-warning ((,class (:foreground ,bg1 :background ,violet))))
	 `(flymake-warning ((,class (:inherit flycheck-warning))))
	 `(flymake-error ((,class (:inherit flycheck-error))))

;;;;; git-gutter-fr
     `(git-gutter-fr:added ((,class (:foreground ,green :weight bold))))
     `(git-gutter-fr:deleted ((,class (:foreground ,war :weight bold))))
     `(git-gutter-fr:modified ((,class (:foreground ,inf :weight bold))))

;;;;; git-timemachine
     `(git-timemachine-minibuffer-detail-face ((,class (:foreground ,inf :bold t :background ,org-h1-bg))))

;;;;; gnus
     `(gnus-emphasis-highlight-words ((,class (:background ,(if (eq variant 'dark) err suc) :foreground ,(when (eq variant 'light) bg1)))))
     `(gnus-header-content ((,class (:foreground ,keyword))))
     `(gnus-header-from ((,class (:foreground ,var))))
     `(gnus-header-name ((,class (:foreground ,comp))))
     `(gnus-header-subject ((,class (:foreground ,func :bold t))))
     `(gnus-summary-cancelled ((,class (:background ,(if (eq variant 'dark) err suc) :foreground ,bg1))))

;;;;; guide-key
     `(guide-key/highlight-command-face ((,class (:foreground ,base))))
     `(guide-key/key-face ((,class (:foreground ,key1))))
     `(guide-key/prefix-command-face ((,class (:foreground ,key2 :weight bold))))

;;;;; helm
     `(helm-bookmark-directory ((,class (:inherit helm-ff-directory))))
     `(helm-bookmark-file ((,class (:foreground ,base))))
     `(helm-bookmark-gnus ((,class (:foreground ,comp))))
     `(helm-bookmark-info ((,class (:foreground ,comp))))
     `(helm-bookmark-man ((,class (:foreground ,comp))))
     `(helm-bookmark-w3m ((,class (:foreground ,comp))))
     `(helm-buffer-directory ((,class (:foreground ,base :background ,bg1))))
     `(helm-buffer-file ((,class (:foreground ,base :background ,bg1))))
     `(helm-buffer-not-saved ((,class (:foreground ,comp :background ,bg1))))
     `(helm-buffer-process ((,class (:foreground ,builtin :background ,bg1))))
     `(helm-buffer-saved-out ((,class (:foreground ,base :background ,bg1))))
     `(helm-buffer-size ((,class (:foreground ,base :background ,bg1))))
     `(helm-candidate-number ((,class (:background ,bg1 :foreground ,inf :bold t))))
     `(helm-ff-directory ((,class (:foreground ,key1 :background ,bg1 :weight bold))))
     `(helm-ff-dotted-directory ((,class (:foreground ,key1 :background ,bg1 :weight bold))))
     `(helm-ff-executable ((,class (:foreground ,suc :background ,bg1 :weight normal))))
     `(helm-ff-file ((,class (:foreground ,base :background ,bg1 :weight normal))))
     `(helm-ff-invalid-symlink ((,class (:foreground ,red :background ,bg1 :weight bold))))
     `(helm-ff-prefix ((,class (:foreground ,bg1 :background ,keyword :weight normal))))
     `(helm-ff-symlink ((,class (:foreground ,cyan :background ,bg1 :weight bold))))
     `(helm-grep-cmd-line ((,class (:foreground ,base :background ,bg1))))
     `(helm-grep-file ((,class (:foreground ,base :background ,bg1))))
     `(helm-grep-finish ((,class (:foreground ,base :background ,bg1))))
     `(helm-grep-lineno ((,class (:foreground ,base :background ,bg1))))
     `(helm-grep-match ((,class (:foreground nil :background nil :inherit helm-match))))
     `(helm-grep-running ((,class (:foreground ,func :background ,bg1))))
     `(helm-header ((,class (:foreground ,base :background ,bg1 :underline nil :box nil))))
     `(helm-header-line-left-margin ((,class (:foreground ,inf :background ,nil))))
     `(helm-match ((,class (:inherit match))))
     `(helm-match-item ((,class (:inherit match))))
     `(helm-moccur-buffer ((,class (:foreground ,func :background ,bg1))))
     `(helm-selection ((,class (:background ,highlight :foreground, bg1 :extend t))))
     `(helm-selection-line ((,class (:background ,bg2))))
     `(helm-M-x-key ((,class (:foreground ,func))))
     `(helm-separator ((,class (:foreground ,comp :background ,bg1))))
     `(helm-source-header ((,class (:background ,comp :foreground ,bg1 :bold t :extend t))))
     `(helm-time-zone-current ((,class (:foreground ,builtin :background ,bg1))))
     `(helm-time-zone-home ((,class (:foreground ,comp :background ,bg1))))
     `(helm-visible-mark ((,class (:foreground ,bg1 :background ,bg3))))

;;;;; helm-swoop
     `(helm-swoop-target-line-block-face ((,class (:foreground ,base :background ,highlight))))
     `(helm-swoop-target-line-face ((,class (:foreground ,base :background ,highlight))))
     `(helm-swoop-target-word-face ((,class (:foreground ,bg1 :background ,suc))))

;;;;; ivy
     `(ivy-current-match ((,class (:foreground ,bg1 :background ,highlight))))

;;;;; ido
     `(ido-first-match ((,class (:foreground ,comp :bold t))))
     `(ido-only-match ((,class (:foreground ,(if (eq variant 'dark) suc red) :bold t))))
     `(ido-subdir ((,class (:foreground ,key1))))
     `(ido-vertical-match-face ((,class (:foreground ,comp :underline nil))))

;;;;; info
     `(info-header-xref ((,class (:foreground ,func :underline t))))
     `(info-menu ((,class (:foreground ,suc))))
     `(info-node ((,class (:foreground ,func :bold t))))
     `(info-quoted-name ((,class (:foreground ,builtin))))
     `(info-reference-item ((,class (:background nil :underline t :bold t))))
     `(info-string ((,class (:foreground ,str))))
     `(info-title-1 ((,class (:height 1.4 :bold t))))
     `(info-title-2 ((,class (:height 1.3 :bold t))))
     `(info-title-3 ((,class (:height 1.3))))
     `(info-title-4 ((,class (:height 1.2))))

;;;;; linum-mode
     `(linum ((,class (:foreground ,base :background ,bg2))))
     `(nlinum ((,class (:foreground ,base :background ,bg2))))
     `(line-number ((,class (:foreground ,base :background ,bg2))))

;;;;; magit
     `(magit-tag ((,class :background nil :foreground ,yellow)))
     `(magit-blame-culprit ((,class :background ,org-h4-bg :foreground ,yellow)))
     `(magit-blame-header  ((,class :background ,org-h4-bg :foreground ,green)))
     `(magit-blame-sha1    ((,class :background ,org-h4-bg :foreground ,func)))
     `(magit-blame-subject ((,class :background ,org-h4-bg :foreground ,yellow)))
     `(magit-blame-time    ((,class :background ,org-h4-bg :foreground ,green)))
     `(magit-blame-name    ((,class :background ,org-h4-bg :foreground ,yellow)))
     `(magit-blame-heading ((,class :background ,org-h4-bg :foreground ,green)))
     `(magit-blame-hash    ((,class :background ,org-h4-bg :foreground ,func)))
     `(magit-blame-summary ((,class :background ,org-h4-bg :foreground ,yellow)))
     `(magit-blame-date    ((,class :background ,org-h4-bg :foreground ,green)))
	 `(magit-branch-local    ((,class :background nil :foreground , func)))
	 `(magit-branch-remote    ((,class :background nil :foreground ,green)))
	 `(magit-branch ((,class (:foreground ,const :weight bold))))
     `(magit-diff-context ((,class (:background nil :foreground ,base))))
     `(magit-diff-context-highlight ((,class (:background ,bg3 :foreground ,base))))
     `(magit-diff-file-header ((,class (:background nil :foreground ,str))))
     `(magit-diff-hunk-header ((,class (:background nil :foreground ,builtin))))
     `(magit-hash ((,class (:foreground ,base))))
     `(magit-hunk-heading           ((,class (:background ,bg3))))
     `(magit-hunk-heading-highlight ((,class (:background ,bg3))))
     `(magit-diff-hunk-heading           ((,class (:foreground ,active1))))
     `(magit-diff-hunk-heading-highlight ((,class (:background ,bg3 :foreground ,active1))))
     `(magit-item-highlight ((,class :background ,bg2)))
     `(magit-log-author ((,class (:foreground ,base))))
     `(magit-log-head-label-head ((,class (:background ,yellow :foreground ,bg1 :bold t))))
     `(magit-log-head-label-local ((,class (:background ,inf :foreground ,bg1 :bold t))))
     `(magit-log-head-label-remote ((,class (:background ,suc :foreground ,bg1 :bold t))))
     `(magit-log-head-label-tags ((,class (:background ,violet :foreground ,bg1 :bold t))))
     `(magit-log-head-label-wip ((,class (:background ,cyan :foreground ,bg1 :bold t))))
     `(magit-log-sha1 ((,class (:foreground ,str))))
     `(magit-process-ng ((,class (:foreground ,war :weight bold))))
     `(magit-process-ok ((,class (:foreground ,func :weight bold))))
     `(magit-section-heading        ((,class (:foreground ,keyword :weight bold))))
     `(magit-section-highlight      ((,class (:background ,bg2))))
     `(magit-section-title ((,class (:background ,bg1 :foreground ,builtin :weight bold))))
     `(magit-header-line ((,class (:background ,active1 :foreground ,bg1 :weight bold))))

;;;;; header-line
     `(header-line           ((,class (:foreground ,white :background ,bg1))))
     `(header-line-highlight ((,class (:foreground ,bg1 :background ,active1))))

;;;;; mode-line
     `(mode-line           ((,class (:foreground ,bg1 :background ,active1 :box (:color ,m-line-brdr :line-width 0)))))
     `(mode-line-inactive  ((,class (:foreground ,white :background ,bg2   :box (:color ,m-line-brdr :line-width 0)))))

;;;;; mode-line
     `(sml/modified    ((,class (:foreground ,bg1 :background ,red))))

;;;;; neotree
     `(neo-dir-link-face ((,class (:foreground ,inf :weight bold))))
     `(neo-expand-btn-face ((,class (:foreground ,base))))
     `(neo-file-link-face ((,class (:foreground ,base))))
     `(neo-root-dir-face ((,class (:foreground ,func :weight bold))))

;;;;; org
     `(org-agenda-clocking ((,class (:foreground ,comp))))
     `(org-agenda-date ((,class (:foreground ,var :height 1.1))))
     `(org-agenda-date-today ((,class (:weight bold :foreground ,keyword :height 1.3))))
     `(org-agenda-date-weekend ((,class (:weight normal :foreground ,base))))
     `(org-agenda-done ((,class (:foreground ,(if (eq variant 'dark) suc green) :bold t))))
     `(org-agenda-structure ((,class (:weight bold :foreground ,comp))))
     `(org-block ((,class (:foreground ,base))))
     `(org-block-begin-line ((,class (:foreground ,func :background ,org-h1-bg))))
     `(org-block-end-line ((,class (:inherit org-block-begin-line))))
     `(org-meta-line ((,class (:inherit org-block-begin-line))))
     `(org-clock-overlay ((,class (:foreground ,comp))))
     `(org-code ((,class (:foreground ,cyan))))
     `(org-column ((,class (:background ,bg2))))
     `(org-column-title ((,class (:background ,highlight))))
     `(org-date ((,class (:underline t :foreground ,var) )))
     `(org-date-selected ((,class (:background ,func :foreground ,bg1) )))
     `(org-document-info-keyword ((,class (:foreground ,str))))
     `(org-document-title ((,class (:foreground ,func :weight bold :height ,(if wpgtk-theme-org-height 1.4 1.0) :underline t))))
     `(org-done ((,class (:foreground ,(if (eq variant 'dark) suc green) :bold t :background ,org-h3-bg))))
     `(org-ellipsis ((,class (:foreground ,builtin))))
     `(org-footnote  ((,class (:underline t :foreground ,base))))
     `(org-hide ((,class (:foreground ,base))))
     `(org-level-1 ((,class (:bold t :foreground ,inf :height ,(if wpgtk-theme-org-height 1.3 1.0) :background ,org-h1-bg))))
     `(org-level-2 ((,class (:bold t :foreground ,white :height ,(if wpgtk-theme-org-height 1.2 1.0) :background ,org-h2-bg))))
     `(org-level-3 ((,class (:bold nil :foreground ,green :height ,(if wpgtk-theme-org-height 1.1 1.0) :background ,org-h3-bg))))
     `(org-level-4 ((,class (:bold nil :foreground ,yellow :background ,org-h4-bg))))
     `(org-level-5 ((,class (:bold nil :foreground ,inf))))
     `(org-level-6 ((,class (:bold nil :foreground ,str))))
     `(org-level-7 ((,class (:bold nil :foreground ,green))))
     `(org-level-8 ((,class (:bold nil :foreground ,yellow))))
     `(org-link ((,class (:underline t :foreground ,comment))))
     `(org-mode-line-clock-overrun ((,class (:foreground ,err))))
     `(org-priority ((,class (:foreground ,war :bold t))))
     `(org-quote ((,class (:inherit org-block :slant italic))))
     `(org-scheduled ((,class (:foreground ,comp))))
     `(org-scheduled-today ((,class (:foreground ,func :weight bold :height 1.2))))
     `(org-sexp-date ((,class (:foreground ,base))))
     `(org-special-keyword ((,class (:foreground ,func))))
     `(org-table ((,class (:foreground ,base :background ,org-h2-bg))))
     `(org-todo ((,class (:foreground ,war :bold t :background ,org-h4-bg))))
     `(org-verbatim ((,class (:foreground ,inf))))
     `(org-verse ((,class (:inherit org-block :slant italic))))
     `(org-warning ((,class (:foreground ,err))))


;;;;; lsp-ui
	 `(lsp-ui-doc-background ((,class (:background, bg2))))

;;;;; enhanced-ruby
     `(enh-ruby-op-face ((,class (:foreground ,func))))
     `(enh-ruby-regex-face ((,class (:foreground ,str))))
     `(enh-ruby-regex-delim-face ((,class (:foreground ,str))))
     `(enh-ruby-heredoc-delimiter-face ((,class (:foreground ,inf))))

;;;;; powerline
     `(powerline-active1 ((,class (:background ,active2 :foreground ,base))))
     `(powerline-active2 ((,class (:background ,active2 :foreground ,base))))
     `(powerline-inactive1 ((,class (:background ,bg2 :foreground ,base))))
     `(powerline-inactive2 ((,class (:background ,bg2 :foreground ,base))))

;;;;; rainbow-delimiters
     `(rainbow-delimiters-depth-1-face ((,class :foreground ,inf)))
     `(rainbow-delimiters-depth-2-face ((,class :foreground ,func)))
     `(rainbow-delimiters-depth-3-face ((,class :foreground ,str)))
     `(rainbow-delimiters-depth-4-face ((,class :foreground ,green)))
     `(rainbow-delimiters-depth-5-face ((,class :foreground ,yellow)))
     `(rainbow-delimiters-depth-6-face ((,class :foreground ,inf)))
     `(rainbow-delimiters-depth-7-face ((,class :foreground ,func)))
     `(rainbow-delimiters-depth-8-face ((,class :foreground ,str)))
     `(rainbow-delimiters-unmatched-face ((,class :foreground ,war)))

;;;;; smartparens
     `(sp-pair-overlay-face ((,class (:background ,highlight :foreground ,bg1))))
     `(sp-show-pair-match-face ((,class (:foreground ,(if (eq variant 'dark) suc red) :weight bold :underline t))))

;;;;; term
     `(term ((,class (:foreground ,base :background ,(get-hex-or-term 15)))))
     `(term-color-black ((,class (:background ,bg4, :foreground ,(get-hex-or-term 0)))))
     `(term-color-blue ((,class (:background ,keyword, :foreground ,(get-hex-or-term 4)))))
     `(term-color-cyan ((,class (:background ,cyan, :foreground ,(get-hex-or-term 6)))))
     `(term-color-green ((,class (:background ,green :foreground ,(get-hex-or-term 2)))))
     `(term-color-magenta ((,class (:background ,builtin :foreground ,(get-hex-or-term 5)))))
     `(term-color-red ((,class (:background ,red :foreground ,(get-hex-or-term 1)))))
     `(term-color-white ((,class (:background ,base :foreground ,(get-hex-or-term 7)))))
     `(term-color-yellow ((,class (:background ,yellow :foreground ,(get-hex-or-term 3)))))

;;;;; which-key
     `(which-key-command-description-face ((,class (:foreground ,base))))
     `(which-key-group-description-face ((,class (:foreground ,key2))))
     `(which-key-key-face ((,class (:foreground ,func :bold t))))
     `(which-key-separator-face ((,class (:background nil :foreground ,str))))
     `(which-key-special-key-face ((,class (:background ,func :foreground ,bg1))))

;;;;; other, need more work
     `(ac-completion-face ((,class (:underline t :foreground ,keyword))))
     `(elixir-atom-face ((,class (:foreground ,func))))
     `(ffap ((,class (:foreground ,base))))
     `(flx-highlight-face ((,class (:foreground ,comp :underline nil))))
     `(font-latex-bold-face ((,class (:foreground ,comp))))
     `(font-latex-italic-face ((,class (:foreground ,key2 :italic t))))
     `(font-latex-match-reference-keywords ((,class (:foreground ,const))))
     `(font-latex-match-variable-keywords ((,class (:foreground ,var))))
     `(font-latex-string-face ((,class (:foreground ,str))))
     `(icompletep-determined ((,class :foreground ,builtin)))
     `(js2-external-variable ((,class (:foreground ,comp  ))))
     `(js2-error ((,class (:foreground ,err))))
     `(js2-function-param ((,class (:foreground ,const))))
     `(js2-function-call ((,class (:inherit ,font-lock-function-name-face))))
     `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,str))))
     `(js2-jsdoc-html-tag-name ((,class (:foreground ,key1))))
     `(js2-jsdoc-value ((,class (:foreground ,str))))
     `(js2-private-function-call ((,class (:foreground ,const))))
     `(js2-private-member ((,class (:foreground ,base))))
     `(js3-error-face ((,class (:underline ,war))))
     `(js3-external-variable-face ((,class (:foreground ,var))))
     `(js3-function-param-face ((,class (:foreground ,key2))))
     `(js3-instance-member-face ((,class (:foreground ,const))))
     `(js3-jsdoc-tag-face ((,class (:foreground ,keyword))))
     `(js3-warning-face ((,class (:underline ,keyword))))
     `(mu4e-cited-1-face ((,class (:foreground ,base))))
     `(mu4e-cited-7-face ((,class (:foreground ,base))))
     `(mu4e-header-marks-face ((,class (:foreground ,comp))))
     `(mu4e-view-url-number-face ((,class (:foreground ,comp))))
     `(py-variable-name-face ((,class (:foreground ,var))))
     `(slime-repl-inputed-output-face ((,class (:foreground ,comp))))
	 `(sh-quoted-text ((,class (:foreground ,func))))
     `(trailing-whitespace ((,class :foreground nil :background ,err)))
     `(undo-tree-visualizer-current-face ((,class :foreground ,builtin)))
     `(undo-tree-visualizer-default-face ((,class :foreground ,base)))
     `(undo-tree-visualizer-register-face ((,class :foreground ,comp)))
     `(undo-tree-visualizer-unmodified-face ((,class :foreground ,var)))
     `(web-mode-builtin-face ((,class (:inherit ,font-lock-builtin-face))))
     `(web-mode-comment-face ((,class (:inherit ,font-lock-comment-face :italic t))))
     `(web-mode-constant-face ((,class (:inherit ,font-lock-constant-face))))
     `(web-mode-doctype-face ((,class (:inherit ,font-lock-comment-face :italic t))))
     `(web-mode-function-name-face ((,class (:inherit ,font-lock-function-name-face))))
     `(web-mode-html-attr-name-face ((,class (:foreground ,func))))
     `(web-mode-html-attr-value-face ((,class (:foreground ,keyword))))
     `(web-mode-html-tag-face ((,class (:foreground ,var))))
     `(web-mode-html-tag-bracket-face ((,class (:foreground ,keyword))))
     `(web-mode-current-element-highlight-face ((,class (:foreground ,var :background ,bg2))))
     `(web-mode-keyword-face ((,class (:foreground ,keyword))))
     `(web-mode-json-context-face ((,class (:foreground ,builtin))))
     `(web-mode-json-key-face ((,class (:foreground ,keyword))))
     `(web-mode-string-face ((,class (:foreground ,str))))
     `(web-mode-symbol-face ((,class (:foreground ,func))))
     `(web-mode-type-face ((,class (:inherit ,font-lock-type-face))))
     `(web-mode-warning-face ((,class (:inherit ,font-lock-warning-face)))))))

(deftheme wpgtk "Theme for wpgtk template system")
(create-wpgtk-theme 'dark 'wpgtk)
(provide-theme 'wpgtk)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; wpgtk-theme.el ends here
