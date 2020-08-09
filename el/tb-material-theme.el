(deftheme tb-material
  "Created 2020-01-01.")

(defvar contrast "#b3e5fc")

(custom-theme-set-variables
 'tb-material
 '(package-selected-packages (quote (exwm material-theme arduino-mode arduino flycheck flycheck-mode lsp-ui lsp-mode dired dired-x ob-shell jupyter which-key visual-fill-column use-package try pdf-tools org-plus-contrib magit hungry-delete htmlize helm flatui-theme elpy darkroom counsel ag)))
 '(fci-rule-color "#37474f")
 '(ansi-color-names-vector (quote (vector "#ffffff" "#f36c60" "#8bc34a" "#fff59d" "#4dd0e1" "#b39ddb" "#81d4fa" "#263238")))
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold]))

(custom-theme-set-faces
 'tb-material
 '(fringe ((((class color) (min-colors 89)) (:background "#263238"))))
 '(jupyter-repl-input-prompt ((t (:foreground "dark green"))))
 '(bold ((((class color) (min-colors 89)) (:weight bold))))
 '(bold-italic ((((class color) (min-colors 89)) (:slant italic :weight bold))))
 '(underline ((((class color) (min-colors 89)) (:underline t))))
 '(italic ((((class color) (min-colors 89)) (:slant italic))))
 '(font-lock-builtin-face ((((class color) (min-colors 89)) (:foreground "#ff8A65"))))
 '(font-lock-comment-delimiter-face ((((class color) (min-colors 89)) (:foreground "#b0bec5"))))
 '(font-lock-comment-face ((((class color) (min-colors 89)) (:foreground "#b0bec5"))))
 '(font-lock-constant-face ((((class color) (min-colors 89)) (:foreground "#8bc34a"))))
 '(font-lock-doc-face ((((class color) (min-colors 89)) (:foreground "moccasin"))))
 `(font-lock-function-name-face ((((class color) (min-colors 89)) (:foreground ,contrast))))
 '(font-lock-keyword-face ((((class color) (min-colors 89)) (:foreground "#fff59d"))))
 '(font-lock-negation-char-face ((((class color) (min-colors 89)) (:foreground "#4dd0e1"))))
 '(font-lock-preprocessor-face ((((class color) (min-colors 89)) (:foreground "gold"))))
 '(font-lock-regexp-grouping-backslash ((((class color) (min-colors 89)) (:foreground "#fff59d"))))
 '(font-lock-regexp-grouping-construct ((((class color) (min-colors 89)) (:foreground "#b39ddb"))))
 '(font-lock-string-face ((((class color) (min-colors 89)) (:foreground "#9ccc65"))))
 `(font-lock-type-face ((((class color) (min-colors 89)) (:foreground ,contrast))))
 '(font-lock-variable-name-face ((((class color) (min-colors 89)) (:foreground "#ffcc80"))))
 '(font-lock-warning-face ((((class color) (min-colors 89)) (:weight bold :foreground "#f36c60"))))
 '(shadow ((((class color) (min-colors 89)) (:foreground "#b0bec5"))))
 '(success ((((class color) (min-colors 89)) (:foreground "SeaGreen2"))))
 '(error ((((class color) (min-colors 89)) (:foreground "#f36c60"))))
 '(warning ((((class color) (min-colors 89)) (:foreground "#ff9800"))))
 '(match ((((class color) (min-colors 89)) (:foreground "#263238" :background "#8bc34a" :inverse-video nil))))
 '(isearch ((((class color) (min-colors 89)) (:foreground "#ffffff" :background "#8bc34a"))))
 '(isearch-fail ((((class color) (min-colors 89)) (:background "#263238" :inherit font-lock-warning-face :inverse-video t))))
 '(cursor ((((class color) (min-colors 89)) (:background "#ff9800"))))
 '(border ((((class color) (min-colors 89)) (:background "#37474f"))))
 '(vertical-border ((((class color) (min-colors 89)) (:background "#555555" :foreground "#555555"))))
 '(highlight ((((class color) (min-colors 89)) (:inverse-video nil :background "#37474f"))))
 '(mode-line ((t (:background "#1c1f26" :foreground "#ffffff"))))
 '(mode-line-buffer-id ((((class color) (min-colors 89)) (:foreground "#ffffff" :background nil :weight bold))))
 '(mode-line-inactive ((((class color) (min-colors 89)) (:inherit mode-line :foreground "#a7adba" :background "#1c1f26" :weight normal :box nil))))
 '(mode-line-emphasis ((((class color) (min-colors 89)) (:foreground "#ffffff" :slant italic))))
 '(mode-line-highlight ((((class color) (min-colors 89)) (:foreground "#b39ddb" :box nil))))
 `(minibuffer-prompt ((((class color) (min-colors 89)) (:foreground ,contrast))))
 '(region ((((class color) (min-colors 89)) (:background "#555555"))))
 '(secondary-selection ((((class color) (min-colors 89)) (:background "#bf616a"))))
 '(header-line ((((class color) (min-colors 89)) (:inherit mode-line :foreground nil :background nil :weight bold))))
 '(trailing-whitespace ((((class color) (min-colors 89)) (:foreground "#f36c60" :inverse-video t :underline nil))))
 '(sh-heredoc ((((class color) (min-colors 89)) (:foreground nil :inherit font-lock-string-face :weight normal))))
 '(sh-quoted-exec ((((class color) (min-colors 89)) (:foreground nil :inherit font-lock-preprocessor-face))))
 '(diff-added ((((class color) (min-colors 89)) (:foreground "#8bc34a"))))
 '(diff-changed ((((class color) (min-colors 89)) (:foreground "#81d4fa"))))
 '(diff-removed ((((class color) (min-colors 89)) (:foreground "#ff9800"))))
 '(diff-header ((((class color) (min-colors 89)) (:foreground "#81d4fa" :background nil))))
 '(diff-file-header ((((class color) (min-colors 89)) (:foreground "#4dd0e1" :background nil))))
 '(diff-hunk-header ((((class color) (min-colors 89)) (:foreground "#b39ddb"))))
 '(diff-refine-added ((((class color) (min-colors 89)) (:inherit diff-added :inverse-video t))))
 '(diff-refine-removed ((((class color) (min-colors 89)) (:inherit diff-removed :inverse-video t))))
 '(eldoc-highlight-function-argument ((((class color) (min-colors 89)) (:foreground "#8bc34a" :weight bold))))
 '(link ((((class color) (min-colors 89)) (:foreground nil :underline t))))
 '(widget-button ((((class color) (min-colors 89)) (:underline t :weight bold))))
 '(widget-field ((((class color) (min-colors 89)) (:background "#37474f" :box (:line-width 1 :color "#ffffff")))))
 '(compilation-column-number ((((class color) (min-colors 89)) (:foreground "#fff59d"))))
 '(compilation-line-number ((((class color) (min-colors 89)) (:foreground "#fff59d"))))
 '(compilation-mode-line-exit ((((class color) (min-colors 89)) (:foreground "#8bc34a"))))
 '(compilation-mode-line-fail ((((class color) (min-colors 89)) (:foreground "#f36c60"))))
 '(compilation-mode-line-run ((((class color) (min-colors 89)) (:foreground "#4dd0e1"))))
 '(which-key-key-face ((((class color) (min-colors 89)) (:foreground "#ffffff" :weight bold))))
 '(which-key-special-key-face ((((class color) (min-colors 89)) (:foreground "#ff9800" :weight bold :height 1.1))))
 '(which-key-command-description-face ((((class color) (min-colors 89)) (:foreground "#ffffff"))))
 '(which-key-group-description-face ((((class color) (min-colors 89)) (:foreground "#81d4fa"))))
 '(which-key-separator-face ((((class color) (min-colors 89)) (:foreground "#b0bec5"))))
 `(org-agenda-structure ((((class color) (min-colors 89)) (:foreground ,contrast :bold t))))
 `(org-agenda-date ((((class color) (min-colors 89)) (:foreground ,contrast :underline nil))))
 '(org-agenda-done ((((class color) (min-colors 89)) (:foreground "#8bc34a"))))
 '(org-agenda-dimmed-todo-face ((((class color) (min-colors 89)) (:foreground "#b0bec5"))))
 '(org-block ((((class color) (min-colors 89)) (:foreground "#ffffff"))));
 '(org-code ((t (:foreground "#ffffff"))))
 '(org-column ((((class color) (min-colors 89)) (:background "#37474f"))))
 '(org-column-title ((((class color) (min-colors 89)) (:inherit org-column :weight bold :underline t))))
 '(org-date ((((class color) (min-colors 89)) (:foreground "#80cbc4" :underline t))))
 '(org-document-info ((((class color) (min-colors 89)) (:foreground "#81d4fa" :height 1.35))))
 `(org-document-info-keyword ((((class color) (min-colors 89)) (:foreground ,contrast :height 1.35))))
 '(org-document-title ((((class color) (min-colors 89)) (:weight bold :foreground "#ffffff" :height 1.35))))
 '(org-done ((((class color) (min-colors 89)) (:foreground "#8bc34a" :bold t :background "#1b5e20"))))
 '(org-ellipsis ((((class color) (min-colors 89)) (:foreground "#b0bec5"))))
 '(org-footnote ((((class color) (min-colors 89)) (:foreground "#81d4fa"))))
 '(org-formula ((((class color) (min-colors 89)) (:foreground "#f36c60"))))
 '(org-hide ((((class color) (min-colors 89)) (:foreground "#263238" :background "#263238"))))
 '(org-link ((((class color) (min-colors 89)) (:foreground "#b3e5fc"))))
 '(org-scheduled ((((class color) (min-colors 89)) (:foreground "#8bc34a"))))
 '(org-scheduled-previously ((((class color) (min-colors 89)) (:foreground "#ff9800"))))
 '(org-scheduled-today ((((class color) (min-colors 89)) (:foreground "#8bc34a"))))
 '(org-special-keyword ((((class color) (min-colors 89)) (:foreground "#b0bec5"))))
 '(org-table ((((class color) (min-colors 89)) (:foreground "#e3f2fd")))) ; :background "#1c1f26"))))
 ;; '(org-todo ((((class color) (min-colors 89)) (:foreground "#ffab91" :bold t :background "#dd2c00"))))
'(org-todo ((((class color) (min-colors 89)) (:foreground "#dd2c00" :bold t))))
 '(org-upcoming-deadline ((((class color) (min-colors 89)) (:foreground "#ff9800"))))
 '(org-warning ((((class color) (min-colors 89)) (:weight bold :foreground "#f36c60"))))
 '(org-block-begin-line ((t (:foreground "#b3e5fc"))))
 '(org-block-end-line ((((class color) (min-colors 89)) (:background nil :foreground "#b3e5fc"))))
 '(org-level-1 ((((class color) (min-colors 89)) (:inherit outline-1 :weight semi-bold  :height 1.0))))
 '(org-level-2 ((((class color) (min-colors 89)) (:inherit org-level-1))))
 '(org-level-3 ((((class color) (min-colors 89)) (:inherit org-level-1))))
 '(org-level-4 ((((class color) (min-colors 89)) (:inherit org-level-1))))
 '(org-level-5 ((((class color) (min-colors 89)) (:inherit org-level-1))))
 '(org-level-6 ((((class color) (min-colors 89)) (:inherit org-level-1))))
 '(org-level-7 ((((class color) (min-colors 89)) (:inherit org-level-1))))
 '(org-level-8 ((((class color) (min-colors 89)) (:inherit org-level-1))))
 '(org-ref-cite-face ((t (:inherit org-link))))
 '(org-drawer ((((class color) (min-colors 89)) (:foreground "#b3e5fc"))))
 '(markdown-header-face-1 ((((class color) (min-colors 89)) (:inherit font-lock-function-name-face :weight bold :height 1.3))))
 '(markdown-header-face-2 ((((class color) (min-colors 89)) (:inherit font-lock-function-name-face :weight bold :height 1.2))))
 '(markdown-header-face-3 ((((class color) (min-colors 89)) (:inherit font-lock-function-name-face :weight bold :height 1.1))))
 '(markdown-header-face-4 ((((class color) (min-colors 89)) (:inherit font-lock-function-name-face :weight bold :height 1.1))))
 '(markdown-header-face-5 ((((class color) (min-colors 89)) (:inherit font-lock-function-name-face :weight bold :height 1.1))))
 '(markdown-header-face-6 ((((class color) (min-colors 89)) (:inherit font-lock-function-name-face :weight bold :height 1.1))))
 '(markdown-header-delimiter-face ((((class color) (min-colors 89)) (:inherit font-lock-function-name-face :weight bold :height 1.2))))
 '(markdown-url-face ((((class color) (min-colors 89)) (:inherit link))))
 '(markdown-link-face ((((class color) (min-colors 89)) (:foreground "#4dd0e1" :underline t))))
 '(message-header-other ((((class color) (min-colors 89)) (:foreground nil :background nil :weight normal))))
 '(message-header-subject ((((class color) (min-colors 89)) (:inherit message-header-other :weight bold :foreground "#fff59d"))))
 '(message-header-to ((((class color) (min-colors 89)) (:inherit message-header-other :weight bold :foreground "#ff9800"))))
 '(message-header-cc ((((class color) (min-colors 89)) (:inherit message-header-to :foreground nil))))
 '(message-header-name ((((class color) (min-colors 89)) (:foreground "#4dd0e1" :background nil))))
 '(message-header-newsgroups ((((class color) (min-colors 89)) (:foreground "#81d4fa" :background nil :slant normal))))
 '(message-separator ((((class color) (min-colors 89)) (:foreground "#b39ddb"))))
 '(outline-1 ((((class color) (min-colors 89)) (:inherit nil :foreground "#eceff1"))))
 '(outline-2 ((((class color) (min-colors 89)) (:inherit nil :foreground "#e1f5fe"))))
 '(outline-3 ((((class color) (min-colors 89)) (:inherit nil :foreground "#a5d6a7"))))
 '(outline-4 ((((class color) (min-colors 89)) (:inherit nil :foreground "#ffcc80"))))
 '(outline-5 ((((class color) (min-colors 89)) (:inherit nil :foreground "#b3e5fc"))))
 '(outline-6 ((((class color) (min-colors 89)) (:inherit nil :foreground "CadetBlue1"))))
 '(outline-7 ((((class color) (min-colors 89)) (:inherit nil :foreground "aquamarine1"))))
 '(outline-8 ((((class color) (min-colors 89)) (:inherit nil :foreground "#b39ddb"))))
 '(gnus-summary-normal-unread ((((class color) (min-colors 89)) (:foreground "#ffffff" :weight bold))))
 '(gnus-summary-normal-read ((((class color) (min-colors 89)) (:foreground "#b0bec5" :weight normal))))
 '(gnus-summary-normal-ancient ((((class color) (min-colors 89)) (:foreground "#81d4fa" :weight normal))))
 '(gnus-summary-normal-ticked ((((class color) (min-colors 89)) (:foreground "#ff9800" :weight normal))))
 '(gnus-summary-low-unread ((((class color) (min-colors 89)) (:foreground "#b0bec5" :weight normal))))
 '(gnus-summary-low-read ((((class color) (min-colors 89)) (:foreground "#b0bec5" :weight normal))))
 '(gnus-summary-low-ancient ((((class color) (min-colors 89)) (:foreground "#b0bec5" :weight normal))))
 '(gnus-summary-high-unread ((((class color) (min-colors 89)) (:foreground "#fff59d" :weight normal))))
 '(gnus-summary-high-read ((((class color) (min-colors 89)) (:foreground "#8bc34a" :weight normal))))
 '(gnus-summary-high-ancient ((((class color) (min-colors 89)) (:foreground "#8bc34a" :weight normal))))
 '(gnus-summary-high-ticked ((((class color) (min-colors 89)) (:foreground "#ff9800" :weight normal))))
 '(gnus-summary-cancelled ((((class color) (min-colors 89)) (:foreground "#f36c60" :background nil :weight normal))))
 '(gnus-group-mail-low ((((class color) (min-colors 89)) (:foreground "#b0bec5"))))
 '(gnus-group-mail-low-empty ((((class color) (min-colors 89)) (:foreground "#b0bec5"))))
 '(gnus-group-mail-1 ((((class color) (min-colors 89)) (:foreground nil :weight normal :inherit outline-1))))
 '(gnus-group-mail-2 ((((class color) (min-colors 89)) (:foreground nil :weight normal :inherit outline-2))))
 '(gnus-group-mail-3 ((((class color) (min-colors 89)) (:foreground nil :weight normal :inherit outline-3))))
 '(gnus-group-mail-1-empty ((((class color) (min-colors 89)) (:inherit gnus-group-mail-1 :foreground "#b0bec5"))))
 '(gnus-group-mail-2-empty ((((class color) (min-colors 89)) (:inherit gnus-group-mail-2 :foreground "#b0bec5"))))
 '(gnus-group-mail-3-empty ((((class color) (min-colors 89)) (:inherit gnus-group-mail-3 :foreground "#b0bec5"))))
 '(gnus-group-news-1 ((((class color) (min-colors 89)) (:foreground nil :weight normal :inherit outline-5))))
 '(gnus-group-news-2 ((((class color) (min-colors 89)) (:foreground nil :weight normal :inherit outline-6))))
 '(gnus-group-news-3 ((((class color) (min-colors 89)) (:foreground nil :weight normal :inherit outline-7))))
 '(gnus-group-news-4 ((((class color) (min-colors 89)) (:foreground nil :weight normal :inherit outline-8))))
 '(gnus-group-news-5 ((((class color) (min-colors 89)) (:foreground nil :weight normal :inherit outline-1))))
 '(gnus-group-news-6 ((((class color) (min-colors 89)) (:foreground nil :weight normal :inherit outline-2))))
 '(gnus-group-news-1-empty ((((class color) (min-colors 89)) (:inherit gnus-group-news-1 :foreground "#b0bec5"))))
 '(gnus-group-news-2-empty ((((class color) (min-colors 89)) (:inherit gnus-group-news-2 :foreground "#b0bec5"))))
 '(gnus-group-news-3-empty ((((class color) (min-colors 89)) (:inherit gnus-group-news-3 :foreground "#b0bec5"))))
 '(gnus-group-news-4-empty ((((class color) (min-colors 89)) (:inherit gnus-group-news-4 :foreground "#b0bec5"))))
 '(gnus-group-news-5-empty ((((class color) (min-colors 89)) (:inherit gnus-group-news-5 :foreground "#b0bec5"))))
 '(gnus-group-news-6-empty ((((class color) (min-colors 89)) (:inherit gnus-group-news-6 :foreground "#b0bec5"))))
 '(custom-variable-tag ((((class color) (min-colors 89)) (:foreground "#4dd0e1"))))
 '(custom-group-tag ((((class color) (min-colors 89)) (:foreground "#4dd0e1"))))
 '(custom-state ((((class color) (min-colors 89)) (:foreground "#8bc34a"))))
 '(term ((((class color) (min-colors 89)) (:foreground nil :background nil :inherit default))))
 '(term-color-black ((((class color) (min-colors 89)) (:foreground "#ffffff" :background "#ffffff"))))
 '(term-color-red ((((class color) (min-colors 89)) (:foreground "#f36c60" :background "#f36c60"))))
 '(term-color-green ((((class color) (min-colors 89)) (:foreground "#8bc34a" :background "#8bc34a"))))
 '(term-color-yellow ((((class color) (min-colors 89)) (:foreground "#fff59d" :background "#fff59d"))))
 '(term-color-blue ((((class color) (min-colors 89)) (:foreground "#4dd0e1" :background "#4dd0e1"))))
 '(term-color-magenta ((((class color) (min-colors 89)) (:foreground "#b39ddb" :background "#b39ddb"))))
 '(term-color-cyan ((((class color) (min-colors 89)) (:foreground "#81d4fa" :background "#81d4fa"))))
 '(term-color-white ((((class color) (min-colors 89)) (:foreground "#263238" :background "#263238"))))
 '(default ((((class color) (min-colors 89)) (:foreground "#ffffff" :background "#263238")))))

(provide-theme 'tb-material)
