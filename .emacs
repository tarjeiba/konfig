(package-initialize)

(org-babel-load-file "~/emacs-konfig.org")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#4d4d4c" "#c82829" "#718c00" "#eab700" "#4271ae" "#8959a8" "#3e999f" "#d6d6d6"))
 '(custom-enabled-themes (quote (minimal-light)))
 '(custom-safe-themes
   (quote
    ("3e335d794ed3030fefd0dbd7ff2d3555e29481fe4bbb0106ea11c660d6001767" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(darkroom-text-scale-increase 0)
 '(fci-rule-color "#d6d6d6" t)
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(org-agenda-files
   (quote
    ("~/jobb/fagmateriell/org/promo/promo.org" "~/jobb/fagmateriell/org/promo/04_abstraksjon.org" "~/jobb/fagmateriell/org/promo/tanker-og-todos.org" "~/journal/journal.org" "~/journal/gjøremål.org" "~/journal/møter.org")))
 '(org-babel-python-command "python")
 '(org-edit-src-turn-on-auto-save t)
 '(org-export-with-section-numbers nil)
 '(org-export-with-toc nil)
 '(org-footnote-section "Fotnoter")
 '(org-html-footnotes-section
   "<div id=\"footnotes\">
<h2 class=\"footnotes\" hidden>%s</h2>
<hr>
<div id=\"text-footnotes\">
%s
</div>
</div>")
 '(org-src-fontify-natively t)

 '(package-selected-packages
   (quote
    (ein epc minimal-theme which-key ob-ipython org helm move-text try hungry-delete paredit par-edit use-package elpy org-ref company-quickhelp htmlize company-jedi company markdown-mode arduino-mode magit darkroom ox-reveal visual-fill-column powerline)))
 '(python-shell-completion-native-enable nil)
 '(python-shell-extra-pythonpaths nil)
 '(python-shell-interpreter "ipython")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#c82829")
     (40 . "#f5871f")
     (60 . "#eab700")
     (80 . "#718c00")
     (100 . "#3e999f")
     (120 . "#4271ae")
     (140 . "#8959a8")
     (160 . "#c82829")
     (180 . "#f5871f")
     (200 . "#eab700")
     (220 . "#718c00")
     (240 . "#3e999f")
     (260 . "#4271ae")
     (280 . "#8959a8")
     (300 . "#c82829")
     (320 . "#f5871f")
     (340 . "#eab700")
     (360 . "#718c00"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)


(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
