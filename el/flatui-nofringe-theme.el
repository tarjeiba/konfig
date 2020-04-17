(deftheme flatui-nofringe
  "Created 2018-11-11.")

(custom-theme-set-variables
 'flatui-nofringe
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector ["#4d4d4c" "#c82829" "#718c00" "#eab700" "#4271ae" "#8959a8" "#3e999f" "#d6d6d6"])
 '(custom-safe-themes (quote ("c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "392395ee6e6844aec5a76ca4f5c820b97119ddc5290f4e0f58b38c9748181e8d" "3e335d794ed3030fefd0dbd7ff2d3555e29481fe4bbb0106ea11c660d6001767" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(fci-rule-color "#d6d6d6")
 '(org-babel-python-command "python")
 '(org-edit-src-turn-on-auto-save t)
 '(org-export-with-section-numbers nil)
 '(org-export-with-toc nil)
 '(org-footnote-section "Fotnoter")
 '(org-html-footnotes-section "<div id=\"footnotes\">
<h2 class=\"footnotes\" hidden>%s</h2>
<hr>
<div id=\"text-footnotes\">
%s
</div>
</div>")
 '(org-html-infojs-options (quote ((sdepth . "2") (path . "https://orgmode.org/org-info.js") (view . "info") (toc . :with-toc) (ftoc . "0") (tdepth . "max") (sdepth . "max") (mouse . "underline") (buttons . "0") (ltoc . "1") (up . :html-link-up) (home . :html-link-home))))
 '(org-indent-indentation-per-level 2)
 '(org-src-fontify-natively t)
 '(package-selected-packages (quote (indium slack org-plus-contrib ox-rss jedi pdf-tools ag xref-js2 js2-refactor js2-mode ein epc minimal-theme which-key ob-ipython org helm move-text try hungry-delete paredit par-edit use-package elpy org-ref company-quickhelp htmlize company markdown-mode arduino-mode magit darkroom ox-reveal visual-fill-column powerline)))
 '(python-shell-completion-native-enable nil)
 '(python-shell-extra-pythonpaths nil)
 '(python-shell-interpreter "python")
 '(org-agenda-files (quote ("~/journal/org/arbeidslogg.org" "~/journal/org/gjøremål.org")))
 '(fringe-mode (quote (nil . 0))))

(custom-theme-set-faces
 'flatui-nofringe
 '(default ((t (:inherit nil :stipple nil :background "#ecf0f1" :foreground "#2c3e50" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "PfEd" :family "Source Code Pro"))))
 '(fringe ((t (:background "#ecf0f1" :foreground "#34495e")))))

(provide-theme 'flatui-nofringe)
