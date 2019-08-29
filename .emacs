(package-initialize)

(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("gnu" . "https://elpa.gnu.org/packages/")
	("org" . "http://orgmode.org/elpa/")))

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-compute-statistics t)

(use-package ivy
  :ensure t
  :bind (("C-x b" . ivy-switch-buffer)
	 ("C-x 4 b" . ivy-switch-buffer-other-window)
	 ("M-x" . counsel-M-x)
	 ("C-x f" . counsel-recentf)
	 ("C-x C-f" . counsel-find-file))
  )



(add-hook 'after-init-hook (lambda () (load-theme 'flatui)))
(add-to-list 'default-frame-alist '(internal-border-width . 30))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))


(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x |") 'toggle-window-split)

(defconst org-journal-dir "~/journal/org")
(defconst org-journal-file (concat org-journal-dir "/journal.org"))
(defconst org-todo-file (concat org-journal-dir "/gjøremål.org"))
(defconst org-worklog-file (concat org-journal-dir "/arbeidslogg.org"))
(defconst work-dir "~/jobb")
(defconst repo-dir "~/repos")
(defconst promo-org-dir (concat repo-dir "/munch/promo/org"))
(defconst promo-pub-dir (concat repo-dir "/munch/promo"))
(defconst fysikk1-org-dir (concat repo-dir "/munch/fysikk1/org"))
(defconst fysikk1-pub-dir (concat repo-dir "/munch/fysikk1"))
(defconst r1-org-dir (concat repo-dir "/munch/r1/org"))
(defconst r1-pub-dir (concat repo-dir "/munch/r1"))
(defconst org-r1-todo-file (concat  r1-org-dir "/r1.org"))
(defconst org-promo-todo-file (concat  promo-org-dir "/promo.org"))
(defconst fagdag-2p-org-dir (concat repo-dir "/munch/fagdag-2p"))
(defconst fagdag-2p-pub-dir (concat repo-dir "/munch/fagdag-2p"))
(setq-default fill-column 100)
(setq-default org-tags-column 0)
(setq recentf-max-menu-items 25)
(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)
(defalias 'yes-or-no-p 'y-or-n-p)

(recentf-mode 1)
(prefer-coding-system 'utf-8)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(display-time-mode 1)
(set-face-font 'default "Source Code Pro-12")

(use-package org
  :ensure t
  :bind (("C-c l" . org-store-link)
	 ("C-c c" . org-capture)
	 ("C-c a" . org-agenda))

  :config 
  (require 'ob-ipython)
  (require 'ob-shell)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (ipython . t)
     (dot . t)
     (ditaa . t)
     (C . t)
     (emacs-lisp . t)
     (latex . t)
     (shell . t)
     (js . t)
     (scheme . t)))

  (setq org-capture-templates
	'(("a" "Avtale" entry
	   (file+headline org-todo-file "Avtaler")
	   "* %^{Avtale} %^G\n%^T\n%?\n" :empty-lines 1 :immediate-finish t)
	  ("c" "Klokk inn" entry
	   (file+olp+datetree org-worklog-file)
	   "* %^{Element} :%^{tag}:\n"
	   :clock-in t :clock-keep t :immediate-finish t)
	  ("k" "Kommentar" item (clock) "%^{Kommentar}" :immediate-finish t)
	  ("g" "Gjøremål" entry
	   (file+headline org-todo-file "Gjøremål")
	   "* TODO %^{Gjøremål}\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t (taba-time-schedule current-prefix-arg)))\n%?"
	   :empty-lines 1)
	  ("j" "Journal")
	  ("jj" "Journal" entry
	   (file+datetree org-journal-file)
	   "* %<%H:%M> %^g\n%?\n  %i\n" :empty-lines 1)
	  ("jd" "Dagbok" entry
	   (file+olp org-journal-file "Dagbok")
	   "* %<%d.%m.%Y>\n%?\n" :empty-lines 1)))

  (org-link-set-parameters
   "popup"
   :follow (lambda (path)
	     (message (format "%s" path)))
   :export (lambda (path desc backend)
	     (cond
	      ((eq backend 'html)
	       (format (concat "<span class=\"popup\""
			       "onclick=\"myPopupFunction()\">%s"
			       "<span class=\"popuptext\" "
			       "id=\"myPopup\">%s</span></span>")
		       (or desc path) path)))))

  (org-link-set-parameters
   "ggb"
   :follow (lambda (path)
	     (message (format "%s" path)))
   :export (lambda (path desc backend)
	     (cond
	      ((eq backend 'html)
	       (format "<a href=\"ggb/%s\">%s</a>" path (or desc path))))))
  (org-link-set-parameters
   "zip"
   :follow (lambda (path)
	     (message (format "%s" path)))
   :export (lambda (path desc backend)
	     (cond
	      ((eq backend 'html)
	       (format "<a href=\"zip/%s\">%s</a>" path (or desc path))))))

  (org-link-set-parameters
   "py"
   :follow (lambda (path)
	     (org-open-file-with-emacs
	      (format "../py/%s" path)))
   :export (lambda (path desc backend)
	     (cond
	      ((eq backend 'html)
	       (format "<a href=\"py/%s\">%s</a>" path (or desc path))))))

  (org-link-set-parameters
   "pdf"
   :follow (lambda (path)
	     (org-open-file-with-emacs
	      (format "../pdf/%s" path)))
   :export (lambda (path desc backend)
	     (cond
	      ((eq backend 'html)
	       (format "<a href=\"pdf/%s\">%s</a>" path (or desc path))))))

  (org-link-set-parameters
   "fig"
   :follow (lambda (path)
	     (org-open-file-with-emacs
	      (format "../figurer/%s" path)))
   :export 'taba-org-export-html)

  ;; (org-link-set-parameters
  ;;  "fig"
  ;;  :follow (lambda (path)
  ;; 	     (org-open-file-with-emacs
  ;; 	      (format "../figurer/%s" path)))
  ;;  :export (lambda (link desc backend)
  ;; 	     (cond
  ;; 	      ((eq backend 'html)
  ;; 	       (format (concat "<img src=\"figurer/%s\""
  ;; 			       " alt=\"%s\"/>")
  ;; 			       link (or desc link))))))
	
;; (attributes-plist
;; 	  (let* ((parent (org-export-get-parent-element link))
;; 		 (link (let ((container (org-export-get-parent link)))
;; 			 (if (and (eq (org-element-type container) 'link)
;; 				  (org-html-inline-image-p link info))
;; 			     container
;; 			   link))))
;; 	    (and (eq (org-element-map parent 'link 'identity info t) link)
;; 		 (org-export-read-attribute :attr_html parent))))


  (org-link-set-parameters
   "pres"
   :follow (lambda (path)
	     (shell-command
	      (format "evince -s ../pdf/%s &" path)))
   :export (lambda (path desc backend)
	     (cond
	      ((eq backend 'html)
	       (format "<a href=\"pdf/%s\">%s</a>" path (or desc path))))))

  (setq org-src-tab-acts-natively t
	org-edit-src-content-indentation 0
	org-src-fontify-natively t
	org-export-coding-system 'utf-8
	org-html-postamble nil
	org-image-actual-width nil
	org-list-allow-alphabetical t)

  (setq holiday-bahai-holidays nil
	holiday-hebrew-holidays nil
	holiday-islamic-holidays nil
	holiday-oriental-holidays nil
	holiday-other-holidays nil
	holiday-local-holidays nil
	holiday-christian-holidays nil
	holiday-general-holidays nil
	holiday-solar-holidays nil)

  (setq org-todo-keywords
	'((sequence "TODO" "|" "DONE")
	  (sequence "." "PLANLAGT" "|" "AVHOLDT")
	  (sequence "|" "CANCELED")))

  (setq org-agenda-custom-commands
	'(("h" "Agenda og hjemme"
	   ((agenda "" ((org-agenda-span 14)
			(org-deadline-warning-days 14)
			(org-agenda-prefix-format "")
			(org-agenda-filter-preset '("-jobb"))))))
	  ("j" "Jobb i dag og i morra"
	   ((agenda "" ((org-agenda-span 2)
			(org-agenda-filter-preset '("+jobb"))
			(org-agenda-prefix-format "%t ")))
	    (todo "TODO" ((org-agenda-filter-preset '("+jobb"))
			  (org-agenda-max-entries 0)))))))



  (setq org-agenda-include-diary t
	org-agenda-skip-deadline-if-done t
	org-agenda-skip-scheduled-if-done t
	org-agenda-skip-timestamp-if-done t
	org-agenda-window-setup '(current-window)
	org-src-window-setup '(current-window)
	org-startup-indented t
	org-agenda-files (mapcar (lambda (x) (concat org-journal-dir "/" x)) '("journal.org" "gjøremål.org" "møter.org")))

  (add-to-list 'org-modules 'org-habit)

  (setq org-refile-targets '((nil :maxlevel . 2)
			     ;; all top-level headlines in the
			     ;; current buffer are used as targets first
			     ;; as a refile target
			     (org-agenda-files :maxlevel . 2)))

  (setq org-refile-use-outline-path 'file)


  (setq org-startup-indented t)
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'org-mode-hook 'visual-fill-column-mode)
  (add-hook 'org-capture-mode-hook 'delete-other-windows)
  ;; (add-hook 'org-trigger-hook 'taba-write-journal-on-todo)

  (defun taba-org-mode-hook ()
    "Org level heading scaling."
    (dolist (face '(org-level-1
		    org-level-2
		    org-level-3
		    org-level-4
		    org-level-5))
      (set-face-attribute face nil :weight 'semi-bold :height 1.0)))
  
  (add-hook 'org-mode-hook 'taba-org-mode-hook)

  (defun taba-org-screenshot (arg)
    "Take a screenshot into a time stamped unique-named file in the
	    same directory as the org-buffer and insert a link to this file."
    (interactive "P")
    (let* ((folder "./figurer/")
	   (filestub (if arg
			 (read-string "Bildenavn (uten .png): ")
		       (concat (file-name-base buffer-file-name) "_"
			       (format-time-string "%Y%m%d_%H%M%S"))))
	   (filename (concat folder filestub ".png")))
      (message filename)
      (if (eq system-type 'windows-nt)
	  (call-process "boxcutter" nil nil nil filename)) ; forutsetter boxcutter http://keepnote.org/boxcutter/
      (if (eq system-type 'gnu/linux)
	  (call-process "import" nil nil nil filename)) ; imagemagick
      (insert (concat "[[file:" filename "]]"))))

  (global-set-key (kbd "C-c s") 'taba-org-screenshot)

  (add-to-list 'org-structure-template-alist
	       (list "sp" "#+BEGIN_SRC python\n?\n#+END_SRC\n"))
  (add-to-list 'org-structure-template-alist
	       (list "se" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC\n"))    
  (add-to-list 'org-structure-template-alist
	       (list "ii" (concat "#+ATTR_HTML: :width 100% :heigh 100%\n"
				  "[[./figurer/?]]")))
  (add-to-list 'org-structure-template-alist
	       (list "ll" (concat
			   "#+html: <input type=\"button\" onclick=\"return toggleMe('special1')\" value=\"løsning\"><br><br>\n"
			   "#+attr_html: :id special1 :style display:none;border:1px solid black\n"
			   "#+begin_div\n?\n#+end_div")))

  (add-to-list 'org-structure-template-alist
	       (list "loe" (concat
			    "#+BEGIN_EXPORT html\n"
			    "<label for=\"toggle-1\">Vis løsning: </label>\n"
			    "<input type=\"checkbox\" id=\"toggle-1\">\n"
			    "#+END_EXPORT\n"
			    "#+BEGIN_LOESNING\n?\n"
			    "#+END_LOESNING")))

  (add-to-list 'org-structure-template-alist
	       (list "sd" (concat
			   "#+BEGIN_SRC dot :file ..figurer/?.png :cmdline -Kdot -Tpng :results silent"
			   "digraph {\n"
			   "graph[fontname=\"Open Sans\"]\n"
			   "node[fontname=\"Open Sans\"]\n"
			   "edge[fontname=\"Open Sans\"]\n"
			   "}\n"
			   "#+END_SRC\n"
			   "[[fig:.png]]")))

  (add-to-list 'org-structure-template-alist
	       (list "t" "#+BEGIN_TASK\n?\n#+END_TASK"))
  (add-to-list 'org-structure-template-alist
	       (list "d" "#+BEGIN_DEFINITION\n?\n#+END_DEFINITION")))

(use-package ox-reveal
  :after org
  :config
  (progn
    (setq org-reveal-title-slide "<h1>%t</h1>")
    (setq org-reveal-root "https://cdn.jsdelivr.net/reveal.js/3.0.0/")
    ))

(use-package org-habit
  :after org)
(use-package ox-publish
  :after org)
(use-package ox-beamer
  :after org)

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config (setenv "GIT_ASKPASS" "git-gui--askpass"))

(use-package which-key
  :ensure t
  :config (which-key-mode t))

(setq calendar-week-start-day 1
      calendar-day-name-array ["søndag" "mandag" "tirsdag" "onsdag" "torsdag" "fredag" "lørdag"]
      calendar-month-name-array ["januar" "februar" "mars" "april"
                                 "mai" "juni" "juli" "august"
                                 "september" "oktober" "november" "desember"])
(setq org-publish-project-alist
      `(("konturer-notes"
	 :base-directory ,(concat work-dir "/fagmateriell/org")
	 :base-extension "org"
	 :publishing-directory ,(concat work-dir "/fagmateriell/html")
	 :publishing-function org-html-publish-to-html
	 :recursive t
	 :headline-levels 4
	 :html-preamble konturer-preamble
	 :exclude "*/todo.org")
	("konturer-static"
	 :base-directory ,(concat work-dir "/fagmateriell/org")
	 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|sww\\|gmbl\\|ggb\\|svg"
	 :publishing-directory ,(concat work-dir "/fagmateriell/html")
	 :recursive t
	 :publishing-function org-publish-attachment)
	("konturer-pres"
	 :base-directory ,(concat work-dir "/fagmateriell/org")
	 :base-extension "orgpres"
	 :publishing-directory ,(concat work-dir "/fagmateriell/html")
	 :recursive t
	 :publishing-function taba-org-reveal-publish-to-html)
	("konturer" 
	 :components ("konturer-notes" "konturer-static" "konturer-pres"))
	))

(add-to-list 'auto-mode-alist '("\\.orgpres\\'" . org-mode))

(add-to-list 'org-publish-project-alist
	     `("r1"
	       :base-directory ,r1-org-dir
	       :base-extension "org"
	       :publishing-directory ,r1-pub-dir
	       :publishing-function org-html-publish-to-html
	       :recursive t
	       :headline-levels 4
	       :html-preamble nil
	       :auto-sitemap nil
	       :exclude "tanker-og-todos.org\\|orgheader.org\\|orgheader_nojs.org\\|sitemap.org"))
(add-to-list 'org-publish-project-alist
	     `("promo"
	       :base-directory ,promo-org-dir
	       :base-extension "org"
	       :publishing-directory ,promo-pub-dir
	       :publishing-function org-html-publish-to-html
	       :recursive t
	       :headline-levels 6
	       :html-preamble nil
	       :auto-sitemap nil
	       :exclude "tanker-og-todos.org\\|orgheader.org\\|orgheader_nojs.org\\|sitemap.org"
	       :exclude-tags "pdf"))

(add-to-list 'org-publish-project-alist
	     `("fysikk1"
	       :base-directory ,fysikk1-org-dir
	       :base-extension "org"
	       :publishing-directory ,fysikk1-pub-dir
	       :publishing-function org-html-publish-to-html
	       :recursive t
	       :headline-levels 6
	       :html-preamble nil
	       :auto-sitemap nil
	       :exclude "tanker-og-todos.org\\|orgheader.org\\|orgheader_nojs.org\\|sitemap.org"))

(with-eval-after-load 'ox-html
  (add-to-list 'org-html-infojs-options '(sdepth . "1"))
  (setq org-html-htmlize-output-type 'css)
  (setq org-html-use-infojs 'when-configured))


(defun euklid-save-hook ()
  (when (string= (file-name-nondirectory (buffer-file-name)) "src.js")
    (call-process-shell-command "build_euklid &")))

(add-hook 'after-save-hook 'euklid-save-hook)

(defun tob64 (filename)
  (base64-encode-string
   (with-temp-buffer
     (insert-file-contents filename)
     (buffer-string))))

(defun taba-insert-encoded-image-link (filename)
  (insert (format "<img src=\"data:image/png;base64,%s\">"
		  (tob64 filename))))

(defun taba-dired-mode-setup ()
  "to be run as hook for `dired-mode'."
  (dired-hide-details-mode 1)) 		; kan skrus av/på med "("
(add-hook 'dired-mode-hook 'taba-dired-mode-setup)

(add-hook 'dired-load-hook '(lambda () (require 'dired-x)))
(setq dired-omit-mode t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#4d4d4c" "#c82829" "#718c00" "#eab700" "#4271ae" "#8959a8" "#3e999f" "#d6d6d6"))
 '(column-number-mode t)
 '(custom-enabled-themes (quote (flatui)))
 '(custom-safe-themes
   (quote
    ("3898b4f9c3f6f2994f5010f766a7f7dac4ee2a5c5eb18c429ab8e71c5dad6947" "896e853cbacc010573cd82b6cf582a45c46abe2e45a2f17b74b4349ff7b29e34" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "392395ee6e6844aec5a76ca4f5c820b97119ddc5290f4e0f58b38c9748181e8d" "3e335d794ed3030fefd0dbd7ff2d3555e29481fe4bbb0106ea11c660d6001767" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(fci-rule-color "#d6d6d6" t)
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(fringe-mode nil nil (fringe))
 '(js2-strict-missing-semi-warning nil)
 '(magit-branch-read-upstream-first (quote fallback))
 '(magit-dispatch-arguments nil)
 '(magit-remote-arguments (quote ("-f")))
 '(mode-line-format
   (quote
    ("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position "  " mode-line-modes
     (:eval
      (cond
       ((org-clocking-p)
	(org-clock-get-clock-string))
       (t "(🕓)")))
     mode-line-end-spaces)))
 '(nand2tetris-core-base-dir "~/nand2tetris")
 '(org-agenda-files
   (quote
    ("~/repos/munch/promo/org/oekter.org" "~/journal/org/journal.org" "~/journal/org/gjøremål.org" "~/journal/org/møter.org")))
 '(org-babel-python-command "python")
 '(org-ditaa-jar-option "-jar")
 '(org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar")
 '(org-edit-src-turn-on-auto-save t)
 '(org-export-with-section-numbers nil)
 '(org-export-with-toc nil)
 '(org-footnote-section "Fotnoter")
 '(org-format-latex-options
   (quote
    (:foreground default :background default :scale 1.5 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
		 ("begin" "$1" "$" "$$" "\\(" "\\["))))
 '(org-html-footnotes-section
   "<div id=\"footnotes\">
<h2 class=\"footnotes\" hidden>%s</h2>
<hr>
<div id=\"text-footnotes\">
%s
</div>
</div>")
 '(org-html-infojs-options
   (quote
    ((path . "https://orgmode.org/org-info.js")
     (view . "info")
     (toc . "1")
     (ftoc . "0")
     (tdepth . "max")
     (sdepth . "2")
     (mouse . "underline")
     (buttons . "0")
     (ltoc . "0")
     (up . :html-link-up)
     (home . :html-link-home))))
 '(org-indent-indentation-per-level 2)
 '(org-src-fontify-natively t)
 '(org-src-preserve-indentation t)
 '(org-structure-template-alist
   (quote
    (("n" "#+BEGIN_NOTES
?
#+END_NOTES")
     ("d" "#+BEGIN_DEFINITION
?
#+END_DEFINITION")
     ("t" "#+BEGIN_TASK
?
#+END_TASK")
     ("rs" "#+REVEAL: split")
     ("rfA" "#+ATTR_REVEAL: :frag (appear)")
     ("rfa" "#+ATTR_REVEAL: :frag appear")
     ("sd" "#+BEGIN_SRC dot :file ..figurer/?.png :cmdline -Kdot -Tpng :results silentdigraph {
graph[fontname=\"Open Sans\"]
node[fontname=\"Open Sans\"]
edge[fontname=\"Open Sans\"]
}
#+END_SRC
[[fig:.png]]")
     ("loe" "#+BEGIN_EXPORT html
<label for=\"toggle-1\">Vis løsning: </label>
<input type=\"checkbox\" id=\"toggle-1\">
#+END_EXPORT
#+BEGIN_LOESNING
?
#+END_LOESNING")
     ("ll" "#+html: <input type=\"button\" onclick=\"return toggleMe('special1')\" value=\"løsning\"><br><br>
#+attr_html: :id special1 :style display:none;border:1px solid black
#+begin_div
?
#+end_div")
     ("ii" "#+ATTR_HTML: :width 100% :heigh 100%
[[./figurer/?]]")
     ("se" "#+BEGIN_SRC emacs-lisp
?
#+END_SRC
")
     ("sp" "#+BEGIN_SRC python
?
#+END_SRC
")
     ("s" "#+BEGIN_SRC ?

#+END_SRC")
     ("e" "#+BEGIN_EXAMPLE
?
#+END_EXAMPLE")
     ("q" "#+BEGIN_QUOTE
?
#+END_QUOTE")
     ("v" "#+BEGIN_VERSE
?
#+END_VERSE")
     ("V" "#+BEGIN_VERBATIM
?
#+END_VERBATIM")
     ("c" "#+BEGIN_CENTER
?
#+END_CENTER")
     ("C" "#+BEGIN_COMMENT
?
#+END_COMMENT")
     ("l" "#+BEGIN_EXPORT latex
?
#+END_EXPORT")
     ("L" "#+BEGIN_LaTeX:
?
#+END_LaTeX")
     ("h" "#+BEGIN_EXPORT html
?
#+END_EXPORT")
     ("H" "#+HTML: ")
     ("a" "#+BEGIN_EXPORT ascii
?
#+END_EXPORT")
     ("A" "#+ASCII: ")
     ("i" "#+INDEX: ?")
     ("I" "#+INCLUDE: %file ?"))))
 '(package-selected-packages
   (quote
    (darkroom flucui-themes nand2tetris nand2tetris-assembler counsel indium org-plus-contrib pdf-tools ag which-key ob-ipython org helm try use-package htmlize magit)))
 '(safe-local-variable-values
   (quote
    ((org-display-custom-times . t)
     (org-time-stamp-custom-formats . "<%Y uke %W: %a %e. %m>"))))
 '(text-scale-mode-step 1.1)
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
 '(vc-annotate-very-old-color nil)
 '(vc-follow-symlinks t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#ecf0f1" :foreground "#2c3e50" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "PfEd" :family "Source Code Pro"))))
 '(fringe ((t (:background "#ecf0f1" :foreground "#34495e"))))
 '(window-divider ((t (:foreground "#ecf0f1")))))

(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)

(defun date-weeknumber (datestring) (interactive) (format-time-string "%V" (date-to-time (concat "2019" datestring "T00:00:00Z"))))

(defun taba-org-export-html (link desc backend)
  (cond
   ((eq backend 'html)
    (progn (message (number-to-string (org-element-property :begin (org-element-at-point))))
	   (format (concat "<img src=\"figurer/%s\""
  		    " alt=\"%s\"/>")
  		   link (or desc link))))
   ((eq backend 'latex)
    (format (concat "\begin{figure}"
		    "\includegraphics[width=\linewidth]{%s}"
		    "\label{fig:boat1}"
		    "\end{figure}") link))))

					; Handle backups differently

(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)


