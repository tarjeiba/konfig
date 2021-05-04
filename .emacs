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

(add-to-list 'load-path "~/repos/konfig/el/")
(add-to-list 'custom-theme-load-path "~/repos/konfig/el/")

(load-theme 'tb-material t nil)

(setq vc-follow-symlinks t)
(setq inhibit-x-resources t)

(add-to-list 'default-frame-alist '(internal-border-width . 30))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(add-to-list 'default-frame-alist '(font . "Source Code Pro-12"))

(setq-default fill-column 100
	      org-tags-column 0)

(setq-default mode-line-format '("%e" mode-line-front-space
				 mode-line-mule-info mode-line-client
				 mode-line-modified mode-line-remote
				 mode-line-frame-identification
				 mode-line-buffer-identification "   "
				 mode-line-position "   "  "%b"
				 mode-line-end-spaces))

(setq recentf-max-menu-items 100
      recentf-max-saved-items 100
      inhibit-startup-screen t
      inhibit-splash-screen t
      initial-scratch-message nil
      text-scale-mode-step 1.1
      column-number-mode t
      ediff-window-setup-function 'ediff-setup-windows-plain)

(run-at-time (current-time) (* 5 60)
             (lambda ()
               (let ((save-silently t))
                 (recentf-save-list))))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq backup-directory-alist `(("." . "~/media/archive"))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(setq auto-save-file-name-transforms `((".*" "~/media/emacs-autosaves/" t)))

(setq create-lockfiles nil)


(setq display-buffer-alist 
      '(("*Async Shell Command*" display-buffer-no-window)))

(recentf-mode 1)
(prefer-coding-system 'utf-8)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(display-time-mode 1)

(set-register ?e '(file . "~/.emacs"))
(set-register ?j '(file . "~/journal/org/journal.org"))
(set-register ?p '(file . "~/journal/pengejournal.dat"))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
	 ("C-x f" . counsel-recentf)
	 ("C-x C-f" . counsel-find-file))
  :custom
  (counsel-find-file-ignore-regexp "__pycache__/"))

(use-package ivy
  :ensure t
  :bind (("C-x b" . ivy-switch-buffer)
	 ("C-x 4 b" . ivy-switch-buffer-other-window))
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-display-functions-alist
    '((counsel-irony . ivy-display-function-overlay)
      (ivy-completion-in-region . nil) ; set to ivy-display-function-overlay for drop-down
     (t)))
  :config
  (ivy-mode 1))

(use-package org
  :ensure t
  :load-path "~/repos/org-mode/lisp"
  :bind (("C-c l" . org-store-link)
	 ("C-c c" . org-capture)
	 ("C-c a" . org-agenda)
	 ("C-c C-x C-j" . org-clock-goto)
	 ("C-c n" . org-advance)
	 ("C-c p" . org-retreat))

  :config
  (defun org-advance ()
    (interactive)
    (when (buffer-narrowed-p)
      (goto-char (point-min))
      (widen)
      (org-forward-heading-same-level 1))
    (org-narrow-to-subtree))

  (defun org-retreat ()
    (interactive)
    (when (buffer-narrowed-p)
      (goto-char (point-min))
      (widen)
      (org-backward-heading-same-level 1))
    (org-narrow-to-subtree))

  (defconst org-journal-dir "~/journal/org")
  (defconst org-journal-file (concat org-journal-dir "/journal.org"))
  (defconst org-diary-file (concat org-journal-dir "/dagbok.org.gpg"))
  (defconst org-todo-file (concat org-journal-dir "/gjøremål.org"))
  (defconst org-worklog-file (concat org-journal-dir "/arbeidslogg.org"))
  (defconst promo-org-dir "~/munch/pmx")
  (defconst promo-pub-dir "~/munch/pmx")
  (defconst skaperverkstedet-org-dir "~/munch/skaperverkstedet/org")
  (defconst skaperverkstedet-pub-dir "~/munch/skaperverkstedet")
  (defconst org-promo-todo-file (concat  promo-org-dir "/pmx.org"))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (dot . t)
     (emacs-lisp . t)
     (jupyter . t)
     (lilypond . t)))

  (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
						      (:session . "py")
						      (:tangle . "temp.py")
						      (:kernel . "python3")
						      (:eval . "never-export")
						      (:exports . "both")))

  (org-babel-jupyter-override-src-block "python")

  (setq org-babel-min-lines-for-block-output 1)

  (add-to-list 'org-structure-template-alist '("p" . "src python"))
  (add-to-list 'org-structure-template-alist '("b" . "src bibtex"))
  (add-to-list 'org-modules 'org-habit t)

  (setq org-startup-indented t
	org-image-actual-width nil
	org-list-allow-alphabetical t
	org-archive-location "~/media/archive/%s_archive::"
	org-archive-file-header-format nil)

  (setq org-clock-persist t)

  (setq org-babel-python-command "python"
	org-confirm-babel-evaluate nil
	org-indent-indentation-per-level 2
	org-adapt-indentation nil
	org-src-fontify-natively t
	org-src-preserve-indentation t
	org-src-tab-acts-natively t
	org-edit-src-content-indentation 0
	org-edit-src-turn-on-auto-save t
	org-src-window-setup 'current-window)

  (setq python-indent-guess-indent-offset-verbose nil)

  (setq org-export-with-section-numbers nil
	org-export-with-toc nil
	org-footnote-section "Fotnoter"
	org-export-coding-system 'utf-8
	org-html-preamble nil
	org-html-postamble nil
	org-reverse-note-order t)

  (setq org-todo-keywords
	'((sequence "TODO" "|" "DONE" "CANCELED")
	  (sequence "." "PLANLAGT" "|" "AVHOLDT")))
    
  (setq	org-agenda-skip-deadline-if-done t
	org-agenda-skip-scheduled-if-done t
	org-agenda-skip-timestamp-if-done t
	org-agenda-window-setup 'current-window)
  
  (setq org-refile-targets '((nil . (:maxlevel . 2))
			     (org-agenda-files . (:maxlevel . 2))))


  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)


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
	     (org-open-file
	      (format "../../py/%s" path)))
   :export (lambda (path desc backend)
	     (cond
	      ((eq backend 'html)
	       (format "<a href=\"/py/%s\">%s</a>" path (or desc path))))))

  (org-link-set-parameters
   "pdf"
   :follow (lambda (path)
	     (shell-command
	      (format "zathura ../pdf/%s &" path)))
   :export (lambda (path desc backend)
	     (cond
	      ((eq backend 'html)
	       (format "<a href=\"pdf/%s\">%s</a>" path (or desc path))))))

  (org-link-set-parameters
   "data"
   :follow (lambda (path)
	     (org-open-file-with-emacs
	      (format "../data/%s" path)))
   :export (lambda (path desc backend)
	     (cond
	      ((eq backend 'html)
	       (format "<a href=\"data/%s\">%s</a>" path (or desc path))))))

  (org-link-set-parameters
   "fig"
   :follow (lambda (path)
	     (org-open-file-with-emacs
	      (format "../figurer/%s" path)))
   :export 'taba-org-export-html)

  (org-link-set-parameters
   "pres"
   :follow (lambda (path)
	     (shell-command
	      (format "evince -s ../pdf/%s &" path)))
   :export (lambda (path desc backend)
	     (cond
	      ((eq backend 'html)
	       (format "<a href=\"pdf/%s\">%s</a>" path (or desc path))))))

  (org-link-set-parameters
   "yt"
   :follow (lambda (path)
	     (shell-command
	      (format "chromium https://www.youtube.com/watch?v=%s" path)))
   :export (lambda (path desc backend)
	     (cond
	      ((eq backend 'html)
	       (format "<iframe width=\"560\" height=\"315\" src=\"https://www.youtube.com/embed/%s\" frameborder=\"0\" allow=\"accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture\" allowfullscreen></iframe>" path (or desc path))))))


  (org-link-set-parameters
   "inkscape"
   :follow (lambda (path)
	     (shell-command
	      (format "inkscape ../inkscape/%s &" path)))
   :export (lambda (path desc backend)
	     (async-shell-command (format "inkscape --export-plain-svg --export-filename=../svg/%s ../inkscape/%s" path path))
	     (format "<figure>\n<img src=\"svg/%s\"></figure>" path)))

  (org-link-set-parameters
   "xopp"
   :follow (lambda (path)
	     (async-shell-command
	      (format "xournalpp ../xopp/%s.xopp" path)))
   :export (lambda (path desc backend)
	     (async-shell-command (format "xournalpp --create-img ../svg/%s.svg ../xopp/%s.xopp" path path))
	     (format "<figure>\n<img src=\"svg/%s.svg\"/></figure>" path)))
))


  (setq org-file-apps '((auto-mode . emacs)
			("\\.mm'" . default)
			("\\.x?html?\\'" . default)
			("\\.pdf\\'" . "zathura %s")
			("\\.pdf::\\([0-9]+\\)\\'" . "zathura -P %1 %s")
			("\\.xopp" . "xournalpp %s")))

  (setq holiday-bahai-holidays nil
	holiday-hebrew-holidays nil
	holiday-islamic-holidays nil
	holiday-oriental-holidays nil
	holiday-other-holidays nil
	holiday-local-holidays nil
	holiday-christian-holidays nil
	holiday-general-holidays nil
	holiday-solar-holidays nil)
  
  (setq calendar-week-start-day 1
	calendar-day-name-array ["søndag" "mandag" "tirsdag" "onsdag" "torsdag" "fredag" "lørdag"]
	calendar-month-name-array ["januar" "februar" "mars" "april"
                                 "mai" "juni" "juli" "august"
                                 "september" "oktober" "november" "desember"]

	calendar-intermonth-text 
	'(propertize
	  (format "%2d"
		  (car
		   (calendar-iso-from-absolute
		    (calendar-absolute-from-gregorian (list month day year)))))
	  'font-lock-face 'font-lock-function-name-face))

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

  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'org-mode-hook 'visual-fill-column-mode))

(use-package org-attach
  :ensure nil
  :after org
  :bind (("C-c s" . taba-org-screenshot))
  :config

  (setq org-attach-expert t)
  (defun taba-org-screenshot ()
    "Take a screenshot into a time-stamped uniquely named file

The screenshot is saved as an attachment."
    (interactive)
    (let* ((folder "/tmp/")
	   (filename (concat "screenshot_"
			     (format-time-string "%Y%m%d_%H%M%S")
			     ".png"))
	   (fullname (concat folder filename)))
      (call-process "import" nil nil nil fullname)
      (org-attach-attach fullname nil 'mv))))

(use-package org-capture
  :ensure nil
  :after org
  :custom
  (org-capture-templates
   `(("a" "Avtale" entry
      (file+headline org-todo-file "Avtaler")
      "* %^{Avtale} %^G\n%^T\n%?\n"
      :empty-lines 1 :immediate-finish t)
     ("g" "Gjøremål generelt" entry
      (file+headline org-todo-file "Gjøremål")
      "* TODO %?\n%T\n"
      :empty-lines 1)
     ("j" "Journal" entry
      (file+olp+datetree org-journal-file "Journal")
      "* %^{Tema} %^g\n%i%?\n\nSkrevet %U."
      :empty-lines 1 :immediate-finish t :jump-to-captured t)
     ("d" "Dagbok" entry
      (file org-diary-file)
      "* %<%d.%m.%Y> -- %?\n\n"
      :empty-lines 1)
     ))
  :config
  (defun org-capture-turn-off-header-line-hook ()
    (setq-local header-line-format nil))
  (add-hook 'org-capture-mode-hook 'delete-other-windows)
  (add-hook 'org-capture-mode-hook #'org-capture-turn-off-header-line-hook))

(use-package org-ref
  :after org
  :config
  (setq reftex-default-bibliography '("~/journal/org/bibliografi.bib"))
  (setq org-ref-bibliography-notes "~/journal/org/bibliografi.org"
	org-ref-default-bibliography '("~/journal/org/bibliografi.bib")
	org-ref-pdf-directory "~/documents/")
  (bibtex-set-dialect))  ; see https://emacs.stackexchange.com/questions/46691/initialization-of-bibtex-package2

(use-package orgit
  :after (org magit))

;; Du finner relevante options her https://orgmode.org/manual/Publishing-options.html
(use-package ox-publish
  :ensure org
  :after org
  :config

  ;;   (defun my-html-src-block (src-block contents info)
  ;;       "Transcode a SRC-BLOCK element from Org to ASCII.
  ;; CONTENTS is nil.  INFO is a plist used as a communication
  ;; channel."
  ;;   (if (not (org-export-read-attribute :attr_html src-block :skulpt))
  ;;       (org-export-with-backend 'html src-block contents info)
  ;;     (concat
  ;;      (format ",--[ %s ]--\n%s`----"
  ;;              (org-element-property :language src-block)
  ;;              (replace-regexp-in-string
  ;;               "^" "| "
  ;;               (org-element-normalize-string
  ;;                (org-export-format-code-default src-block info)))))))


  ;; Vedrørende bruk av processing
  ;; https://gist.github.com/s-cork/3397e26bc62e9d82a339372d1f613299
  (defun skulpt-html-src-block (src-block contents info)
    "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
    (if (not (org-export-read-attribute :attr_html src-block :skulpt))
	(org-export-with-backend 'html src-block contents info)
      (concat
       (format "<div class=\"%s\" id=\"%s\"><textarea id=\"%s\" cols=\"85\" rows=\"35\">\n%s</textarea>\n<a id=\"%s\">Kjør</a>
<a id=\"%s\">Lagre</a>
%s
<pre id=\"%s\"></pre>%s</div>"
	       "skulpt"
	       (org-element-property :name src-block)
	       (concat (org-element-property :name src-block) "-code")
	       (org-element-normalize-string
		(org-export-format-code-default src-block info))
	       (concat (org-element-property :name src-block) "-run")
	       (concat (org-element-property :name src-block) "-save")
	       (if (org-export-read-attribute :attr_html src-block :turtle)
		   (format "<a id=\"%s\">Figur</a>"
			   (concat (org-element-property :name src-block) "-saveCanvas"))
		 "")
	       (concat (org-element-property :name src-block) "-output")
	       (if (org-export-read-attribute :attr_html src-block :turtle)
		   (format "<div id=\"%s\" height=\"600\" width=\"800\"></div>"
			   (concat (org-element-property :name src-block) "-canvas"))
		 "")))))

  (org-export-define-derived-backend 'my-html 'html
    :translate-alist '((src-block . skulpt-html-src-block)))

  ;; (org-export-to-buffer 'my-html "*Org MY-HTML Export*")


  ;; https://emacs.stackexchange.com/questions/27060/embed-image-as-base64-on-html-export-from-orgmode
  (defun replace-in-string (what with in)
    (replace-regexp-in-string (regexp-quote what) with in nil 'literal))

  (defun org-html--format-image (source attributes info)
    (progn
      (setq source (replace-in-string "%20" " " source))
      (format "<img src=\"data:image/%s;base64,%s\"%s />"
	      (or (file-name-extension source) "")
	      (base64-encode-string
	       (with-temp-buffer
		 (insert-file-contents-literally source)
		 (buffer-string)))
	      (file-name-nondirectory source))))

  (defun my-org-html-publish-to-html (plist filename pub-dir)
    "Publish an org file to HTML.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
    (org-publish-org-to 'my-html filename
			(concat (when (> (length org-html-extension) 0) ".")
				(or (plist-get plist :html-extension)
				    org-html-extension
				    "html"))
			plist pub-dir))
  
  (defun konturer-publish-and-push (plist filename pub-dir)
    (my-org-html-publish-to-html plist filename pub-dir)
    (let ((default-directory "~/repos/tarjeiba.github.io"))
      (shell-command "git add .")
      (shell-command (format-time-string "git commit -m \"%Y%m%d-%H%M%S\""))
      (shell-command "git push")))

  (setq org-html-head-include-default-style nil)

  (setq konturer-html-preamble "<link rel=\"icon\" type=\"image/x-icon\" href=\"/promo/favicon.ico\">
<link rel=\"stylesheet\" type=\"text/css\" href=\"/promo/stylesheet.css\">
<link href='https://fonts.googleapis.com/css?family=Open Sans' rel='stylesheet'>
<link href='https://fonts.googleapis.com/css?family=Source Code Pro' rel='stylesheet'>
<script src=\"https://cdnjs.cloudflare.com/ajax/libs/svg.js/3.0.16/svg.min.js\" type=\"text/javascript\"></script>
<script src=\"/js/skulpt.min.js\" type=\"text/javascript\"></script>   
<script src=\"/js/skulpt-stdlib.js\" type=\"text/javascript\"></script>
<script src=\"/js/editor.js\" type=\"text/javascript\"></script>       
<script src=\"/js/codemirror_python.js\"></script>                   
<script src=\"/js/codemirror.js\"></script>                          
<link rel=\"stylesheet\" href=\"/css/codemirror.css\">                 
<link rel=\"stylesheet\" media=\"screen\" href=\"/css/main.css\">                 
<link rel=\"stylesheet\" media=\"print\" href=\"/css/print.css\">                 
")

  (setq org-publish-project-alist
	`(("promo"
	   :base-directory ,promo-org-dir
	   :exclude-tags ("pdf" "noexport")
	   :publishing-directory ,promo-pub-dir
	   :publishing-function org-html-publish-to-html
	   :recursive t
	   :headline-levels 6
	   :html-preamble "<link rel=\"icon\" type=\"image/x-icon\" href=\"/promo/favicon.ico\">
<link rel=\"stylesheet\" type=\"text/css\" href=\"/promo/stylesheet.css\">
<link href='https://fonts.googleapis.com/css?family=Open Sans' rel='stylesheet'>
<link href='https://fonts.googleapis.com/css?family=Source Code Pro' rel='stylesheet'>"
	   :auto-sitemap nil
	   :exclude "tanker-og-todos.org\\|orgheader.org\\|orgheader_nojs.org\\|sitemap.org\\|README.org"
	   :with-smart-quotes nil
	   :with-emphasize t
	   :with-special t
	   :with-fixed-width t
	   :with-timestamps t
	   :preserve-breaks nil
	   :with-sub-superscript nil
	   :with-archived-trees nil
	   :with-date nil
	   :with-entities t
	   :with-footnotes t
	   :with-inline-tasks t
	   :with-planning nil
	   :with-priority nil
	   :section-numbers nil
	   :creator "<a href=\"http://www.gnu.org/software/emacs/\">Emacs</a> 26.3 (<a href=\"http://orgmode.org\">Org</a> mode 9.1.14)"
	   :author "Tarjei Bærland"
	   :email "tarjei.barland@osloskolen.no"
	   :languaoge "no")

	  ("skaperverkstedet"
	   :base-directory ,skaperverkstedet-org-dir
	   :base-extension "org"
	   :exclude-tags ("noexport")
	   :publishing-directory ,skaperverkstedet-pub-dir
	   :publishing-function org-html-publish-to-html
	   :recursive t
	   :headline-levels 6
	   :html-preamble skaperverkstedet-html-preamble
	   :html-postamble skaperverkstedet-html-postamle
	   :auto-sitemap t
	   :sitemap-function skaperverkstedet-sitemap-function
	   :sitemap-filename "_sitemap.org"
	   :sitemap-title "Skaperverkstedet"
	   :exclude "_opplastningsinfo.org\\|_timeplan.org")

	  ("skaperverkstedet-static"
	   :base-directory "~/repos/templates"
	   :base-extension "css"
	   :publishing-directory ,skaperverkstedet-pub-dir
	   :publishing-function org-publish-attachment)

	  ("konturer-org"
	   :base-directory "~/repos/konturer/org"
	   :base-extension "org"
	   :publishing-directory "~/repos/konturer"
	   :publishing-function my-org-html-publish-to-html
	   :exclude: "konturer_header.org")

	  ("konturer-static"
	   :base-directory "~/repos/konturer/org"
	   :base-extension "gif"
	   :publishing-directory "~/repos/konturer"
	   :recursive t
	   :publishing-function org-publish-attachment)

	  ("konturer"
	   :components ("konturer-org"))
	  ))


  (add-to-list 'org-src-lang-modes '("svgjs" . javascript))
  (defvar org-babel-default-header-args:svgjs
    '((:results . "html")
      (:exports . "results")))

  (defun org-babel-execute:svgjs (body _params)
    (message "start")
    (message (format "I am %s" (nth 4 (org-babel-get-src-block-info))))
    (format "<script type=\"text/javascript\">\n%s\n</script>" body))

  (customize-set-variable 'org-babel-exp-code-template
			  (concat "\n=%name=:\n"
				  org-babel-exp-code-template))



;; https://svgjs.com/docs/3.0/getting-started/
;; https://cdnjs.com/libraries/svg.js
;; https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-js.html
;; https://emacs.stackexchange.com/questions/28301/export-javascript-source-block-to-script-tag-in-html-when-exporting-org-file-to
  (defun tarjeiba-read-file-contents (filename)
    (with-temp-buffer
      (insert-file-contents filename)
      (buffer-string)))

  (defun skaperverkstedet-sitemap-function (title list)
    (concat "#+TITLE: " title "\n\n"
	    (org-list-to-org list)))

  (defun skaperverkstedet-html-preamble (options)
    (let ((html-buffer "*html-export-preamble*")
	  (sitemap-file (expand-file-name (plist-get options :sitemap-filename)
					  (plist-get options :base-directory))))
      (with-temp-buffer
	(insert-file-contents sitemap-file)
	(org-export-to-buffer 'html html-buffer nil nil nil t nil nil))

      (with-current-buffer html-buffer
	(goto-char (point-min))
	(insert "<div class=\"navbar\">")
	(goto-char (point-max))
	(insert "</div>")
	(buffer-string))))

  (defun skaperverkstedet-html-postamle (options)
    (concat 
     (format "<p class=\"date\">Endret %s"
	     (format-time-string "%Y-%m-%d %H:%M:%S"))
     (format " av <a href=\"mailto:%s\">%s</a>" (plist-get options :email) (car (plist-get options :author)))
     (format " via %s." (plist-get options :creator))
     "</p>"))
  
  (with-eval-after-load 'ox-html
    (add-to-list 'org-html-infojs-options '(sdepth . "1"))
    (setq org-html-htmlize-output-type 'css)
    (setq org-html-use-infojs 'when-configured))


  (setq org-format-latex-options
	'(:foreground default :background default :scale 1.5 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
		 ("begin" "$1" "$" "$$" "\\(" "\\[")))
  (setq org-html-footnotes-section (concat "<div id=\"footnotes\">\n"
					   "<h2 class=\"footnotes\" hidden>%s</h2>\n"
					   "<hr>\n"
					   "<div id=\"text-footnotes\">\n"
					   "%s\n</div>\n</div>")))

(use-package ledger-mode
  :ensure t)

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  (setenv "GIT_ASKPASS" "git-gui--askpass")
  (setq magit-branch-read-upstream-first 'fallback
	magit-dispatch-arguments nil
	magit-remote-arguments '("f")))

(use-package jupyter
  :ensure t
  :bind (("C-c C-v S" . jupyter-org-split-src-block)))

(use-package which-key
  :ensure t
  :config (which-key-mode t))

;; (use-package fill-column-indicator
;;   :ensure t
;;   :config
;;   (setq fci-rule-color "#d6d6d6"
;; 	fci-rule-width 1))

(use-package arduino-mode
  :ensure t)

(use-package vterm
  :ensure t)

(use-package js
  :ensure t
  :config
  (setq js-indent-level 2))

(use-package typescript-mode
  :ensure t
  :config
  (setq typescript-indent-level 2))

(use-package lsp-mode
  :ensure t)

(use-package lsp-ivy
  :ensure t)
  

(use-package lua-mode
  :ensure t)

(use-package dired-x
  :ensure nil
  :config
  (setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$"))
  (setq dired-listing-switches "-alh")
  (setq dired-dwim-target t)
  (setq dired-omit-mode t)
  (add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))
  (setq dired-guess-shell-alist-user
      '(("\\.pdf$" "zathura")
        ("\\.flv$" "mpv")
        ("\\.mov$" "mpv")
        ("\\.3gp$" "mpv")
        ("\\.png$" "feh")
        ("\\.jpg$" "feh")
        ("\\.JPG$" "feh")
        ("\\.avi$" "mpv"))))

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install :no-query)
  (setq pdf-view-midnight-colors (cons (face-attribute 'default :foreground)
				       (face-attribute 'default :background)))
  (add-hook 'pdf-tools-enabled-hook 'pdf-view-midnight-minor-mode))

(defun tob64 (filename)
  (base64-encode-string
   (with-temp-buffer
     (insert-file-contents filename)
     (buffer-string))))

(defun taba-insert-encoded-image-link (filename)
  (insert (format "<img src=\"data:image/png;base64,%s\">"
		  (tob64 filename))))


(setq user-mail-address "tarjei@purelymail.com"
      user-full-name "Tarjei Bærland")

(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(defun taba-org-export-html (link desc backend)
  (cond
   ((eq backend 'html)
    (progn (message (number-to-string (org-element-property :begin (org-element-at-point))))
	   (format (concat "<img src=\"figurer/%s\""
  		    " alt=\"%s\"/>")
  		   link (or desc link))))
   ((eq backend 'latex)
    (format
     "\\begin{figure}\n\\includegraphics[width=\\linewidth]{%s}\n\\label{fig:boat1}\n\\end{figure}"
     link))))

(use-package mu4e
  :ensure nil
  :init (setq mu4e-view-use-gnus t)
  :config
  (setq mu4e-maildir "~/media/mail/purely"
	mu4e-sent-folder "/Sent"
	mu4e-drafts-folder "/Drafts"
	mu4e-trash-folder "/Trash"
	mu4e-refile-folder "/Archive"
	mu4e-update-interval 1800)
  (setq mu4e-get-mail-command "offlineimap"))
    
(use-package org-mu4e
  :ensure nil)

(use-package writeroom-mode
  :ensure t)

(setq smtpmail-smtp-server "smtp.purelymail.com"
      smtpmail-smtp-service 587)


(use-package haskell-mode
  :ensure t)

 ;; (use-package emms
 ;;  :ensure t
 ;;  :config
 ;;  (require 'emms-setup)
 ;;  (emms-minimalistic)
 ;;  (setq emms-player-list '(emms-player-mpv)))


 ;;  (emms-default-players)
 ;;  (setq emms-source-file-default-directory "~/media/musikk/")
 ;;  (setq emms-playlist-buffer-name "*Music*")
 ;;  (setq emms-info-asynchronously t)
 ;;  (require 'emms-info-libtag) ;;; load functions that will talk to emms-print-metadata which in turn talks to libtag and gets metadata
 ;;  (setq emms-info-functions '(emms-info-libtag)) ;;; make sure libtag is the only thing delivering metadata
 ;;  (require 'emms-playing-time)
 ;;  (emms-playing-time 1))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   '(vector "#ffffff" "#f36c60" "#8bc34a" "#fff59d" "#4dd0e1" "#b39ddb" "#81d4fa" "#263238"))
 '(custom-safe-themes
   '("72e0b2fcc58a2bc2cfe588b27853f7dea7db6d2bda7d481e1e348b5a15f69a18" "bf511364b090a19300634da43adb41144180e3c7926a4b0097155a8b33c1f6f2" "a8ad2c75696c9b97b59372c92aec60fee9059b3e9dafb8888c5ce0b64362f584" "732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" "fe5e8725280cd104da1ee0488d88f896628f509cd17ecfdf54741ba63d52eff1" "f3fb21a7e3695c47631c5c488b2c70fa488ea42ab2525460698a5df18e925d80" "b554a4750d2ed87c2ecf97eea9ef7ad4e413324e3b273632552ee2489bda74b5" "4b934e6ec7a295c1af3ea0a194d38b54a3b93e4edb8a9fe957ed12076bc32dce" "883f6d34c0bac676833397f4b4d9ef1f98df2b566fe95bddb1da88bb08fe7245" "1b1c04792dae7e9cebc27aa5aa3ba7516250b440364a8d08cc03aff566032387" "ee11d22f12ee5113edf2b81738d027125b9e38d926ee8f61ea577b47a1330233" "1a5f3ceefb834fad698866ce5606de03cd9a9af13e66b351b86d94367a305d7b" "bf798e9e8ff00d4bf2512597f36e5a135ce48e477ce88a0764cfb5d8104e8163" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" default))
 '(fci-rule-color "#37474f")
 '(gnus-group-tool-bar 'gnus-group-tool-bar-gnome)
 '(grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "dist"))
 '(ledger-reports
   '(("bal" "ledger [[ledger-mode-flags]] -f /home/tarjei/journal/pengejournal.dat bal")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)")))
 '(org-agenda-files
   '("~/munch/pmx/fysikkmotor/fysikkmotor.org" "~/journal/org/bibliografi.org" "~/journal/org/jensbjelkes.org" "~/journal/org/fjellgata.org" "~/munch/tekfors/tekfors.org" "~/munch/skaperverkstedet/skaperverkstedet.org" "~/munch/r2/r2.org" "~/munch/pmx/pmx.org" "~/kikora/kikora.org" "~/munch/munch.org" "~/journal/org/arbeidsflyt.org" "~/journal/org/journal.org" "~/journal/org/gjøremål.org" "~/journal/org/merkedager.org"))
 '(org-id-link-to-org-use-id 'create-if-interactive)
 '(org-log-into-drawer t)
 '(org-log-note-headings
   '((done . "CLOSING NOTE %t")
     (state . "State %-12s from %-12S %t")
     (note . "%t")
     (reschedule . "Rescheduled from %S on %t")
     (delschedule . "Not scheduled, was %S on %t")
     (redeadline . "New deadline from %S on %t")
     (deldeadline . "Removed deadline, was %S on %t")
     (refile . "Refiled on %t")
     (clock-out . "")))
 '(org-ref-clean-bibtex-entry-hook
   '(org-ref-bibtex-format-url-if-doi orcb-key-comma orcb-& orcb-% org-ref-title-case-article orcb-clean-year orcb-key orcb-clean-doi orcb-clean-pages orcb-check-journal org-ref-sort-bibtex-entry orcb-fix-spacing))
 '(package-selected-packages
   '(lua-mode centered-window-mode lsp-ivy ivy-lsp emms ox-ipynb writeroom-mode writeroom ledger-mode org-ref org-mu4e orgit counsel mu4e vterm typescript-mode exwm arduino-mode arduino flycheck flycheck-mode dired dired-x ob-shell jupyter which-key visual-fill-column use-package pdf-tools org-plus-contrib magit htmlize darkroom ag))
 '(send-mail-function 'smtpmail-send-it)
 '(tex-fontify-script nil)
 '(vc-follow-symlinks t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(subscript ((t (:height tex-suscript-height))))
 '(tex-verbatim ((t (:background "dark grey")))))
(put 'magit-edit-line-commit 'disabled nil)
