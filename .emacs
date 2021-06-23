(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(add-to-list 'default-frame-alist '(internal-border-width . 30))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(add-to-list 'default-frame-alist '(font . "Iosevka-12"))
(setenv "BASH_ENV" "$HOME/.bashrc")

(setq-default fill-column 100)
(setq recentf-max-menu-items 100
      recentf-max-saved-items 100
      inhibit-startup-screen t
      inhibit-splash-screen t
      initial-scratch-message nil
      text-scale-mode-step 1.1
      column-number-mode t
      ediff-window-setup-function 'ediff-setup-windows-plain)
(setq vc-follow-symlinks t)
(setq inhibit-x-resources t)
(setq backup-directory-alist `(("." . "~/media/archive"))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      create-lockfiles nil)
(setq auto-save-file-name-transforms `((".*" "~/media/emacs-autosaves/" t)))
(setq display-buffer-alist
      '(("*Async Shell Command*" display-buffer-no-window)))
(setq smtpmail-smtp-server "smtp.purelymail.com"
      smtpmail-smtp-service 587)
(setq user-mail-address "tarjei@purelymail.com"
      user-full-name "Tarjei Bærland")

(run-at-time (current-time) (* 5 60)
             (lambda ()
               (let ((save-silently t))
                 (recentf-save-list))))

(defalias 'yes-or-no-p 'y-or-n-p)

(recentf-mode 1)
(prefer-coding-system 'utf-8)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(display-time-mode 1)

(global-set-key (kbd "<f5>") 'menu-bar-mode)
(global-set-key (kbd "<f9>") (lambda () (interactive) (revert-buffer t t)))

(defun taba--filetime (file)
  "Modification time of file."
  (time-convert (file-attribute-modification-time
		 (file-attributes file))
		'integer))

(defun taba-newer-file-p (file1 file2)
  "Whether file `file1' is newer than `file2'."
  (let ((f1time (taba--filetime file1))
	(f2time (taba--filetime file2)))
    (< f1time f2time)))

(defun tob64 (filename)
  (base64-encode-string
   (with-temp-buffer
     (insert-file-contents filename)
     (buffer-string))))

(defun taba-insert-encoded-image-link (filename)
  (insert (format "<img src=\"data:image/png;base64,%s\">"
		  (tob64 filename))))

(defun taba-org-export-html (link desc backend)
  (cond
   ((eq backend 'html)
    (progn (message
	    (number-to-string
	     (org-element-property :begin (org-element-at-point))))
	   (format (concat "<img src=\"figurer/%s\""
  			   " alt=\"%s\"/>")
  		   link (or desc link))))
   ((eq backend 'latex)
    (format
     "\\begin{figure}\n\\includegraphics[width=\\linewidth]{%s}\n\\label{fig:boat1}\n\\end{figure}"
     link))))

(defun enable-doom-modeline-icons (_frame)
  (setq doom-modeline-icon t))

(use-package exec-path-from-shell
  :config
  (when (daemonp)
    (exec-path-from-shell-initialize)))

(use-package jupyter
  :bind (("C-c C-v S" . jupyter-org-split-src-block)))

(use-package org
  :mode ("\\.org" . org-mode)
  :hook
  (org-mode . visual-line-mode)
  :bind (("C-c l" . org-store-link)
	 ("C-c c" . counsel-org-capture)
	 ("C-c a" . org-agenda)
	 ("C-c A" . org-agenda-list)
	 ("C-c C-x C-j" . org-clock-goto)
	 :map org-mode-map
	 ("C-c n" . org-advance)
	 ("C-c p" . org-retreat))
  :custom
  (org-id-link-to-org-use-id 'create-if-interactive)
  (org-log-into-drawer t)
  (org-export-default-language "no")
  (org-footnote-section "Fotnoter")
  (org-clock-persist t)
  (org-confirm-babel-evaluate nil)
  (org-indent-indentation-per-level 2)
  (org-startup-indented t)
  (org-image-actual-width nil)
  (org-archive-file-header-format nil)
  (org-archive-location "~/media/archive/%s_archive::")
  (org-list-allow-alphabetical t)
  (org-adapt-indentation nil)
  (org-src-fontify-natively t)
  (org-src-preserve-indentation t)
  (org-src-tab-acts-natively t)
  (org-edit-src-content-indentation 0)
  (org-edit-src-turn-on-auto-save t)
  (org-src-window-setup 'current-window)
  (org-export-with-section-numbers nil)
  (org-export-with-toc nil)
  (org-export-coding-system 'utf-8)
  (org-html-preamble nil)
  (org-html-postamble nil)
  (org-reverse-note-order t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-timestamp-if-done t)
  (org-agenda-window-setup 'current-window)
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-todo-keywords
   '((sequence "TODO" "|" "DONE" "CANCELED")
     (sequence "." "PLANLAGT" "|" "AVHOLDT")))
  (org-refile-targets '((nil . (:maxlevel . 2))
			(org-agenda-files . (:maxlevel . 2))))

  :config
  (require 'org-loaddefs)

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

  (require 'ob-js)
  (require 'ob-python)
  (require 'ob-shell)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (dot . t)
     (emacs-lisp . t)
     (jupyter . t)
     (lilypond . t)
     (js . t)))

  (add-to-list 'org-src-lang-modes '("inline-js" . javascript)) ;; js2 if you're fancy
  (defvar org-babel-default-header-args:inline-js
    '((:results . "html")
      (:exports . "results")))
  (defun org-babel-execute:inline-js (body _params)
    (format "<script type=\"text/javascript\">\n%s\n</script>" body))

  (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
						       (:session . "py")
						       (:tangle . "temp.py")
						       (:kernel . "python3")
						       (:eval . "never-export")
						       (:exports . "both")))

  (setq org-babel-min-lines-for-block-output 1)

  (add-to-list 'org-structure-template-alist '("p" . "src python"))
  (add-to-list 'org-structure-template-alist '("b" . "src bibtex"))
  (add-to-list 'org-structure-template-alist '("t" . "task"))
  (add-to-list 'org-modules 'org-habit t)

  (setq python-indent-guess-indent-offset-verbose nil)

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
	     (format "<figure>\n<img src=\"/svg/%s\"></figure>" path)))

  (org-link-set-parameters
   "xopp"
   :follow (lambda (path)
	     (let ((xoppfile (format "/home/tarjei/repos/konturer/xopp/%s" path))
		   (template "/home/tarjei/repos/konturer/maler/skisse.xopp"))
	       (cond ((not (file-exists-p xoppfile))
		      (copy-file template xoppfile)))
	       (async-shell-command
		(format "xournalpp %s" xoppfile))))
   :export (lambda (path desc backend)
	     (let ((xoppfile (format "/home/tarjei/repos/konturer/xopp/%s" path))
		   (tempfile "/home/tarjei/temp/xournal_export.svg"))
	       (delete-file tempfile)
	       (message "deleted file")
	       (shell-command (format "xournalpp --create-img %s %s" tempfile xoppfile))
	       (format "<figure>\n%s\n</figure>"
		       (with-temp-buffer
			 (insert-file-contents tempfile)
			 (buffer-string))))))

  (defun org-html--format-image (source attributes info)
    (format "<img src=\"data:image/%s;base64,%s\"%s />"
	    (or (file-name-extension source) "")
	    (base64-encode-string
	     (with-temp-buffer
	       (insert-file-contents-literally source)
	       (buffer-string)))
	    (file-name-nondirectory source)))

  (org-link-set-parameters
   "xournalpp"
   :follow (lambda (path)
	     (let ((xoppfile (concat (org-attach-dir-get-create) "/" path))
		   (template "/home/tarjei/repos/konturer/maler/skisse.xopp"))
	       (cond ((not (file-exists-p xoppfile)) (copy-file template xoppfile)))
	       (org-attach-sync)
	       (shell-command (format "xournalpp %s" xoppfile))))
   :export (lambda (path desc backend)
	     (org-attach-sync)
	     (let ((xoppfile (concat (org-attach-dir-get-create) "/" path))
		   (tempfile "/home/tarjei/temp/xournal_export.svg"))
	       (shell-command (format "xournalpp --create-img %s %s" tempfile xoppfile))
	       (with-temp-buffer
		 (insert-file tempfile)p
		 (buffer-string)))))

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
			  (org-agenda-max-entries nil)))))))

  (advice-add 'org-agenda-goto :after
	      (lambda (&rest args)
		(org-narrow-to-subtree)))

  (advice-add 'org-agenda-switch-to :after
	      (lambda (&rest args)
		(org-narrow-to-subtree)))

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
      (org-attach-attach fullname nil 'mv)))

  (setq org-capture-templates
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
	   "* %^{Tema} %^g\n%i\n%?\n\nSkrevet %U."
	   :empty-lines 1 :immediate-finish t :jump-to-captured t)
	  ("d" "Dagbok" entry
	   (file org-diary-file)
	   "* %<%d.%m.%Y> -- %?\n\n"
	   :empty-lines 1)
	  ))

  (defun org-capture-turn-off-header-line-hook ()
    (setq-local header-line-format nil))

  (add-hook 'org-capture-mode-hook 'delete-other-windows)
  (add-hook 'org-capture-mode-hook #'org-capture-turn-off-header-line-hook)


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

  (require 'ox)
  (org-export-define-derived-backend 'my-html 'html
    :translate-alist '((src-block . skulpt-html-src-block)))

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

  (setq konturer-html-preamble "
<link href='https://fonts.googleapis.com/css?family=Open Sans' rel='stylesheet'>
<link href='https://fonts.googleapis.com/css?family=Source Code Pro' rel='stylesheet'>
<script src=\"https://cdnjs.cloudflare.com/ajax/libs/svg.js/3.0.16/svg.min.js\" type=\"text/javascript\"></script>
<script src=\"/js/codemirror.js\" type=\"text/javascript\"></script>                          
<script src=\"/js/codemirror_python.js\" type=\"text/javascript\"></script>                   
<script src=\"/js/skulpt.min.js\" type=\"text/javascript\"></script>   
<script src=\"/js/skulpt-stdlib.js\" type=\"text/javascript\"></script>
<script src=\"/js/editor.js\" type=\"text/javascript\"></script>       
<link rel=\"stylesheet\" href=\"/css/codemirror.css\">                 
<link rel=\"stylesheet\" media=\"screen\" href=\"/css/main.css\">                 
<link rel=\"stylesheet\" media=\"print\" href=\"/css/print.css\">                 
")

	     (setq org-publish-project-alist
		   `(("konturer-skisser"
		      :auto-sitemap t
		      :sitemap-filename "index.org"
		      :sitemap-title "Skisser"
		      :sitemap-file-entry-format "%d - %t"

		      :html-preamble ,konturer-html-preamble
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

		      :language "no"
		      :base-directory "~/repos/konturer/org/skisser"
		      :base-extension "org"
		      :publishing-directory "~/repos/tarjeiba.github.io/skisser"
		      :publishing-function konturer-publish-and-push)

		     ("konturer-org"
		      :auto-sitemap nil
		      :sitemap-filename "index.org"
		      :sitemap-title "Konturer"
		      :sitemap-file-entry-format "%d - %t"

		      :html-preamble ,konturer-html-preamble
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

		      :base-directory "~/repos/konturer/org"
		      :base-extension "org"
		      :publishing-directory "~/repos/tarjeiba.github.io"
		      :publishing-function konturer-publish-and-push
		      :html-preamble ,konturer-html-preamble)
		     
		     ("konturer-pres"
		      :base-directory "~/repos/konturer/org"
		      :base-extension "orgpres"
		      :publishing-directory "~/repos/tarjeiba.github.io"
		      :publishing-function org-reveal-publish-to-reveal)

		     ("konturer-static"
		      :base-directory "~/repos/konturer/org"
		      :base-extension "gif\\|png"
		      :publishing-directory "~/repos/tarjeiba.github.io"
		      :recursive t
		      :publishing-function org-publish-attachment)

		     ("konturer"
		      :components ("konturer-org" "konturer-pres" "konturer-static"))
		     ))
	     

	     (add-to-list 'org-src-lang-modes '("svgjs" . javascript))
	     (defvar org-babel-default-header-args:svgjs
	       '((:results . "html")
		 (:exports . "results")))

	     (defun org-babel-execute:svgjs (body _params)
	       (message "start")
	       (message (format "I am %s" (nth 4 (org-babel-get-src-block-info))))
	       (format "<script type=\"text/javascript\">\n%s\n</script>" body))

	     ;; (setq org-babel-exp-code-template
	     ;; 	(concat
	     ;; 	 "#+BEGIN_CODEBLOCK\n"
	     ;; 	 "\n=%name=:\n"
	     ;; 	 "#+BEGIN_SRC %lang%switches%flags\n"
	     ;; 	 "%body\n"
	     ;; 	 "#+END_SRC\n"
	     ;; 	 "#+END_CODEBLOCK"))

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
						      "%s\n</div>\n</div>"))

	     )

(use-package all-the-icons)

(use-package doom-themes
	     :config
	     (load-theme 'doom-nord t)
	     (doom-themes-org-config))

(use-package doom-modeline
	     :config
	     (doom-modeline-mode 1)
	     (add-hook 'after-make-frame-functions
		       #'enable-doom-modeline-icons))

(use-package counsel
	     :bind (("M-x" . counsel-M-x)
		    ("C-x f" . counsel-recentf)
		    ("C-x C-f" . counsel-find-file))
	     :custom
	     (counsel-find-file-ignore-regexp "__pycache__/"))

(use-package company
	     :after lsp-mode
	     :hook (lsp-mode . company-mode)
	     :bind 
	     (:map company-active-map
		   ("<tab>" . company-complete-selection))
	     (:map lsp-mode-map
		   ("<tab>" . company-indent-or-complete-common))
	     :custom
	     (company-minimum-prefix-length 1)
	     (compnay-idle-delay 0.1))

(use-package company-box
	     :hook (company-mode . company-box-mode))

(use-package ivy
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

(use-package ivy-hydra
	     :after ivy)

(use-package theme-magic)

(use-package projectile
	     :bind-keymap ("C-c p" . projectile-command-map)
	     :init
	     (when (file-directory-p "~/repos")
	       (setq projectile-project-search-path '("~/repos"))))

(use-package counsel-projectile
	     :custom
	     (counsel-projectile-switch-project-action 'counsel-projectile-switch-project-action-dired)
	     :config 
	     (counsel-projectile-mode))

(use-package lsp-python-ms
	     :after lsp
	     :init (setq lsp-python-ms-auto-install-server t)
	     :hook (python-mode . (lambda ()
				    (require 'lsp-python-ms)
				    (lsp))))

(use-package org-appear
	     :after org)

(use-package org-fragtog
	     :after org)

(use-package org-ref
	     :after org
	     :config
	     (setq reftex-default-bibliography '("~/journal/org/bibliografi.bib"))
	     (setq org-ref-bibliography-notes "~/journal/org/bibliografi.org"
		   org-ref-default-bibliography '("~/journal/org/bibliografi.bib")
		   org-ref-pdf-directory "~/documents/")
	     (bibtex-set-dialect))

(use-package orgit
	     :after (org magit))

(use-package ox-reveal
	     :after org)

(use-package ledger-mode
	     :config
	     (setq ledger-add-transaction-prompt-for-text nil))

(use-package magit
	     :bind ("C-x g" . magit-status)
	     :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
	     :config
	     (setenv "GIT_ASKPASS" "git-gui--askpass")
	     (setq magit-branch-read-upstream-first 'fallback
		   magit-dispatch-arguments nil
		   magit-remote-arguments '("f"))
	     (put 'magit-edit-line-commit 'disabled nil))

(use-package forge
	     :after magit)

(use-package which-key
	     :config (which-key-mode t))

(use-package flycheck)

(use-package arduino-mode)

(use-package vterm)

(use-package js
	     :config
	     (setq js-indent-level 2))

(use-package typescript-mode
	     :mode "\\.ts\\'"
	     :hook (typescript-mode . lsp-deferred)
	     :config
	     (setq typescript-indent-level 2))

(use-package lsp-mode
	     :commands (lsp lsp-deferred)
	     :init
	     (setq lsp-keymap-prefix "C-c q")
	     :config
	     (lsp-enable-which-key-integration t))

(use-package lua-mode)

(use-package dired-x
	     :straight nil
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
	     :config
	     (pdf-tools-install :no-query)
	     (setq pdf-view-midnight-colors (cons (face-attribute 'default :foreground)
						  (face-attribute 'default :background)))
	     (add-hook 'pdf-tools-enabled-hook 'pdf-view-midnight-minor-mode))

(use-package mu4e
	     :init (setq mu4e-view-use-gnus t)
	     :custom ((mu4e-compose-format-flowed t))
	     :config
	     (setq mu4e-maildir "~/media/mail/purely"
		   mu4e-sent-folder "/Sent"
		   mu4e-drafts-folder "/Drafts"
		   mu4e-trash-folder "/Trash"
		   mu4e-refile-folder "/Archive"
		   mu4e-update-interval 1800)
	     (setq mu4e-get-mail-command "offlineimap"))

(use-package darkroom
	     :custom (darkroom-text-scale-increase 1.2))

(use-package emms
	     :config
	     (require 'emms-player-simple)
	     (require 'emms-source-file)
	     (require 'emms-source-playlist)
	     (require 'emms-history)
	     (emms-history-load)
	     (emms-all)
	     (setq emms-playlist-buffer-name "*Music*")
	     (setq emms-source-file-default-directory "/home/tarjei/media/musikk")
	     (setq emms-player-list '(emms-player-mpv)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   '(vector "#ffffff" "#f36c60" "#8bc34a" "#fff59d" "#4dd0e1" "#b39ddb" "#81d4fa" "#263238"))
 '(doom-modeline-mode t)
 '(elfeed-feeds '("https://ciechanow.ski/atom.xml"))
 '(fci-rule-color "#37474f")
 '(fringe-mode 0 nil (fringe))
 '(gnus-group-tool-bar 'gnus-group-tool-bar-gnome)
 '(grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "dist"))
 '(ledger-reports
   '(("bal" "ledger [[ledger-mode-flags]] -f /home/tarjei/journal/pengejournal.dat bal --cleared")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)")))
 '(org-agenda-diary-file "~/journal/org/dagbok.org.gpg")
 '(org-agenda-files
   '("/home/tarjei/repos/konturer/battleships/battleships.org" "/home/tarjei/kikora/oppgaver.org" "/home/tarjei/repos/konturer/org/skisser/20210412a.org" "/home/tarjei/repos/konturer/org/sir-modellen.org" "/home/tarjei/repos/konturer/org/index.org" "/home/tarjei/journal/org/journal.org" "/home/tarjei/journal/org/bibliografi.org" "/home/tarjei/journal/org/jensbjelkes.org" "/home/tarjei/journal/org/fjellgata.org" "/home/tarjei/kikora/kikora.org" "/home/tarjei/journal/org/arbeidsflyt.org" "/home/tarjei/journal/org/gjøremål.org" "/home/tarjei/journal/org/merkedager.org"))
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
 '(safe-local-variable-values '((projectile-project-run-cmd . "ng serve --open")))
 '(send-mail-function 'smtpmail-send-it)
 '(tex-fontify-script nil)
 '(vc-follow-symlinks t))

(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(set-register ?e '(file . "~/.emacs"))
(set-register ?j '(file . "~/journal/org/journal.org"))
(set-register ?p '(file . "~/journal/pengejournal.dat"))
(set-register ?b '(file . "~/journal/org/bibliografi.org"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
