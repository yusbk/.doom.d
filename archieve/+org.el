;;; org.el -*- lexical-binding: t; -*-

;;; Directories
;; Other packages to make org-mode nicer
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!

(when IS-LINUX
  (setq org-onedrive (concat hdir-dir-c onedrive)))

(when IS-WINDOWS
  (setq org-onedrive onedrive))

(setq org-directory (concat org-onedrive "org/"))

;; Need to create the org files manually with #+FILETAGES:
(setq my-org-agenda-directory (file-truename (concat org-directory "gtd/")))

;;; Global variables
(defvar my-agenda-inbox (expand-file-name "inbox.org" my-org-agenda-directory)
  "Unstructured capture")
(defvar my-agenda-work (expand-file-name "work.org" my-org-agenda-directory)
  "Work related")
(defvar my-agenda-private (expand-file-name "private.org" my-org-agenda-directory)
  "Private related")
(defvar my-org-roam (expand-file-name "org-roam/" org-directory)
  "Notes and references using org-roam" )

;; PDF files
(defvar my-reference-pdf (expand-file-name "references/" my-org-roam)
  "Reference as PDF files")
(defvar my-reference-notes (expand-file-name "notes/" my-org-roam)
  "Reference notes on PDF files")
(defvar my-bibtex-file (expand-file-name "bibtex/library.bib" my-org-roam)
  "Reference library file")


;;; General Settings
(after! org
  ;; (add-hook! org-load
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "START(s)" "|" "DONE(d)")
          (sequence "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

  ;;Tetapkan warna keyword
  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("START" :foreground "orange" :weight bold)
                ("NEXT" :foreground "purple" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
                ("HOLD" :foreground "magenta" :weight bold)
                ("CANCELLED" :foreground "gray" :weight bold)
                )))

  ;; ditaa.jar can be downloaded from https://ditaa.sourceforge.net/
  (setq
   ;; org-babel-mathematica-command "~/.local/bin/mash"
   ;; org-ditaa-jar-path (concat (getenv "HOME") "/.doom.d/bin/ditaa.jar")
   org-ditaa-jar-path "~/.doom.d/bin/ditaa.jar"
   org-ditaa-eps-jar-path "~/.doom.d/bin/ditaa.jar"
   plantuml-jar-path "~/.doom.d/bin/plantuml.jar"
   )

  ;; save buffer after change todo state
  (add-hook! 'org-trigger-hook 'save-buffer)

  (setq org-refile-allow-creating-parent-nodes 'confirm)

  ;; To change agenda view, use 'z'
  (setq org-agenda-start-day "-1d"
        org-agenda-span 4
        org-agenda-start-on-weekday 1
        )

  ;;By default, the time grid has a lot of ugly '....' lines. Remove those.
  (setq org-agenda-time-grid
        '((daily today remove-match) (800 1000 1200 1400 1600 1800 2000) "" ""))
  ;; Change separator from == to -
  (setq org-agenda-block-separator ?-)

  (setq org-agenda-current-time-string "<---| now |")
  ;; Make deadline and overdue stand out
  (setq org-agenda-deadline-leaders '("Deadline: " "In %d days: " "Overdue %d day: "))
  ;; Remove done from agenda
  (setq org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-timestamp-if-done t
        org-agenda-hide-tags-regexp "."
        )

  ;; Shortcuts for codeblock using C-c C-,
  (setq org-structure-template-alist '(("a" . "export ascii")
                                       ("c" . "center")
                                       ("C" . "comment")
                                       ("e" . "example")
                                       ("E" . "export")
                                       ("h" . "export html")
                                       ("l" . "export latex")
                                       ("q" . "quote")
                                       ("r" . "SRC R")
                                       ("s" . "SRC")
                                       ("S" . "src sh")
                                       ("v" . "verse")
                                       ("el" . "src emacs-lisp")
                                       ("d" . "definition")
                                       ("t" . "theorem")
                                       ))

  ;; Add babel
  ;; ref https://dotdoom.rgoswami.me/config.html
  (org-babel-do-load-languages 'org-babel-load-languages
                               (append org-babel-load-languages
                                       '((R . t))
                                       '((ditaa . t))
                                       '((plantuml . t))
                                       '((latex . t))
                                       ))

  ;; https://emacs.stackexchange.com/questions/68188/org-mode-do-not-inheriting-unnumbered-property
  ;; Break inheritance of UNNUMBERED in properties drawer ie. :UNNUMBERED: t
  (defun org-export-numbered-headline-p (headline info)
    "Return a non-nil value if HEADLINE element should be numbered.
INFO is a plist used as a communication channel."
    (unless (org-not-nil (org-export-get-node-property :UNNUMBERED headline  ))
                                        ; removing `t` here ↑
                                        ; removes inheritance
      (let ((sec-num (plist-get info :section-numbers))
            (level (org-export-get-relative-level headline info)))
        (if (wholenump sec-num) (<= level sec-num) sec-num))))

  ;; Numbering
  (defun org-export-get-headline-number (headline info)
    "Return numbered HEADLINE numbering as a list of numbers.
INFO is a plist holding contextual information."
    (and (org-export-numbered-headline-p headline info)
         (let* (
                (nums (cdr (assq headline (plist-get info :headline-numbering))))
                (root-heading (let ((parent headline)(temp)) (while (and (setq temp (org-element-property :parent parent)) (eq 'headline (org-element-type temp))) (setq parent temp)) parent))
                (appendix (member "appendix" (org-element-property :tags root-heading))))
           (if (eq 1 (length nums))
               ;; if it's a part get roman numbers
               (list (nth (car nums) '("Z" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX" "X")))
             (let ((nums (cdr nums))) ; remove part number
               (cons (if appendix
                         ;; appendix chapters in alpha
                         (byte-to-string
                          (-
                           (+ ?A (car nums))
                           (nth 1 (cdr (assq
                                        (org-element-map root-heading 'headline (lambda (el) (when (org-export-numbered-headline-p el info) el)) nil t)
                                        (plist-get info :headline-numbering)))) ; number of first appendix
                           ))
                       (+ (car nums) ; sum chapters of previous parts
                          (-count 'identity
                                  (org-element-map (org-element-property :parent root-heading) 'headline
                                    (lambda (el)
                                      (and
                                       (eq 2 (org-element-property :level el))
                                       (< (org-element-property :begin el) (org-element-property :begin root-heading))))))))
                     (cdr nums))
               )))))
  (defun number-to-string (number)
    (format "%s" number))



  ;; Org-capture fix
  ;; ref https://github.com/hlissner/doom-emacs/issues/4832#issuecomment-831538124
  (advice-add #'org-capture :around
              (lambda (fun &rest args)
                (letf! ((#'+org--restart-mode-h #'ignore))
                  (apply fun args))))

  (add-to-list 'org-capture-templates
               '("i" "Inbox" entry (file my-agenda-inbox)
                 "* TODO %?\n\n /Created:/ %U"))
  )

(map! :map org-mode-map
      :localleader
      :desc "SRC block" "B" #'org-insert-structure-template)


;;; Other settings

(defun my-org-capture-inbox ()
  "Capture to Inbox directly"
  (interactive)
  ;; (call-interactively 'org-store-link)
  (org-capture nil "i"))

(defun open-inbox-file ()
  "Open my agenda inbox file"
  (interactive)
  (find-file my-agenda-inbox))

(map! :niv
      :desc "Capture inbox"
      "<f5>" #'my-org-capture-inbox)

;; Show local file TODO list
(map! :leader
      :desc "local TODO"
      "n T" #'org-show-todo-tree)

;;; Agenda
(setq org-agenda-files `(,my-org-agenda-directory
                         ,my-org-roam))

(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t%s")
        (todo   . " ")
        (tags   . " %i %-12:c")
        (search . " %i %-12:c")))

;; Use SPC-m-r to refile inbox
(setq org-agenda-custom-commands
      '(
        ("i" tags "inbox")
        ("W" tags "work")
        ("w" "WORK AGENDA"
         ((agenda "" nil)
          (todo "START"
                ((org-agenda-max-entries 5)
                 (org-agenda-overriding-header "Igangsett oppgaver:")))
          (todo "NEXT"
                ((org-agenda-max-entries 5)
                 (org-agenda-overriding-header "Dagens oppgaver:")))
          (tags "inbox"
                ((org-agenda-overriding-header "Refile SPC-m-r:")
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "NEXT" "START" "CANCELLED")))))
          (tags "work"
                ((org-agenda-overriding-header "Work TODO:")
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "NEXT" "START" "CANCELLED")))))
          ))
        ("d" "Deadlines"
         ((agenda ""
                  ((org-agenda-entry-types '(:deadline))
                   (org-agenda-span 'fortnight)
                   (org-agenda-time-grid nil)
                   (org-deadline-warning-days 0)
                   (org-agenda-skip-deadline-prewarning-if-scheduled nil)
                   (org-agenda-skip-deadline-if-done nil)))))
        ("u" "Unscheduled"
         ((todo  "TODO"
                 ((org-agenda-overriding-header "Unscheduled tasks")
                  (org-agenda-todo-ignore-with-date t)))))
        ))


;; Exclude DONE state tasks from refile targets
(defun ybk/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'ybk/verify-refile-target)

;; ;;; Org roam
;; ;; Need emacsql-sqlite to be installed. In Windows need extra work:
;; ;; Install msys2 via scoop
;; ;; Restart shell
;; ;; in msys2 install make and gcc
;; ;; pacman -S make gcc
;; ;; Go to ~/.emacs.d/.local/straight/build/emacsql-sqlite/sqlite
;; ;; Run this command: make emacsql-sqlite CC=gcc LDLIBS=

;; ;; Define path before loading org-roam
;; (setq org-roam-directory my-org-roam)

;; (after! org-roam
;;   :config
;;   ;; Example for template https://github.com/sunnyhasija/Academic-Doom-Emacs-Config#org-roam-capture-templates
;;   (setq org-roam-capture-templates
;;         (quote (("n" "default" plain
;;                  "%?"
;;                  :if-new
;;                  (file+head "notes/%<%Y-%m-%d-%H%M%S>-${slug}.org"
;;                             "#+title: ${title}\n#+filetags: ${tags}")
;;                  :unnarrowed t)
;;                 ("d" "definition" plain
;;                  "%?"
;;                  :if-new
;;                  (file+head "notes/${slug}.org" "#+title: ${title}\n#+filetags: definition \n\n* Definition\n\n\n* Examples\n")
;;                  :unnarrowed t)
;;                 ("r" "ref" plain "%?"
;;                  :if-new
;;                  (file+head "references/${citekey}.org"
;;                             "#+title: ${slug}: ${title}\n
;;    \n#+filetags: reference ${keywords} \n
;;    \n* ${title}\n\n
;;    \n* Summary
;;    \n\n\n* Rough note space\n")
;;                  :unnarrowed t)
;;                 ("b" "bibliography reference" plain "%?"
;;                  :target
;;                  (file+head "references/${citekey}.org" "#+title: ${title}\n#+filetags: reference ${keywords}")))))

;;   (setq citar-org-roam-note-title-template "${author} - ${title}\n#+filetags: ${tags}")
;;   )

;; (use-package! websocket
;;   :after org-roam)

;; (use-package! org-roam-ui
;;   :after org-roam
;;   :commands (org-roam-ui-mode)
;;   :config
;;   (setq org-roam-ui-open-on-start nil)
;;   (setq org-roam-ui-browser-function #'xwidget-webkit-browse-url)
;;   )


;; (use-package! org-roam-bibtex
;;   :after org-roam
;;   :config
;;   (require 'org-cite)) ; optional
                                        ;
;; Ref https://www.reddit.com/r/emacs/comments/m7ofbh/setting_up_orgroam_protocol_in_qutebrowser/
;; (use-package! org-roam-protocol
;;   :after (org-roam org-roam-dailies org-protocol)
;;   :config
;;   (add-to-list
;;    'org-roam-capture-ref-templates
;;    `(;; Browser bookletmark template:
;;      ;; javascript:location.href =
;;      ;; 'org-protocol://roam-ref?template=w&ref='
;;      ;; + encodeURIComponent(location.href)
;;      ;; + '&title='
;;      ;; + encodeURIComponent(document.getElementsByTagName("h1")[0].innerText)
;;      ;; + '&hostname='
;;      ;; + encodeURIComponent(location.hostname)
;;      ("w" "webref" entry "* ${title} ([[${ref}][${hostname}]])\n%?"
;;       :target
;;       (file+head
;;        ,(concat my-reference-notes "%<%Y-%m>.org")
;;        ,(string-join
;;          '(":properties:"
;;            ":roam_refs: %^{Key}"
;;            ":end:"
;;            "#+title: %<%Y-%m>"
;;            "#+filetags: journal"
;;            "#+startup: overview"
;;            "#+created: %U"
;;            "") "\n"))
;;       :unnarrowed t))))

;;; Deft for searcing notes
;; keybinding for deft not activated automatically
;; https://github.com/hlissner/doom-emacs/issues/2991
(after! deft
  (setq deft-extensions '("org")
        deft-directory my-org-roam
        deft-recursive t
        deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
        deft-use-filename-as-title t)
  (map! :map deft-mode-map
        :n "gr"  #'deft-refresh
        :n "C-s" #'deft-filter
        :i "C-n" #'deft-new-file
        :i "C-m" #'deft-new-file-named
        :i "C-d" #'deft-delete-file
        :i "C-r" #'deft-rename-file
        :n "r"   #'deft-rename-file
        :n "a"   #'deft-new-file
        :n "A"   #'deft-new-file-named
        :n "d"   #'deft-delete-file
        :n "D"   #'deft-archive-file
        :n "q"   #'kill-current-buffer
        :localleader
        "RET" #'deft-new-file-named
        "a"   #'deft-archive-file
        "c"   #'deft-filter-clear
        "d"   #'deft-delete-file
        "f"   #'deft-find-file
        "g"   #'deft-refresh
        "l"   #'deft-filter
        "n"   #'deft-new-file
        "r"   #'deft-rename-file
        "s"   #'deft-toggle-sort-method
        "t"   #'deft-toggle-incremental-search)
  )

;;; Citation
;; Need to install BetterBibTex .xpi extention https://retorque.re/zotero-better-bibtex/
;; Setting in Zotero to access BetterBibTeX and CLS style can be read here:
;; Ref: https://blog.tecosaur.com/tmio/2021-07-31-citations.html#working-with-zotero
;; Install Zotfile to admin PDF ie. the .xpi extention here http://zotfile.com/index.html

;; Change path in "Innstillinger" under in Zotero. Go to "Avansert" -> "Filer og mapper" and in Basismappe:
;; C:\Users\ybka\OneDrive - Folkehelseinstituttet\org\org-roam\bibtex
;; This is to ensure relative link to files especially when using a cloud or network drive

;; Identify where your *.bib file is located ie. variable my-bibtex-file in here,
;; Export zotero library ie. file *.bib to my-bibtex-file by selecting your library, right-click, "Export bibliotek...",
;; Select Format "Better BibLaTeX" then tick "Use Journal Abbreviation" and "Keep updated"
;; save file as library.bib or as defined in your my-bibtex-file location folder eg. ../bibtex/library.bib
;; Ref https://blog.tecosaur.com/tmio/2021-07-31-citations.html#working-with-zotero

;; Citation Style Language (CSL) style is prefered than biblatex (only for LaTeX)
;; Define export style with #+cite_export and the csl file can be found from folder ~/Zotero/styles
;; eg. to use apa.cls style add in orgfile #+CITE_EXPORT: csl apa.csl
(when IS-WINDOWS
  (defvar my-zotero-styles "C:/Users/ybka/Zotero/styles"
    "Default CLS folder for Zotero"))

(when IS-LINUX
  (defvar my-zotero-styles "~/snap/zotero-snap/common/Zotero/styles"
    "Default CLS folder for Zotero"))

;; Main ref: https://blog.tecosaur.com/tmio/2021-07-31-citations.html#basic-usage
;; Ref: https://kristofferbalintona.me/posts/202206141852/
;; Activate :tools biblio
;; Ref: https://github.com/doomemacs/doomemacs/tree/develop/modules/tools/biblio
;; To use other bib file #+bibliography: newlibrary.bib
;; To change reference style #+cite_export: csl vancouver.csl
;; To place the references in orgfile add #+PRINT_BIBLIOGRAPHY:

;; Ref : https://hieuphay.com/doom-emacs-config/#citations
(use-package! citar
  :custom
  (citar-bibliography (list my-bibtex-file))
  (citar-library-paths (list my-reference-pdf))
  (citar-notes-paths (list my-reference-notes))
  (citar-file-variable "file")
  ;; ;; Icons
  ;; (citar-symbols
  ;;  `(
  ;;    (file ,(all-the-icons-faicon "file-pdf-o" :face 'all-the-icons-red :v-adjust -0.1) . " ")
  ;;    (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
  ;;    (link ,(all-the-icons-material "link" :face 'all-the-icons-blue) . " ")))
  (citar-symbol-separator "  ")
  :config
  ;; org-cite
  (setq org-cite-global-bibliography citar-bibliography)
  (setq org-cite-csl-styles-dir my-zotero-styles)

  ;; Able to choose pdf or notes
  (map! :leader
        (:prefix-map ("n" . "notes") ;; notes
         :desc "Open PDF or note"
         "B" #'citar-open))
  )

;;; Notes taking PDF file
(use-package! org-noter
  :after (:any org pdf-view)
  :config
  (setq org-noter-notes-search-path (list my-reference-notes) ;related to main notes files
        org-noter-hide-other nil ;show whole file
        org-noter-separate-notes-from-heading t
        org-noter-default-notes-file-names (list "notes.org")
        org-noter-always-create-frame t) ;keep emacs after kill session

  (map!
   :after org-noter
   :map org-noter-notes-mode-map
   :desc "Insert note"
   "C-M-i" #'org-noter-insert-note
   :desc "Insert precise note"
   "C-M-p" #'org-noter-insert-precise-note
   :desc "Go to previous note"
   "C-M-k" #'org-noter-sync-prev-note
   :desc "Go to next note"
   "C-M-j" #'org-noter-sync-next-note
   :desc "Create skeleton"
   "C-M-s" #'org-noter-create-skeleton
   :desc "Kill session"
   "C-M-q" #'org-noter-kill-session
   )

  (map!
   :after org-noter
   :map org-noter-doc-mode-map
   :desc "Insert note"
   "C-M-i" #'org-noter-insert-note
   :desc "Insert precise note"
   "C-M-p" #'org-noter-insert-precise-note
   :desc "Go to previous note"
   "C-M-k" #'org-noter-sync-prev-note
   :desc "Go to next note"
   "C-M-j" #'org-noter-sync-next-note
   :desc "Create skeleton"
   "C-M-s" #'org-noter-create-skeleton
   :desc "Kill session"
   "C-M-q" #'org-noter-kill-session
   )
  )

(use-package! org-pdftools
  :hook (org-load . org-pdftools-setup-link))
(use-package! org-noter-pdftools
  :after org-noter
  :config
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

;;; Visual
;; Distraction-free screen
(use-package! olivetti
  :init
  (setq olivetti-body-width .67)
  :config
  (defun distraction-free ()
    "Distraction-free writing environment"
    (interactive)
    (if (equal olivetti-mode nil)
        (progn
          (window-configuration-to-register 1)
          (delete-other-windows)
          (text-scale-increase 2)
          (olivetti-mode t))
      (progn
        (jump-to-register 1)
        (olivetti-mode 0)
        (text-scale-decrease 2))))

  (map! :niv
        :desc "Disctraction free"
        "<f9>" #'distraction-free)
  )

;; Ref https://github.com/minad/org-modern
;; This is diabled now
(use-package! org-modern
  :disabled t
  :init
  ;; Add frame borders and window dividers
  (modify-all-frames-parameters
   '((right-divider-width . 10)
     (internal-border-width . 10)))
  (dolist (face '(window-divider
                  window-divider-first-pixel
                  window-divider-last-pixel))
    (face-spec-reset-face face)
    (set-face-foreground face (face-attribute 'default :background)))
  (set-face-background 'fringe (face-attribute 'default :background))

  :config
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-ellipsis "..."

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?-
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     "....." ".................")
   org-agenda-current-time-string
   "<-- now -------------------------------------------------")

  (global-org-modern-mode)
  )

;;; Latex export
;; Use xelatex instead of latex
;; Need to install: apt install xetex
(after! org
  (setq org-latex-pdf-process '("xelatex -shell-escape %f"))
  )


;;; latex preview
;; Auto toggle org-mode latex fragment previews as the cursor enters and exits
;; C-c C-x C-l to toggle
(use-package! org-fragtog
  :after org
  :hook (org-mode . org-fragtog-mode))

;;; Hugo
;;;; ox-hugo
;; Activated by adding +hugo in init.el
(defun org-hugo-new-subtree-post-capture-template ()
  "Returns `org-capture' template string for new Hugo post.
   See `org-capture-templates' for more information."
  (let* (;;https://github.com/sunnyhasija/Academic-Doom-Emacs-Config#ox-hugo
         (date (format-time-string (org-time-stamp-format  :inactive) (org-current-time)))
         (title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
         (fname (org-hugo-slug title)))
    (mapconcat #'identity
               `(
                 ,(concat "* TODO " title)
                 ":PROPERTIES:"
                 ,(concat ":EXPORT_FILE_NAME: " fname)
                 ,(concat ":EXPORT_DATE: " date) ;Enter current date and time
                 ,(concat ":EXPORT_HUGO_CUSTOM_FRONT_MATTER: "  ":tags something :subtitle booyea :featured false :categories abc :highlight true ")
                 ":END:"
                 "%?\n")          ;Place the cursor here
               "\n")))

(setq my-hugo-org-file (expand-file-name "Git-personal/blog-raw/org/blog-harbor.org" hdir-dir-c))

(after! org
  (add-to-list 'org-capture-templates
               '("h" "Hugo Post" entry
                 (file+olp my-hugo-org-file "Posts")
                 (function org-hugo-new-subtree-post-capture-template))))

;;;; easy-hugo
;; From https://github.com/masasam/emacs-easy-hugo
;; Some tweak can be read here https://whatacold.io/blog/2022-10-10-emacs-hugo-blogging/
(use-package! easy-hugo
  :init
  ;; Main blog
  (setq easy-hugo-basedir (expand-file-name "Git-personal/blog-raw/" hdir-dir-c))
  (setq easy-hugo-url "https://yusbk.github.io")
  (setq easy-hugo-sshdomain "github")
  (setq easy-hugo-root (expand-file-name "Git-personal/" hdir-dir-c))
  (setq easy-hugo-previewtime "300")
  )

(map! "C-c C-e" #'easy-hugo)

;;; ox-pandoc
;; Already included in doom with +pandoc but keeping the settings below
;; just in case the default doesn't work
;; (use-package! ox-pandoc
;;   :after org
;;   :config
;;   ;; default options for all output formats
;;   (setq org-pandoc-options '((standalone . t)))
;;   ;; cancel above settings only for 'docx' format
;;   (setq org-pandoc-options-for-docx '((standalone . nil)))
;;   ;; special settings for beamer-pdf and latex-pdf exporters
;;   (setq org-pandoc-options-for-beamer-pdf '((pdf-engine . "xelatex")))
;;   (setq org-pandoc-options-for-latex-pdf '((pdf-engine . "pdflatex")))
;;   ;; special extensions for markdown_github output
;;   (setq org-pandoc-format-extensions '(markdown_github+pipe_tables+raw_html))
;;   )

;; Define keybind instead of +prefix https://rameezkhan.me/posts/2020/2020-07-03--adding-keybindings-to-doom-emacs/
;; Copy paste website when open within Emacs
(use-package! org-download
  :after org
  :config
  (map! :map org-mode-map
        :localleader
        (:prefix ("C" . "Screen capture")
         :desc "Screenshot" "Y" #'org-download-screenshot
         :desc "Yank screenshot" "y" #'org-download-yank)))

;;; presentation
;; Root can use this as well
;; #+REVEAL_ROOT: https://cdn.jsdelivr.net/npm/reveal.js
;; #+REVEAL_REVEAL_JS_VERSION: 4
(use-package! ox-reveal
  :init
  (setq org-reveal-root "file:///n:/Helseprofiler_SPULS/reveal.js"))

(use-package! ox-re-reveal
  :disabled t
  :init
  (setq org-reveal-root "file:///n:/Helseprofiler_SPULS/reveal.js"))

;;; text color
;; this is from here https://stackoverflow.com/a/65750225
;; to use [[color:green][green text is here]]
;; need to install sly package
(org-add-link-type
 "color"
 (lambda (path)
   (message (concat "color "
                    (progn (add-text-properties
                            0 (length path)
                            (list 'face `((t (:foreground ,path))))
                            path) path))))
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "<span style=\"color:%s;\">%s</span>" path desc))
   ((eq format 'latex)
    (format "{\\color{%s}%s}" path desc)))))

;;; Tips
;; Structuring for heading can be found here https://orgmode.org/manual/Structure-Editing.html
;; M-LEFT or M-Right (org-do-demote) to promote or demote current heading (or marked region) by one level
;; M-UP or M-DOWN (org-move-subtree) to move subtree up or down
;; C-c * (org-toggle-heading) to turn normal line or plain list item into a headline
;; C-S-RET (org-insert-todo-heading)
