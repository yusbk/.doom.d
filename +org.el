;;; org.el -*- lexical-binding: t; -*-

;;; Directories
;; Other packages to make org-mode nicer
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;;

(when IS-LINUX
  (setq org-onedrive (concat fhi-dir-c onedrive)))

(when IS-WINDOWS
  (setq org-onedrive onedrive))

(setq org-directory (concat org-onedrive "org/"))

;; Need to create the org files manually with #+FILETAGES:
(setq my-org-agenda-directory (file-truename (concat org-directory "gtd/")))
(defvar my-agenda-inbox (expand-file-name "inbox.org" my-org-agenda-directory)
  "Unstructured capture")
(defvar my-agenda-work (expand-file-name "work.org" my-org-agenda-directory)
  "Work related")
(defvar my-agenda-private (expand-file-name "private.org" my-org-agenda-directory)
  "Private related")
(defvar my-org-roam (expand-file-name "org-roam/" org-directory)
  "Notes and references using org-roam" )

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

  (setq org-agenda-current-time-string "------------------------------------------| now |---")
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
                                       ))
  )

(map! :map org-mode-map
      :localleader
      :desc "SRC block" "B" #'org-insert-structure-template)


;;; Other settings
(setq
 ;; org-babel-mathematica-command "~/.local/bin/mash"
 org-ditaa-jar-path (concat (getenv "HOME") "/.local/bin/ditaa0_9.jar")
 )

;; Org-capture fix
;; ref https://github.com/hlissner/doom-emacs/issues/4832#issuecomment-831538124
(advice-add #'org-capture :around
            (lambda (fun &rest args)
              (letf! ((#'+org--restart-mode-h #'ignore))
                (apply fun args))))

(after! org-capture
  (add-to-list 'org-capture-templates
               '("i" "Inbox" entry (file my-agenda-inbox)
                 "* TODO %?\n\n /Created:/ %U")))

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
        ("r" tags "inbox")
        ("w" "Work Agenda"
         ((agenda "" nil)
          (todo "START"
                ((org-agenda-max-entries 5)
                 (org-agenda-overriding-header "Igangsett oppgaver:")))
          (todo "NEXT"
                ((org-agenda-max-entries 5)
                 (org-agenda-overriding-header "Dagens oppgaver:")))
          (tags "inbox"
                ((org-agenda-overriding-header "Refile SPC-m-r:")
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "NEXT" "START" "CANCELLED")))))))
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

;;; Notes taking
;; PDF files
(defvar my-reference-pdf (expand-file-name "references/" my-org-roam)
  "Reference files mostly as PDF")

;; Take notes on pdf files
(defvar my-reference-notes (expand-file-name "notes/" my-org-roam)
  "Reference notes on PDF files")

(use-package! org-noter
  :after (:any org pdf-view)
  :config
  (setq org-noter-notes-search-path (list my-reference-notes) ;related to main notes files
        org-noter-hide-other nil ;show whole file
        org-noter-separate-notes-from-heading t
        org-noter-default-notes-file-names (list "notes.org")
        org-noter-always-create-frame nil)

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

;;; Deft for searcing notes
;; For searching text for files in defined deft-directory
(setq deft-directory my-org-roam
      deft-extensions '("org" "txt") ;which file extention to search
      ;; deft-auto-save-interval -1.0 ;disable auto-save
      deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
      deft-recursive t ;to be able searching in sub-directories
      ;; converts the filter string into a readable file-name using kebab-case:
      deft-file-naming-rules
      '((noslash . "-")
        (nospace . "-")
        (case-fn . downcase))
      )

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


;;; Referencing
;; Ref: https://github.com/WouterSpekkink/dotfiles/blob/master/doom/config.el
(setq my-bibtex-file (expand-file-name "bibtex/library.bib" my-org-roam))

(use-package! helm-bibtex
  :custom
  (bibtex-completion-notes-path my-reference-notes)
  ;; default library file
  (bibtex-completion-bibliography my-bibtex-file)
  (reftex-default-bibliography my-bibtex-file)
  ;; The line below tells helm-bibtex to find the path to the pdf
  ;; in the "file" field in the .bib file.
  (bibtex-completion-pdf-field "file")
  :hook (Tex . (lambda () (define-key Tex-mode-map "\C-ch" 'helm-bibtex))))

;; Set up org-ref stuff
(use-package! org-ref
  :custom
  (org-ref-default-bibliography my-bibtex-file)
  ;; How to cite http://merkel.texture.rocks/Latex/natbib.php
  (org-ref-default-citation-link "citep") ;Change default from cite:
  (org-ref-insert-link-function 'org-ref-insert-link-hydra/body)
  (org-ref-insert-cite-function 'org-ref-cite-insert-helm)
  (org-ref-insert-label-function 'org-ref-insert-label-link)
  (org-ref-insert-ref-function 'org-ref-insert-ref-link)
  (org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body)))
  (org-ref-notes-directory my-org-roam)

  :config
  (define-key org-mode-map (kbd "C-c r") 'org-ref-insert-link)
  (define-key org-mode-map (kbd "C-c s") 'org-ref-insert-link-hydra/body)

  ;; The function below allows me to consult the pdf of the citation I currently have my cursor on.
  (defun my/org-ref-open-pdf-at-point ()
    "Open the pdf for bibtex key under point if it exists."
    (interactive)
    (let* ((results (org-ref-get-bibtex-key-and-file))
           (key (car results))
           (pdf-file (funcall org-ref-get-pdf-filename-function key)))
      (if (file-exists-p pdf-file)
          (find-file pdf-file)
        (message "No PDF found for %s" key))))

  (setq org-ref-completion-library 'org-ref-ivy-cite
        org-export-latex-format-toc-function 'org-export-latex-no-toc
        org-ref-get-pdf-filename-function
        (lambda (key) (car (bibtex-completion-find-pdf key)))
        org-ref-open-pdf-function 'my/org-ref-open-pdf-at-point
        ;; For pdf export engines
        org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f")
        ;; org-latex-pdf-process '("pdflatex -interaction nonstopmode -output-directory %o %f" "bibtex %b" "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f" "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")
        org-ref-notes-function 'orb-edit-notes)
  )

(after! org-ref
  (setq bibtex-completion-notes-template-multiple-files
        (concat
         "#+TITLE: ${title}\n"
         "#+ROAM_KEY: cite:${=key=}\n"
         "#+ROAM_TAGS: ${keywords}\n"
         "* TODO Notes\n"
         ":PROPERTIES:\n"
         ":Custom_ID: ${=key=}\n"
         ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
         ":AUTHOR: ${author-abbrev}\n"
         ":JOURNAL: ${journaltitle}\n"
         ":DATE: ${date}\n"
         ":YEAR: ${year}\n"
         ":DOI: ${doi}\n"
         ":URL: ${url}\n"
         ":END:\n\n"
         ))
  )

;; For exporting org to LaTeX with specified class to work
;; Ref https://jonathanabennett.github.io/blog/2019/05/29/writing-academic-papers-with-org-mode/
(after! org
  (add-to-list 'org-latex-classes
               '("apa6"
                 "\\documentclass{apa6}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("report"
                 "\\documentclass{report}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

  (add-to-list 'org-latex-classes
               '("koma-article"
                 "\\documentclass{scrartcl}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("memoir"
                 "\\documentclass{memoir}"
                 ("\\book{%s}" . "\\book*{%s}")
                 ("\\part{%s}" . "\\part*{%s}")
                 ("\\chapter{%s} .\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               '("paper"
                 "\\documentclass{paper}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (defun org-export-latex-no-toc (depth)
    (when depth
      (format "%% Org-mode is exporting headings to %s levels.\n"
              depth)))

  )

;;; Journal
;; Have to decide either to use org-roam or org-journal but have to find out
;; how to add TODO to agenda
;;
;;;; org-roam
;; I use multiple directories specified in .dir-locals.el
;; Remember to run org-roam-db-build-cache from a file within specific diretory

;; Need emacsql-sqlite to be installed. In Windows need extra work:
;; Install msys2 via scoop
;; Restart shell
;; in msys2 install make and gcc
;; pacman -S make gcc
;; Go to ~/.emacs.d/.local/straight/build/emacsql-sqlite/sqlite
;; Run this command: make emacsql-sqlite CC=gcc LDLIBS=

(setq org-roam-directory my-org-roam)

(after! org-roam
  (add-hook 'after-init-hook 'org-roam-mode)

  ;; org-roam-bibtex stuff
  (use-package! org-roam-bibtex)
  (org-roam-bibtex-mode)

  (setq orb-preformat-keywords
        '("citekey" "title" "url" "author-or-editor" "keywords" "file")
        orb-process-file-keyword t
        orb-file-field-extensions '("pdf"))

  ;; Let's set up some org-roam capture templates
  (setq org-roam-capture-templates
        (quote (("d" "default" plain
                 "%?"
                 :target
                 (file+head "%<%Y-%m-%d-%H%M%S>-${slug}.org"
                            "#+title: ${title}\n")
                 :unnarrowed t)
                ("r" "bibliography reference" plain "%?"
                 :target
                 ;; The folder as defined in my-reference-notes
                 (file+head "notes/${citekey}.org" "#+title: ${title}\n")))))

  ;; Function to capture quotes from pdf
  (defun org-roam-capture-pdf-active-region ()
    (let* ((pdf-buf-name (plist-get org-capture-plist :original-buffer))
           (pdf-buf (get-buffer pdf-buf-name)))
      (if (buffer-live-p pdf-buf)
          (with-current-buffer pdf-buf
            (car (pdf-view-active-region-text)))
        (user-error "Buffer %S not alive" pdf-buf-name))))

  ;; For org-roam-ui
  (use-package! org-roam-ui)
  (use-package! websocket)
  (use-package! org-roam-ui
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t))

  ;; Workaround for org-roam minibuffer issues
  (defun my/org-roam-node-read--to-candidate (node template)
    "Return a minibuffer completion candidate given NODE.
  TEMPLATE is the processed template used to format the entry."
    (let ((candidate-main (org-roam-node--format-entry
                           template
                           node
                           (1- (frame-width)))))
      (cons (propertize candidate-main 'node node) node)))
  (advice-add 'org-roam-node-read--to-candidate :override #'my/org-roam-node-read--to-candidate)
  )


;;;; org-journal
(after! org-journal
  :init
  (setq org-journal-dir (concat org-directory "Journal/")
        org-journal-time-prefix "* " ;Start at first level heading
        org-journal-date-prefix "#+TITLE: "
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-date-format "%a, %Y-%m-%d")
  :config
  (map! :map org-journal-mode-map
        :localleader
        "S" #'evil-save-modified-and-close )
  )

;;; latex preview
;; Auto toggle org-mode latex fragment previews as the cursor enters and exits
;; C-c C-x C-l to toggle
(use-package! org-fragtog
  :after org
  :hook (org-mode . org-fragtog-mode))

;;; ox-hugo
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

(setq my-hugo-org-file (expand-file-name "Git-personal/blog-raw/org/blog-harbor.org" fhi-dir-c))

(after! org-capture
  (add-to-list 'org-capture-templates
               '("h" "Hugo Post" entry
                 (file+olp my-hugo-org-file "Posts")
                 (function org-hugo-new-subtree-post-capture-template))))

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

;;; Viewing

;;;; Global keys
(map! :leader
      :desc "Org noter"
      "n p" #'org-noter)

(map! :leader
      :desc "Open literature database"
      "o l" #'helm-bibtex)

(map! :map helm-map
      "C-j" #'helm-next-line
      "C-k" #'helm-previous-line)
