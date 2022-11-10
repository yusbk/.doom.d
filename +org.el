;;; org.el -*- lexical-binding: t; -*-

;;; Org extention
;; Other packages to make org-mode nicer
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;;

(when IS-LINUX
  (setq org-onedrive (concat fhi-dir-c onedrive)))

(when IS-WINDOWS
  (setq org-onedrive onedrive))

(setq org-directory (concat org-onedrive "/org"))

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
                                       ("r" . "r")
                                       ("s" . "src")
                                       ("v" . "verse")
                                       ("el" . "src emacs-lisp")
                                       ("d" . "definition")
                                       ("t" . "theorem")
                                       ))
  )


;;; Other org settings
;; Add babel
;; ref https://dotdoom.rgoswami.me/config.html
(after! 'org
  (org-babel-do-load-languages 'org-babel-load-languages
                               (append org-babel-load-languages
                                       '((R . t))
                                       '((ditaa . t))
                                       ))
  )

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

(map! :niv
      :desc "Capture inbox"
      "<f5>" #'my-org-capture-inbox)

;; Show local file TODO list
(map! :leader
      :desc "local TODO"
      "n T" #'org-show-todo-tree)

;; Need to create the org files with #+FILETAGES:
(setq my-org-agenda-directory (file-truename (concat org-directory "/gtd/")))
(defvar my-agenda-inbox (expand-file-name "inbox.org" my-org-agenda-directory)
  "Unstructured capture")
(defvar my-agenda-work (expand-file-name "work.org" my-org-agenda-directory)
  "Work related")
(defvar my-agenda-private (expand-file-name "private.org" my-org-agenda-directory)
  "Private related")
(defvar my-reminder-date (expand-file-name "misc/" org-directory)
  "Dates to remember")

(defvar my-org-roam (expand-file-name "org-roam/" org-directory)
  "Notes and references using org-roam" )

(setq org-agenda-files `(,my-org-agenda-directory
                         ,my-reminder-date
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
;; Take notes on pdf files
(defvar my-reference-pdf (expand-file-name "pdf/" my-org-roam)
  "Reference files mostly as PDF")

(after! org-noter
  (setq org-noter-hide-other nil ;show whole file
        org-noter-separate-notes-from-heading t)

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
  )

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
;; Ref: http://www.wouterspekkink.org/academia/writing/tool/doom-emacs/2021/02/27/writing-academic-papers-with-org-mode.html
(setq my-bibtex-file (expand-file-name "bibtex/library.bib" my-org-roam))

(use-package! helm-bibtex
  :custom
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
  (org-ref-default-citation-link "citep") ;Change default from cite:
  (org-ref-insert-link-function 'org-ref-insert-link-hydra/body)
  (org-ref-insert-cite-function 'org-ref-cite-insert-helm)
  (org-ref-insert-label-function 'org-ref-insert-label-link)
  (org-ref-insert-ref-function 'org-ref-insert-ref-link)
  (org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body))))

(define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link)
(define-key org-mode-map (kbd "s-[") 'org-ref-insert-link-hydra/body)

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
      ;; org-latex-pdf-process (list "latexmk -pdflatex='%latex -shell-escape -interaction nonstopmode' -pdf -bibtex -f -output-directory=%o %f")
      org-latex-pdf-process '("pdflatex -interaction nonstopmode -output-directory %o %f" "bibtex %b" "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f" "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")
      org-ref-notes-function 'orb-edit-notes)

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
                 (file+head "references/${citekey}.org" "#+title: ${title}\n")))))

  )

;; (when IS-LINUX
;;   (use-package! org-roam-server
;;     :after org-roam
;;     :config
;;     (setq org-roam-server-host "127.0.0.1"
;;           org-roam-server-port 8080
;;           org-roam-server-authenticate nil
;;           org-roam-server-export-inline-images t
;;           org-roam-server-serve-files nil
;;           org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
;;           org-roam-server-network-poll t
;;           org-roam-server-network-arrows nil
;;           org-roam-server-network-label-truncate t
;;           org-roam-server-network-label-truncate-length 60
;;           org-roam-server-network-label-wrap-length 20))
;;   )

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

;; Code block shortcuts instead of <s[TAB]
(defun my-org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
          '("emacs-lisp" "python" "sh" "R" "latex")))
     (list (consult-completing-read-multiple "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (org-edit-src-code)))


(map! :map org-mode-map
      :localleader
      :desc "Insert SRC block" "C" #'my-org-insert-src-block)


;;; latex preview
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
;; just incase the default doesn't work
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
