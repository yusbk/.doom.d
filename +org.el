;;; +org.el -*- lexical-binding: t; -*-

;;; Directories
;; Other packages to make org-mode nicer
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!

(setq org-directory (concat onedrive "org/"))

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

;; Enable variable-pitch-mode automatically for org buffers
(add-hook 'org-mode-hook #'variable-pitch-mode)

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

;; -------------------------------------------------------
;; Visual Fill Column (Center Org Documents)
;; -------------------------------------------------------

;; (use-package! visual-fill-column
;;   ;; :hook (org-present-mode . visual-fill-column-mode)
;;   :config
;;   (setq visual-fill-column-width 110
;;         visual-fill-column-center-text t))

;;; Presentation
;; ----------------------------------------
;; Org-Present + Olivetti Integration
;; ----------------------------------------
;; C-c C-n Next slide
;; C-c C-p Previous slide
;; C-c C-q Quit presentation

(use-package! org-present
  :hook ((org-present-mode . my/org-present-start)
         (org-present-mode-quit . my/org-present-end)
         (org-present-after-navigate-functions . my/org-present-prepare-slide))
  :config
  (defun my/org-present-prepare-slide (_buffer _heading)
    ;; Show only top-level headlines
    (org-overview)
    ;; Unfold current entry
    (org-show-entry)
    ;; Show direct children but keep them collapsed
    (org-show-children))

  (defun my/org-present-start ()
    ;; Enable olivetti for nice centering
    (olivetti-mode 1)
    ;; Optional: set olivetti width for slides
    (setq olivetti-body-width 90)

    ;; Increase font sizes for better readability
    (setq-local face-remapping-alist
                '((default (:height 1.6) variable-pitch)
                  (org-document-title (:height 1.8) org-document-title)
                  (org-level-1 (:height 1.5) org-level-1)
                  (org-level-2 (:height 1.3) org-level-2)
                  (org-code (:height 1.25) org-code)
                  (org-verbatim (:height 1.25) org-verbatim)
                  (org-block (:height 1.15) org-block)))

    ;; Show inline images
    (org-display-inline-images)
    (org-present-hide-cursor)
    ;; Better centering + wrapping
    (visual-fill-column-mode 1)
    (visual-line-mode 1))

  (defun my/org-present-end ()
    ;; Reset fonts
    (setq-local face-remapping-alist nil)
    ;; Disable olivetti when quitting org-present
    (olivetti-mode -1)
    ;; Remove inline images
    (org-remove-inline-images)
    (org-present-show-cursor)
    ;; Stop centering
    (visual-fill-column-mode 0)
    (visual-line-mode 0)))


;;; Tips
;;; -----------------
;; Structuring for heading can be found here https://orgmode.org/manual/Structure-Editing.html
;; M-LEFT or M-Right (org-do-demote) to promote or demote current heading (or marked region) by one level
;; M-UP or M-DOWN (org-move-subtree) to move subtree up or down
;; C-c * (org-toggle-heading) to turn normal line or plain list item into a headline
;; C-S-RET (org-insert-todo-heading)
