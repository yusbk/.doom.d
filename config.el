;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Yusman Kamaleri"
      buser-mail-address "ybkamaleri@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))


;;; Check system.
;; Source https://stackoverflow.com/questions/1817257/how-to-determine-operating-system-in-elisp
(defmacro with-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))

(with-system gnu/linux
  (setq fhi-dir-h "/mnt/H")
  (setq fhi-dir-f "/mnt/F")
  (setq fhi-dir-n "/mnt/N")
  (setq fhi-dir-c "~/"))

(with-system windows-nt
  (setq fhi-dir-h "H:")
  (setq fhi-dir-f "F:")
  (setq fhi-dir-n "N:")
  (setq fhi-dir-c "C:/Users/ybka"))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)
;; (setq doom-theme 'doom-palenight)


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;; Font
;; Installed from https://github.com/be5invis/Iosevka
(setq doom-font (font-spec :family "Iosevka SS04")
      doom-big-font (font-spec :family "Iosevka SS04" :size 30)
                                        ;doom-variable-pitch-font (font-spec :family "ETBembo" :size 24)
                                        ;doom-serif-font (font-spec :family "ETBembo" :size 24)
      )

;; ;; Original code for reference
;; Font size adjustment based on monitor size
;; ;; https://www.reddit.com/r/emacs/comments/dpc2aj/readjusting_fontsize_according_to_monitor/
;; (defun hoagie-adjust-font-size (frame)
;;   "Inspired by https://emacs.stackexchange.com/a/44930/17066. FRAME is ignored.
;; If I let Windows handle DPI everything looks blurry."
;;   ;; Using display names is unreliable...switched to checking the resolution
;;   (let* ((attrs (frame-monitor-attributes)) ;; gets attribs for current frame
;;          (monitor-name (cdr (fourth attrs)))
;;          (width-mm (second (third attrs)))
;;          (width-px (fourth (first attrs)))
;;          (size 13)) ;; default for first screen at work
;;     (when (eq width-px 2560) ;; middle display at work
;;       (setq size 11))
;;     (when (eq width-px 1920) ;; laptop screen
;;       (setq size 12))
;;     (when (eq 531 width-mm)
;;       (setq size 9)) ;; External monitor at home
;;     (when (eq 1095 width-mm)
;;       (setq size 15)) ;; television
;;     (when (eq (length (display-monitor-attributes-list)) 1) ;; override everything if no external monitors!
;;       (setq size 10))
;;     (set-frame-font (format "Iosevka SS04 %s" size))
;;     ))
;; (add-hook 'window-size-change-functions #'hoagie-adjust-font-size)

;;; UI
;; Nice Academic settings here https://github.com/sunnyhasija/Academic-Doom-Emacs-Config
(unless (equal "Battery status not available"
               (battery))
  (display-battery-mode 1))                           ; On laptops it's nice to know how much power you have

(if (eq initial-window-system 'x)                 ; if started by emacs command or desktop file
    (toggle-frame-maximized)
  (toggle-frame-fullscreen))

(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 tab-width 4                                      ; Set width for tabs
 uniquify-buffer-name-style 'forward              ; Uniquify buffer names
 window-combination-resize t                      ; take new window space from all other windows (not just current)
 x-stretch-cursor t                               ; Stretch cursor to the glyph width
 )

(setq
 delete-selection-mode 1                         ; Replace selection when inserting text
 display-time-mode 1                             ; Enable time in the mode-line
 global-subword-mode 1                           ; Iterate through CamelCase words
 line-spacing 0.3                                ; seems like a nice line spacing balance.
 confirm-kill-emacs nil                          ; No need to confirm when exit
 )

(after! doom-modeline
  (setq doom-modeline-major-mode-color-icon t
        doom-modeline-minor-modes (featurep 'minions)))

;;;; Split windows and show Ivy for view
(setq evil-vsplit-window-right t
      evil-split-window-below t)
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer))
(setq +ivy-buffer-preview t)

;; Only show if document isn't in UTF-8
(defun doom-modeline-conditional-buffer-encoding ()
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))
(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

;; Use SPC w SPC to rotate if not using Doom default SPC w r/R
(map! :map evil-window-map
      "SPC" #'rotate-layout
      ;; Swapping windows
      "C-<left>"       #'+evil/window-move-left
      "C-<down>"       #'+evil/window-move-down
      "C-<up>"         #'+evil/window-move-up
      "C-<right>"      #'+evil/window-move-right)

(map! :leader "c b" #'beacon-blink) ;makes cursor blink when needed

(map! :leader "o x" #'+eshell/frame) ;open shell at doc path
(set-eshell-alias! "dsync" "~/.emacs.d/bin/doom sync")
(set-eshell-alias! "cdc" fhi-dir-c)
(set-eshell-alias! "cdh" fhi-dir-h)
(set-eshell-alias! "cdn" fhi-dir-n)
(set-eshell-alias! "cdf" (concat fhi-dir-f "/Forskningsprosjekter/'PDB 2455 - Helseprofiler og til_'"))
(set-eshell-alias! "gpush" "git push origin master --recurse-submodules=on-demand")

(set-eshell-alias! "gitp"
                   (concat "cd " (expand-file-name "/Git-personal && ls -a" fhi-dir-c)))

(set-eshell-alias! "gitf"
                   (concat "cd " (expand-file-name "/Git-fhi && ls -a" fhi-dir-c)))

;; (with-system gnu/linux
;;   (set-eshell-alias! "cdf" "cd '/mnt/F/Forskningsprosjekter/PDB 2455 - Helseprofiler og til_'"))

;; (with-system windows-nt
;;        (set-eshell-alias! "cdf" "cd 'F:/Forskningsprosjekter/PDB 2455 - Helseprofiler og til_'"))

;; (cond (( eq system-type 'gnu/linux )
;;        (set-eshell-alias! "cdf" "cd '/mnt/F/Forskningsprosjekter/PDB 2455 - Helseprofiler og til_'")  )
;;       ((eq system-type 'windows-nt)
;;        (set-eshell-alias! "cdf" "cd 'F:/Forskningsprosjekter/PDB 2455 - Helseprofiler og til_'")
;;        ))

(set-eshell-alias! "cdss" "ssh -i ~/.ssh/id_rsa_work ybk@shiny.fhi-api.com")
(set-eshell-alias! "cds" "/ssh:shiny:/home/ybk/ShinyApps")

(use-package! dimmer
  :custom
  (dimmer-fraction 0.40)
  :config
  (dimmer-configure-which-key)
  (dimmer-configure-posframe)
  (dimmer-mode t)
  )

;;; Outshine header
;; For nativation like Org major-mode. Use <S-Tab> or <C-M i> on the header to fold
(use-package! outshine
  :hook (emacs-lisp-mode . outshine-mode))

;;; Copy file path
(defun xah-copy-file-path (&optional @dir-path-only-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.
  Result is full path.
  If `universal-argument' is called first, copy only the dir path.
  If in dired, copy the file/dir cursor is on, or marked files.
  If a buffer is not file and not dired, copy value of `default-directory' (which is usually
                                                                                  the current dir when that buffer was created)
  URL `http://ergoemacs.org/emacs/emacs_copy_file_path.html'
  Version 2017-09-01"
  (interactive "P")
  (let (($fpath
         (if (string-equal major-mode 'dired-mode)
             (progn
               (let (($result (mapconcat 'identity (dired-get-marked-files) "\n")))
                 (if (equal (length $result) 0)
                     (progn default-directory )
                   (progn $result))))
           (if (buffer-file-name)
               (buffer-file-name)
             (expand-file-name default-directory)))))
    (kill-new
     (if @dir-path-only-p
         (progn
           (message "Directory path copied: ?%s?" (file-name-directory $fpath))
           (file-name-directory $fpath))
       (progn
         (message "File path copied: ?%s?" $fpath)
         $fpath )))))

(map! :leader "f L" #'xah-copy-file-path)

;;; Aggressive Indent
(use-package! aggressive-indent
  :hook ((emacs-lisp-mode ess-r-mode org-src-mode) . aggressive-indent-mode))

(after! ess
  (add-hook! 'prog-mode-hook #'rainbow-delimiters-mode)
  (map! (:map ess-mode-map
         :localleader
         "T" #'test-R-buffer)
        (:map ess-r-mode-map
         :i "M--" #'ess-cycle-assign
         :i "M-+" #'my-add-column
         :i "M-'" #'my-add-match
         :i "M-\\" #'my-add-pipe)
        (:map inferior-ess-r-mode-map
         :i "M--" #'ess-cycle-assign
         :n "C-<up>" #'ess-readline))
  :config
  ;; Error when saving .Rhistory because folder ess-history doesn't exist
  ;; not sure if it's ess problem. Create ~/.emacs.d/.local/cache/ess-history
  ;; folder manually


  (setq comint-scroll-to-bottom-on-input t
        comint-scroll-to-bottom-on-output t
        comint-move-point-for-output t)

  (setq inferior-R-args "--no-save")
  ;; (setq ess-history-directory "~/rhistory")

  (setq ess-R-font-lock-keywords
        '((ess-R-fl-keyword:modifiers . t)
          (ess-R-fl-keyword:fun-defs . t)
          (ess-R-fl-keyword:keywords . t)
          (ess-R-fl-keyword:assign-ops . t)
          (ess-R-fl-keyword:constants . t)
          (ess-fl-keyword:fun-calls . nil)
          (ess-fl-keyword:numbers . t)
          (ess-fl-keyword:operators . t)
          (ess-fl-keyword:delimiters . nil)
          (ess-fl-keyword:= . t)
          (ess-R-fl-keyword:F&T . t)
          (ess-R-fl-keyword:%op% . t)))
  (setq inferior-ess-r-font-lock-keywords
        '((ess-S-fl-keyword:prompt . t)
          (ess-R-fl-keyword:messages . t)
          (ess-R-fl-keyword:modifiers . t)
          (ess-R-fl-keyword:fun-defs . t)
          (ess-R-fl-keyword:keywords . t)
          (ess-R-fl-keyword:assign-ops . t)
          (ess-R-fl-keyword:constants . t)
          (ess-fl-keyword:matrix-labels . t)
          (ess-fl-keyword:fun-calls . nil)
          (ess-fl-keyword:numbers . nil)
          (ess-fl-keyword:operators . nil)
          (ess-fl-keyword:delimiters . nil)
          (ess-fl-keyword:= . nil)
          (ess-R-fl-keyword:F&T . nil)))


  ;; use styler package but it has to be install first
  (defun ess-indent-region-with-styler (beg end)
    "Format region of code R using styler::style_text()."
    (interactive "r")
    (let ((string
           (replace-regexp-in-string
            "\"" "\\\\\\&"
            (replace-regexp-in-string ;; how to avoid this double matching?
             "\\\\\"" "\\\\\\&"
             (buffer-substring-no-properties beg end))))
          (buf (get-buffer-create "*ess-command-output*")))
      (ess-force-buffer-current "Process to load into:")
      (ess-command
       (format
        "local({options(styler.colored_print.vertical = FALSE);styler::style_text(text = \"\n%s\", reindention = styler::specify_reindention(regex_pattern = \"###\", indention = 0), indent_by = 4)})\n"
        string) buf)
      (with-current-buffer buf
        (goto-char (point-max))
        ;; (skip-chars-backward "\n")
        (let ((end (point)))
          (goto-char (point-min))
          (goto-char (1+ (point-at-eol)))
          (setq string (buffer-substring-no-properties (point) end))
          ))
      (delete-region beg end)
      (insert string)
      (delete-char -1)
      ))


  ;; data.table update
  (defun my-add-column ()
    "Adds a data.table update."
    (interactive)
    ;;(just-one-space 1) ;delete whitespace around cursor
    (insert " := "))

  ;; Match
  (defun my-add-match ()
    "Adds match."
    (interactive)
    (insert " %in% "))

  ;; pipe
  (defun my-add-pipe ()
    "Adds a pipe operator %>% with one space to the left and then
  starts a newline with proper indentation"
    (interactive)
    (just-one-space 1)
    (insert "%>%")
    (ess-newline-and-indent))

  ;; Get commands run from script or console
  ;; https://stackoverflow.com/questions/27307757/ess-retrieving-command-history-from-commands-entered-in-essr-inferior-mode-or
  (defun ess-readline ()
    "Move to previous command entered from script *or* R-process and copy
     to prompt for execution or editing"
    (interactive)
    ;; See how many times function was called
    (if (eq last-command 'ess-readline)
        (setq ess-readline-count (1+ ess-readline-count))
      (setq ess-readline-count 1))
    ;; Move to prompt and delete current input
    (comint-goto-process-mark)
    ;; (end-of-buffer nil) ;; tweak here
    (goto-char (point-max))
    (comint-kill-input)
    ;; Copy n'th command in history where n = ess-readline-count
    (comint-previous-prompt ess-readline-count)
    (comint-copy-old-input)
    ;; Below is needed to update counter for sequential calls
    (setq this-command 'ess-readline)
    )

  (defun test-R-buffer ()
    "Create a new empty buffer with R-mode."
    (interactive)
    (let (($buf (generate-new-buffer "*r-test*"))
          (test-mode2 (quote R-mode)))
      (switch-to-buffer $buf)
      (insert (format "## == Test %s == \n\n" "R script"))
      (funcall test-mode2)
      (setq buffer-offer-save t)
      $buf
      ))
  )

;;; Org extention
;; Other packages to make org-mode nicer
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory (expand-file-name "Dropbox/org/" fhi-dir-h))

(after! org
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

  ;;Tetapkan warna keyword
  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("NEXT" :foreground "purple" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
                ("HOLD" :foreground "magenta" :weight bold)
                ("CANCELLED" :foreground "dark green" :weight bold)
                )))

  (setq org-refile-allow-creating-parent-nodes 'confirm)

  (setq org-agenda-start-day "-1d"
        org-agenda-span 4
        org-agenda-start-on-weekday 1)
  )

(setq my-org-agenda-directory (file-truename (expand-file-name "Dropbox/org/gtd/" fhi-dir-h)))
(defvar my-agenda-inbox (expand-file-name "inbox.org" my-org-agenda-directory)
  "Unstructured capture")
(defvar my-agenda-work (expand-file-name "work.org" my-org-agenda-directory)
  "Work related")
(defvar my-agenda-private (expand-file-name "private.org" my-org-agenda-directory)
  "Work related")

(setq org-agenda-files
      `(,my-agenda-inbox
        ,my-agenda-work
        ,my-agenda-private))

(after! org-capture
  (add-to-list 'org-capture-templates
               '("i" "Inbox" entry (file my-agenda-inbox)
                 "* TODO %?\n /Created:/ %U")))

(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")
        (todo   . " ")
        (tags   . " %i %-12:c")
        (search . " %i %-12:c")))


(setq org-agenda-custom-commands
      '(
        ("r" tags "REFILE")
        ("w" "Work Agenda"
         ((agenda "" nil)
          (todo "NEXT"
                ((org-agenda-max-entries 5)
                 (org-agenda-overriding-header "Dagens oppgaver:")))
          (tags "REFILE"
                ((org-agenda-overriding-header "Refile:")
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "NEXT" "CANCELLED")))))))
        ))

;; (use-package! org-super-agenda
;;   :commands (org-super-agenda-mode))
;; (after! org-agenda
;;   (org-super-agenda-mode))

;; (setq org-agenda-skip-scheduled-if-done t
;;       org-agenda-skip-deadline-if-done t
;;       org-agenda-include-deadlines t
;;       org-agenda-block-separator nil
;;       ;; org-agenda-start-day nil ;;i.e today
;;       ;; org-agenda-span 1
;;       org-agenda-tags-column 100
;;       org-agenda-compact-blocks t)
;; (setq org-agenda-custom-commands
;;       '(
;;         ("r" tags "REFILE")
;;         ("o" "Overview"
;;          ((agenda "" ((org-agenda-span 'day)
;;                       (org-super-agenda-groups
;;                        '((:name "Today"
;;                           :time-grid t
;;                           :date today
;;                           :todo "TODAY"
;;                           :scheduled today
;;                           :order 1)))))
;;           (alltodo "" ((org-agenda-overriding-header "")
;;                        (org-super-agenda-groups
;;                         '((:name "Next to do"
;;                            :todo "NEXT"
;;                            :order 1)
;;                           (:name "Due Today"
;;                            :deadline today
;;                            :order 2)
;;                           (:name "Due Soon"
;;                            :deadline future
;;                            :order 6)
;;                           (:name "Refile"
;;                            :tag "REFILE"
;;                            :order 3)))))))))

;; Exclude DONE state tasks from refile targets
(defun ybk/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'ybk/verify-refile-target)

;; Deft for searcing notes
(setq deft-directory (concat org-directory "Notes/")
      deft-extensions '("org" "txt")
      def-recursive t) ;to be able searching in sub-directories

;; org-journal the DOOM way
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
     (list (ivy-completing-read "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (org-edit-src-code)))

(map! :map org-mode-map
      :localleader
      :desc "Insert SRC block" "C" #'my-org-insert-src-block)

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
                 ;; ,(concat ":EXPORT_HUGO_CUSTOM_FRONT_MATTER: "  ":tags something :subtitle booyea :featured false :categories abc :highlight true ")
                 ":END:"
                 "%?\n")          ;Place the cursor here
               "\n")))
(defvar hugo-org-path "/home/ybk/org/"
  "define the place where we put our org files for hugo")
;;(defvar org-capture-blog (concat hugo-org-path "blog.org"))

(after! org-capture
  (add-to-list 'org-capture-templates
               '("h" "Hugo Post" entry
                 (file+olp "c:/Git-personal/blog-raw/org/blog-harbor.org" "Blog Posts")
                 (function org-hugo-new-subtree-post-capture-template))))

;;; CSV
(use-package! csv-mode
  :mode "\\.csv$"
  :custom
  (csv-separators '(","))
  :config
  (map! :map csv-mode-map
        :localleader
        "a" #'csv-align-fields
        "u" #'csv-unalign-fields
        "s" #'csv-sort-fields
        "n" #'csv-sort-numeric-fields))

;;; Personal keybindings
(map! :leader
      (:prefix ("y" . "My keys")
       :desc "file-other-window"
       "f" #'find-file-other-window))
