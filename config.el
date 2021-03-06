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

;;; Personal settings
;;;; Check system.
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
(setq doom-theme 'doom-badger)
;; (setq doom-theme 'doom-gruvbox)
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

;;;; Local folder
;; Create folder if it doesn't exist
(defvar ybk/local-folder (concat fhi-dir-c "emacs-local/")
  (unless (file-exists-p ybk/local-folder)
    (make-directory ybk/local-folder)))

(defvar ybk/local-cache (concat ybk/local-folder "cache/")
  "Else use standard doom .cache")

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
 ;; line-spacing 0.3                                ; seems like a nice line spacing balance.
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
(set-eshell-alias! "cdss" "ssh -i ~/.ssh/id_rsa_work ybk@shiny.fhi-api.com")
(set-eshell-alias! "cds" "/ssh:shiny:/home/ybk/ShinyApps")
;; Git shortcuts
(set-eshell-alias! "gw"
                   (concat "cd " (concat fhi-dir-c "/Git-fhi && ls -a")))
(set-eshell-alias! "gp"
                   (concat "cd " (concat fhi-dir-c "/Git-personal && ls -a")))
(set-eshell-alias! "gc" "git checkout $1")
(set-eshell-alias! "gm" "git merge $1")
(set-eshell-alias! "gpo" "git push origin")


(after! eshell
  :config
  (setq eshell-list-files-after-cd t))

;; (with-system gnu/linux
;;   (set-eshell-alias! "cdf" "cd '/mnt/F/Forskningsprosjekter/PDB 2455 - Helseprofiler og til_'"))

;; (with-system windows-nt
;;        (set-eshell-alias! "cdf" "cd 'F:/Forskningsprosjekter/PDB 2455 - Helseprofiler og til_'"))

;; (cond (( eq system-type 'gnu/linux )
;;        (set-eshell-alias! "cdf" "cd '/mnt/F/Forskningsprosjekter/PDB 2455 - Helseprofiler og til_'")  )
;;       ((eq system-type 'windows-nt)
;;        (set-eshell-alias! "cdf" "cd 'F:/Forskningsprosjekter/PDB 2455 - Helseprofiler og til_'")
;;        ))

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

;;; Undo
;; Doom uses undo-fu with C-_ and M-_ but I keep undo-tree and activate it when needed
;; as I have used it for a while now
(use-package! undo-tree
  :config
  (defalias 'redo 'undo-tree-redo)

  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)

  (defun ybk/undo-tree-enable-save-history ()
    "Enable auto saving of the undo history."
    (interactive)

    (setq undo-tree-auto-save-history t)

    ;; Compress the history files as .gz files
    ;; (advice-add 'undo-tree-make-history-save-file-name :filter-return
    ;;             (lambda (return-val) (concat return-val ".gz")))

    ;; Persistent undo-tree history across emacs sessions
    (setq my-undo-tree-history-dir (let ((dir (concat ybk/local-cache
                                                      "undo-tree-history/")))
                                     (make-directory dir :parents)
                                     dir))
    (setq undo-tree-history-directory-alist `(("." . ,my-undo-tree-history-dir)))

    (add-hook 'write-file-functions #'undo-tree-save-history-hook)
    (add-hook 'find-file-hook #'undo-tree-load-history-hook))

  (defun my-undo-tree-disable-save-history ()
    "Disable auto saving of the undo history."
    (interactive)

    (setq undo-tree-auto-save-history nil)

    (remove-hook 'write-file-functions #'undo-tree-save-history-hook)
    (remove-hook 'find-file-hook #'undo-tree-load-history-hook))

  ;; ;; Aktifkan
  (global-undo-tree-mode)
  )

;;; Config
;;;; Aggressive Indent
(use-package! aggressive-indent
  :hook ((emacs-lisp-mode ess-r-mode org-src-mode) . aggressive-indent-mode))

;;; ESS
(after! ess
  (add-hook! 'prog-mode-hook #'rainbow-delimiters-mode)
  (map! (:map ess-mode-map
         :localleader
         "T" #'test-R-buffer
         ;; "s" #'ess-indent-region-with-styler
         )
        (:map ess-r-mode-map
         :i "M--" #'ess-cycle-assign
         :i "M-+" #'my-add-column
         :i "M-'" #'my-add-match
         :i "M-\\" #'my-add-pipe
         )
        (:map inferior-ess-r-mode-map
         :i "M--" #'ess-cycle-assign
         :n "C-<up>" #'ess-readline
         ))
  :config
  ;; Error when saving .Rhistory because folder ess-history doesn't exist
  ;; not sure if it's ess problem. Create ~/.emacs.d/.local/cache/ess-history
  ;; folder manually

  ;; When Rterm not found, add R to Windows path. Else use this:
  (when IS-WINDOWS
    (setq inferior-R-program-name "C:/Program Files/R/R-4.1.0/bin/R.exe"))

  (setq ess-style 'RStudio) ;has trouble with styler

  (setq comint-scroll-to-bottom-on-input t
        comint-scroll-to-bottom-on-output t
        comint-move-point-for-output t)

  (setq inferior-R-args "--no-save")
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


  ;; ;; use styler package but it has to be installed first
  ;; (defun ess-indent-region-with-styler (beg end)
  ;;   "Format region of code R using styler::style_text()."
  ;;   (interactive "r")
  ;;   (let ((string
  ;;          (replace-regexp-in-string
  ;;           "\"" "\\\\\\&"
  ;;           (replace-regexp-in-string ;; how to avoid this double matching?
  ;;            "\\\\\"" "\\\\\\&"
  ;;            (buffer-substring-no-properties beg end))))
  ;;         (buf (get-buffer-create "*ess-command-output*")))
  ;;     (ess-force-buffer-current "Process to load into:")
  ;;     (ess-command
  ;;      (format
  ;;       "local({options(styler.colored_print.vertical = FALSE);styler::style_text(text = \"\n%s\", reindention = styler::specify_reindention(regex_pattern = \"###\", indention = 0), indent_by = 4)})\n"
  ;;       string) buf)
  ;;     (with-current-buffer buf
  ;;       (goto-char (point-max))
  ;;       ;; (skip-chars-backward "\n")
  ;;       (let ((end (point)))
  ;;         (goto-char (point-min))
  ;;         (goto-char (1+ (point-at-eol)))
  ;;         (setq string (buffer-substring-no-properties (point) end))
  ;;         ))
  ;;     (delete-region beg end)
  ;;     (insert string)
  ;;     (delete-char -1)
  ;;     ))


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

  ;; Run ShinyApp
  ;; Source  https://jcubic.wordpress.com/2018/07/02/run-shiny-r-application-from-emacs/
  (defun run-shiny ()
    "run shiny R application in new shell buffer
if there is displayed buffer that have shell it will use that window"
    (interactive)
    (let* ((R (concat "shiny::runApp('" default-directory "')"))
           (name "*shiny*")
           (new-buffer (get-buffer-create name))
           (script-proc-buffer
            (apply 'make-comint-in-buffer "script" new-buffer "R" nil `("-e" ,R)))
           (window (get-window-with-mode '(comint-mode eshell-mode)))
           (script-proc (get-buffer-process script-proc-buffer)))
      (if window
          (set-window-buffer window new-buffer)
        (switch-to-buffer-other-window new-buffer))))

  (defun search-window-buffer (fn)
    "return first window for which given function return non nil value"
    (let ((buffers (buffer-list))
          (value))
      (dolist (buffer buffers value)
        (let ((window (get-buffer-window buffer)))
          (if (and window (not value) (funcall fn buffer window))
              (setq value window))))))

  (defun get-window-with-mode (modes)
    "return window with given major modes"
    (search-window-buffer (lambda (buff window)
                            ((let ((mode (with-current-buffer buffer major-mode)))
                               (member mode modes))))))
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
        org-agenda-hide-tags-regexp ".")

  (setq org-babel-load-languages '((R . t)
                                   (dot . t)
                                   (emacs-lisp . t)
                                   (python . t))
        org-use-speed-commands t
        org-structure-template-alist '(("a" . "export ascii")
                                       ("c" . "center")
                                       ("C" . "comment")
                                       ("e" . "example")
                                       ("E" . "export")
                                       ("h" . "export html")
                                       ("l" . "export latex")
                                       ("q" . "quote")
                                       ("s" . "src")
                                       ("v" . "verse")
                                       ("el" . "src emacs-lisp")
                                       ("d" . "definition")
                                       ("t" . "theorem"))
        )
  )

;; https://github.com/Emiller88/doom-emacs-private/blob/6aedee9bb7a5624f1dd63fcd5b6534aad400101a/config.org#reformatter
(use-package! reformatter
  :config
  (defconst Rscript-command "Rscript")
  (reformatter-define styler
                      :program Rscript-command
                      :args (list "--vanilla" "-e" "con <- file(\"stdin\")
out <- styler::style_text(readLines(con))
close(con)
out")
                      :lighter " styler"))


;;; Other org settings
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

(setq my-org-agenda-directory (file-truename (expand-file-name "gtd/" org-directory)))
(defvar my-agenda-inbox (expand-file-name "inbox.org" my-org-agenda-directory)
  "Unstructured capture")
(defvar my-agenda-work (expand-file-name "work.org" my-org-agenda-directory)
  "Work related")
(defvar my-agenda-private (expand-file-name "private.org" my-org-agenda-directory)
  "Private related")
(defvar my-reminder-date (expand-file-name "misc/" org-directory)
  "Dates to remember")

(setq org-agenda-files `(,my-org-agenda-directory
                         ,my-reminder-date))
;; (setq org-agenda-files
;;       `(,my-agenda-inbox
;;         ,my-agenda-work
;;         ,my-agenda-private))

(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t%s")
        (todo   . " ")
        (tags   . " %i %-12:c")
        (search . " %i %-12:c")))


(setq org-agenda-custom-commands
      '(
        ("r" tags "inbox")
        ("w" "Work Agenda"
         ((agenda "" nil)
          (todo "NEXT"
                ((org-agenda-max-entries 5)
                 (org-agenda-overriding-header "Dagens oppgaver:")))
          (tags "inbox"
                ((org-agenda-overriding-header "Refile:")
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "NEXT" "CANCELLED")))))))
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

;;;; Deft for searcing notes
(setq deft-directory (concat org-directory "Notes/")
      deft-extensions '("org" "txt")
      deft-recursive t) ;to be able searching in sub-directories

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

;;;; Roam
;; I use multiple directories specified in .dir-locals.el
;; Remember to run org-roam-db-build-cache from a file within specific diretory
(after! org-roam
  :init
  (setq org-roam-directory (concat org-directory "Notes/"))
  )

(when IS-LINUX
  (use-package! org-roam-server
    :after org-roam
    :config
    (setq org-roam-server-host "127.0.0.1"
          org-roam-server-port 8080
          org-roam-server-authenticate nil
          org-roam-server-export-inline-images t
          org-roam-server-serve-files nil
          org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
          org-roam-server-network-poll t
          org-roam-server-network-arrows nil
          org-roam-server-network-label-truncate t
          org-roam-server-network-label-truncate-length 60
          org-roam-server-network-label-wrap-length 20))
  )

;;;; org-journal the DOOM way
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


;;; Flyspell
;; Check spelling error
(after! flyspell
  ;; This setting specifically for Windows
  ;; http://juanjose.garciaripoll.com/blog/my-emacs-windows-configuration/
  ;; https://www.reddit.com/r/emacs/comments/8by3az/how_to_set_up_sell_check_for_emacs_in_windows/
  ;; general guide for downloading hundspell http://www.nextpoint.se/?p=656
  ;; Dictionary https://github.com/LibreOffice/dictionaries
  (when IS-WINDOWS
    ;; Dictionary folder. Download from https://github.com/LibreOffice/dictionaries
    (setenv "DICTPATH" "H:/Dropbox/hunspell-1.3.2-3-w32/share/hunspell")
    (setq ispell-program-name "H:/Dropbox/hunspell-1.3.2-3-w32/bin/hunspell.exe")
    ;; ;;use the newest version installed via MSYS2
    ;; (ispell-program-name "C:/Users/ybka/scoop/apps/msys2/2020-09-03/mingw64/bin/hunspell.exe")
    (setq lang-norsk "nb_NO")
    (setq lang-eng "nb_GB")
    )

  (setq cache-h-drive (concat fhi-dir-h "Dropbox/cache/"))

  ;; (setq ispell-extra-args '("--sug-mode=ultra" ;normal|fast|ultra for speed
  ;;                           "--lang=en_GB"
  ;;                           "-p" ,(expand-file-name "hundspell" cache-h-drive) ;Save dict common location
  ;;                           ))

  ;; (with-system gnu/linux
  ;;   (setq ispell-program-name "aspell"))

  (when IS-LINUX
    (setq ispell-program-name "aspell")
    (setq lang-norsk "norsk")
    (setq lang-eng "english"))

  (defun lang-norsk ()
    "Change to Norwegian."
    (interactive)
    (ispell-change-dictionary lang-norsk)
    (flyspell-buffer))

  (defun lang-eng ()
    "Change to English."
    (interactive)
    (ispell-change-dictionary lang-eng)
    (flyspell-buffer))
  )

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

;;; Misc
;; Different functions that helps my work

;;;; Vectorise
;; Make words in several lines to be a vector
;; Ref: https://news.ycombinator.com/item?id=22129636
;; Can also use macro as follows:
;; - go to end of the first word
;; - press F3
;; - add comma and a space
;; - delete all the whitespaces the start of next word
;; - go to end of that word
;; - press F4 to register the macro
;; - press F4 to implement the macro repeatedly or C-u 100 F4 to repeat it 100 times
;; Or Vim way in Evil :'<,'>!awk '{printf("\"\%s\", ",$0)}'
;; or the whole file with :%!awk '{printf("\"\%s\", ",$0)}'
(defun vectorise (start end quote)
  "Turn strings on newlines into a QUOTEd, comma-separated one-liner."
  (interactive "r\nMQuote with: ")
  (let ((insertion
         (mapconcat
          (lambda (x) (format "%s%s%s" quote x quote))
          (split-string (buffer-substring start end)) ", ")))
    (delete-region start end)
    (insert insertion)))

;;;; Agenda inbox file
(defun open-inbox-file ()
  "Open my agenda inbox file"
  (interactive)
  (find-file my-agenda-inbox))


;;; Personal keybindings
(map! :leader
      (:prefix ("y" . "My keys")
       :desc "Inbox"
       "i" #'open-inbox-file
       :desc "file-other-window"
       "f" #'find-file-other-window
       :desc "Norsk"
       "n" #'lang-norsk
       :desc "English"
       "e" #'lang-eng
       :desc "fold/toggle"    ;folds keys accessable with z in normal mode too
       "a" #'+fold/toggle     ;with similar keys but less explicit prefix
       :desc "fold/open-all"
       "r" #'+fold/open-all
       :desc "fold/close-all"
       "m" #'+fold/close-all
       :desc "vectorise"
       "v" #'vectorise))
