;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; (when IS-WINDOWS
;;   (server-start))

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

;;; Personal settings
;;;; System adaptation
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
  (setq fhi-dir-c "C:/Users/ybka/"))

;;;; Local folder
;; ;; Create folder if it doesn't exist
;; (defvar ybk/local-folder (concat fhi-dir-c "emacs-local")
;;   (unless (file-exists-p ybk/local-folder)
;;     (make-directory ybk/local-folder)))

;; (defvar ybk/local-cache (concat ybk/local-folder "cache")
;;   "Else use standard doom .cache")

;;;; OneDrive
(when IS-LINUX
  (setq onedrive "OneDrive/"
        shortcutonedrive (concat fhi-dir-c "OneDrive/")))

(when IS-WINDOWS
  (setq onedrive "C:/Users/ybka/OneDrive - Folkehelseinstituttet/"
        shortcutonedrive "C:/Users/ybka/OneDrive\\ -\\ Folkehelseinstituttet/"))

(set-eshell-alias! "cdo" (concat "cd " shortcutonedrive))

;;;; GoogleDrive
;; This option only applicable in Linux
;; Mount with rclone
;; sudo apt install rclone
;; Create "GoogleDrive" directory ie. mkdir GoogleDrive
;; cd GoogleDrive
;; rclone config (New drive and select "drive" ie. Google Drive)
;; Follow the instruction and use default
;; rclone sync remote:"folerNameInGoogleDrive" if you one to sync for only a drive

(when IS-LINUX
  (set-eshell-alias! "gsync" "rclone sync remote: ~/GoogleDrive"))

;;;; External tools
;; Check tools that required
(defconst QUARTO-P (executable-find "quarto"))
(defconst ZOTERO-P (executable-find "zotero"))

;;; Display and themes
;;;; Themes
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

;; Ref https://stackoverflow.com/questions/9900232/changing-color-themes-emacs-24-order-matters/18796138#18796138
(setq my-themes '(
                  ;; doom-monokai-pro
                  doom-gruvbox
                  doom-gruvbox-light
                  ;; doom-plain-dark
                  ;; doom-plain
                  tango
                  ;; tango-dark
                  deeper-blue
                  ))

(setq my-cur-theme nil)
(defun cycle-themes ()
  "Cycle through a list of themes, my-themes"
  (interactive)
  (when my-cur-theme
    (disable-theme my-cur-theme)
    (setq my-themes (append my-themes (list my-cur-theme))))
  (setq my-cur-theme (pop my-themes))
  (load-theme my-cur-theme :no-confirm)
  (message "Tema dipakai: %s" my-cur-theme))

;; Switch to the first theme in the list above
(cycle-themes)

;; This is now disabled
;; It doesn't work the way I want it
(use-package! cycle-themes
  :custom
  (cycle-themes-theme-list '(
                             doom-monokai-pro
                             doom-gruvbox
                             doom-gruvbox-light
                             doom-plain-dark
                             doom-plain
                             ))
  :config (cycle-themes-mode))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Ref https://hieuphay.com/doom-emacs-config/
;; Show * if file has changed and not saved
(setq frame-title-format
      '(""
        (:eval
         (if (s-contains-p org-roam-directory (or buffer-file-name ""))
             (replace-regexp-in-string
              ".*/[0-9]*-?" "~ "
              (subst-char-in-string ?_ ?  buffer-file-name))
           "%b"))
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format (if (buffer-modified-p)  " * %s" " < %s") project-name))))))

;;;; Font
(when IS-WINDOWS
  ;; Installed from https://github.com/be5invis/Iosevka
  (setq doom-font (font-spec :family "Consolas" :size 17)
        doom-big-font (font-spec :family "Consolas" :size 30)))


;; ;; Set default, fixed and variabel pitch fonts
;; ;; Use M-x menu-set-font to view available fonts
;; (use-package! mixed-pitch
;;   :hook
;;   (text-mode . mixed-pitch-mode)
;;   :config
;;   (set-face-attribute 'default nil :font "Consolas" :height 120)
;;   (set-face-attribute 'fixed-pitch nil :font "Consolas")
;;   (set-face-attribute 'variable-pitch nil :family "Arial")
;;   )

;;;; UI and logo
;; (setq fancy-splash-image (expand-file-name "img/doom-emacs-cute.png" doom-user-dir))
(setq fancy-splash-image (expand-file-name "img/emacs-e.png" doom-user-dir))

;; Nice Academic settings here https://github.com/sunnyhasija/Academic-Doom-Emacs-Config
(unless (equal "Battery status not available"
               (battery))
  (display-battery-mode 1))                       ; On laptops it's nice to know how much power you have

;; (if (eq initial-window-system 'x)                 ; if started by emacs command or desktop file
;;     (toggle-frame-maximized)
;;   (toggle-frame-fullscreen))
(when IS-LINUX
  (toggle-frame-maximized))

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


;;;; Focus
;; Dims region that isn't focused
(use-package! focus)
;;; Split windows
(setq evil-vsplit-window-right t
      evil-split-window-below t)
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  ;; (+ivy/switch-buffer)
  (consult-buffer)
  )
;; (setq +ivy-buffer-preview t)
(setq consult-buffer t)

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

;; This is now disabled since Doom has solaire-mode activate
(use-package! dimmer
  :custom
  (dimmer-fraction 0.40)
  :config
  (dimmer-configure-which-key)
  (dimmer-configure-posframe)
  (dimmer-mode t)
  )

;;; Shell and alias
;; to use sh then need to install shfmt and shellcheck via scoop
(map! :leader "o x" #'+eshell/frame) ;open shell at doc path
(set-eshell-alias! "dsync" "~/.emacs.d/bin/doom sync")
(set-eshell-alias! "cdc" fhi-dir-c)
(set-eshell-alias! "cdh" fhi-dir-h)
(set-eshell-alias! "cdn" fhi-dir-n)
(set-eshell-alias! "cdk" (concat fhi-dir-f "/Forskningsprosjekter/'PDB 2455 - Helseprofiler og til_'"))
(set-eshell-alias! "cdf" (concat fhi-dir-f "/Forskningsprosjekter/'PDB 3327 - Skader i Norge analy_'"))
(set-eshell-alias! "cdss" "ssh -i ~/.ssh/id_rsa_work ybk@shiny.fhi-api.com")
(set-eshell-alias! "cds" "/ssh:shiny:/home/ybk/ShinyApps")

;; Ref https://www.linuxuprising.com/2020/02/how-to-keep-onedrive-in-sync-with.html
;; Use systemctl --user enable onedrive and then start the OneDrive systemd service
;; Usage for onedrive https://github.com/abraunegg/onedrive/blob/master/docs/USAGE.md
;; Reauthorize ie. change password:
;; Stop service if running as client with systemctl --user stop onedrive
;; onedrive --reauth  and follow the instruction
(when IS-LINUX
  (set-eshell-alias! "oe" "systemctl --user enable onedrive")
  (set-eshell-alias! "ostop" "systemctl --user stop onedrive")
  (set-eshell-alias! "odown" "onedrive --synchronize")
  (set-eshell-alias! "oup" "onedrive --synchronize --local-first")
  (set-eshell-alias! "ostart" "systemctl --user start onedrive")
  (set-eshell-alias! "ostatus" "systemctl status --user onedrive")
  (set-eshell-alias! "oss" "onedrive --display-sync-status")
  (set-eshell-alias! "ol" "journalctl --user-unit onedrive -f")
  )

;;;; Git alias
(set-eshell-alias!
 "cgw" (concat "cd " (concat fhi-dir-c "/Git-fhi/$1 && ls -a"))
 "cgk" (concat "cd " (concat fhi-dir-c "/Git-kh/$1 && ls -a"))
 "cgp" (concat "cd " (concat fhi-dir-c "/Git-personal/$1 && ls -a"))
 "cgwl" (concat "cd " (concat fhi-dir-c "/Git-fhi && ls -a"))
 "cgkl" (concat "cd " (concat fhi-dir-c "/Git-kh && ls -a"))
 "cgpl" (concat "cd " (concat fhi-dir-c "/Git-personal && ls -a")))

(set-eshell-alias! "gc" "git checkout $1"
                   "gcb" "git checkout -b $1"
                   "gb" "git branch"
                   "gbd" "git branch -d $1"
                   "gbD" "git branch -D $1"
                   "gf" "git fetch $1"
                   "gm" "git merge $1"
                   "gmf" "git merge --no-ff $1"
                   "gpusho" "git push origin"
                   "gpush" "git push origin $1"
                   "gpull" "git pull"
                   "gpushs" "git push origin master --recurse-submodules=on-demand"
                   "gpulls" "git pull --recurse-submodules")

;; Change keyboard layout when using ssh
(set-eshell-alias! "kb" "setxkbmap no")

;; ref https://github.com/doomemacs/doomemacs/issues/5125
(after! eshell
  :config
  (setq eshell-list-files-after-cd t)
  ;; FIXME: Path-completion, for example with "ls", is disabled until
  ;; `eshell-cmpl-initialize' is called.
  (add-hook! eshell-mode :append #'eshell-cmpl-initialize)
  ;; For some reason, the first `add-hook!' adds
  ;; `eshell-cmpl-initialize' to the beginning, even with `:append'.
  ;; Remove it and add it again to truly append it.
  ;; (When it is at the beginning, it fails to enable completions.)
  (remove-hook! eshell-mode #'eshell-cmpl-initialize)
  (add-hook! eshell-mode :append #'eshell-cmpl-initialize)

  ;; Make sure the hooks were run.
  (add-hook! eshell-mode :append
    (defun my-post-eshell-mode-hook-h ()
      (message "Ran hooks in `eshell-mode-hook'."))))

;;; External settings
;; Load my custom org settings
(load! "+bindings.el")
(load! "+org.el") ; org-mode settigs


;;; Projectile
;; Define project paths to discover with SPC p D
(after! projectile
  :config
  (setq projectile-project-search-path '(("~/Git-fhi" . 1) ("~/Git-kh" . 1)))

  ;; Standard project
  (projectile-add-known-project (expand-file-name "Git-fhi/orgdata" fhi-dir-c))
  )



;;; Outshine header
;; For nativation like Org major-mode. Use <S-Tab> or <C-M i> on the header to fold
(use-package! outshine
  :hook (emacs-lisp-mode . outshine-mode))

;;; Copy paste
;; Paste with encoding
;; Ensure when pasting norwegian special character to keep as it's ie. æøå
(when IS-LINUX
  (setq set-clipboard-coding-system 'utf-8-unix))

;; https://rufflewind.com/2014-07-20/pasting-unicode-in-emacs-on-windows
(when IS-WINDOWS
  (setq set-selection-coding-system 'utf-16-le))

;; ;; Example to use if-else
;; (when IS-WINDOWS
;;   (condition-case nil
;;       (setq inferior-ess-r-program "C:/Users/ybka/scoop/apps/R/current/bin/R.exe")
;;     (error (setq inferior-ess-r-program "C:/Program Files/R/R-4.1.3/bin/R.exe")))
;;   )
;; --- OR ---
;; (if (and (string-match-p "Windows" (getenv "PATH")) (not IS-WINDOWS))
;;     (setq onedrive-directory "/mnt/OneDrive/")
;;   (setq onedrive-directory "~/OneDrive/"))

;; (setq org-directory (concat onedrive-directory "Notes/"))


;;---- Note ----
;; Use simpleclip for copy-paste
;; Copy C-<insert>
;; Cut S-<delete>
;; Paste S-<insert>
;; (simpleclip-mode 1)

;; Copy file path
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
                                                      "/undo-tree-history")))
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
  )

;;; Config
;;;; Aggressive Indent
(use-package! aggressive-indent
  :hook ((emacs-lisp-mode ess-r-mode org-src-mode) . aggressive-indent-mode))

;;; Tramp
;; For windows using plink
;; (when IS-WINDOWS
;;   (setq tramp-default-method "plink")
;;   (when (and (not (string-match putty-directory (getenv "PATH")))
;;      (file-directory-p putty-directory))
;;     (setenv "PATH" (concat putty-directory ";" (getenv "PATH")))
;;     (add-to-list 'exec-path putty-directory)))
(when IS-WINDOWS
  (setq tramp-default-method "plink"))

;; ;; GNU TRAMP Configuration
;; (setq tramp-default-method "ssh"                         ; Default to SSH, that's what I primarily use
;;       tramp-terminal-type "tramp"                        ; Let other terminal know what client I'm connecting with (might need to configure server)
;;       tramp-auto-save-directory "$XDG_CACHE_HOME/tramp/" ; Send Tramp info into XDG Cache directory on machine
;;       tramp-chunksize 2000)                              ; Resonable Chunk size for speed and disk space in mind

;;; Stata
(use-package! ado-mode
  :mode "\\.do$"
  )

;;; ESS
;; ess-switch-process use to choose R process when eval codes with many running processes
;; SPC-m-Shift-TAB or C-c C-s
(after! ess
  (add-hook! 'prog-mode-hook #'rainbow-delimiters-mode)
  (map! (:map ess-mode-map
         :localleader
         "T" #'test-R-buffer
         :nv "v" nil
         "n" 'ess-dev-map ;; renamed from doom default ie. "v"
         "s" #'ess-indent-region-with-styler
         )
        (:map ess-r-mode-map
         :i "M--" #'ess-cycle-assign
         :i "M-+" #'my-add-column
         :i "M-'" #'my-add-match
         :i "M-\\" #'my-add-pipe
         )
        (:map inferior-ess-r-mode-map
         :i "M--" #'ess-cycle-assign
         :i "M-+" #'my-add-column
         :n "C-<up>" #'ess-readline
         ))
  :config
  ;; Error when saving .Rhistory because folder ess-history doesn't exist
  ;; not sure if it's ess problem. Create ~/.emacs.d/.local/cache/ess-history
  ;; folder manually

  ;; ;; When Rterm not found, add R to Windows path. Else use this:
  ;; (when IS-WINDOWS
  ;;   (setq inferior-ess-r-program "C:/Program Files/R/R-4.1.3/bin/R.exe"))

  ;; If most recent R version shall be used
  (when IS-WINDOWS
    (condition-case nil
        (setq inferior-ess-r-program "C:/Users/ybka/scoop/apps/R/current/bin/R.exe")
      (error (setq inferior-ess-r-program "C:/Program Files/R/R-4.1.3/bin/R.exe")))
    )

  (setq ess-style 'RStudio) ;has trouble with styler
  ;; auto-width
  (setq ess-auto-width 'window)
  ;; let lsp manage lintr
  (setq ess-use-flymake nil)

  (setq comint-scroll-to-bottom-on-input t
        comint-scroll-to-bottom-on-output t
        comint-move-point-for-output t)

  ;; Problem with color theme https://github.com/emacs-ess/ESS/issues/1193
  ;; Should be a temporary solution
  (defun my-inferior-ess-init ()
    (setq-local ansi-color-for-comint-mode 'filter)
    (smartparens-mode 1))
  (add-hook 'inferior-ess-mode-hook 'my-inferior-ess-init)

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

  ;; use styler package but it has to be installed first
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

;; ;; https://github.com/r-lib/styler/issues/517
;; (use-package! reformatter
;;   :config
;;   (defconst Rscript-command "Rscript")
;;   (reformatter-define styler
;;     :program Rscript-command
;;     :args (list "--vanilla" "-e" "con <- file(\"stdin\")
;; out <- styler::style_text(readLines(con))
;; close(con)
;; out")
;;     :lighter " styler"))

;; Then to use the above setting, add in you .dir-locals-el project
;; ((ess-r-mode
;;   (mode . styler-on-save)))

;; Trouble with (format +onsave). It can't handle breakpoint
;; when save
(setq +format-on-save-enabled-modes
      '(not ess-r-mode))

;;; Quarto
;; Replacement for Rmarkdown
;; Add to PATH in Windows to be able to compile
;; C:\Users\ybka\scoop\apps\quarto\current\bin\quarto.cmd
(use-package! quarto-mode
  :when QUARTO-P)

(map!
 :map markdown-mode-map
 :localleader
 :n "v" #'quarto-preview)

;;; PDF
;; Install msys2 via scoop
;; In msys2, insall autotools "pacman -S autotools"
;; Follow the steps here https://github.com/doomemacs/doomemacs/blob/develop/modules/tools/pdf/README.org
;; pacman -Syu
;; pacman -S base-devel
;; pacman -S mingw-w64-x86_64-toolchain
;; pacman -S mingw-w64-x86_64-zlib
;; pacman -S mingw-w64-x86_64-libpng
;; pacman -S mingw-w64-x86_64-poppler
;; pacman -S mingw-w64-x86_64-imagemagick
;;
;; Python2 is needed to compile and can be installed from https://repo.msys2.org/mingw/x86_64/
;; pacman -U http://repo.msys2.org/mingw/x86_64/mingw-w64-x86_64-python2-2.7.18-1-any.pkg.tar.xz
;; or using scoop via bucket versions: scoop bucket add versions ; scoop install python27
;; If database is locked, delete file /var/lib/pacman/db.lck via Msys2 before running the commond below
;; Run 'M-x pdf-tools-install' first
(use-package! pdf-tools
  :custom
  (pdf-annot-activate-annotation t "automatically annotate highlights")
  :config
  (when IS-LINUX (pdf-tools-install))
  ;; pdfs are fitted to width by default when openning pdf file
  (setq-default pdf-view-display-size 'fit-width)
  ;; automatically annotate highlights
  (setq pdf-annot-activate-created-annotations t
        pdf-view-resize-factor 1.1)

  (map!
   :map pdf-view-mode-map
   :n "g g"          #'pdf-view-first-page
   :n "G"            #'pdf-view-last-page
   :n "N"            #'pdf-view-next-page-command
   :n "E"            #'pdf-view-previous-page-command
   :n "e"            #'evil-collection-pdf-view-previous-line-or-previous-page
   :n "n"            #'evil-collection-pdf-view-next-line-or-next-page
   :localleader
   (:prefix "o"
            (:prefix "n"
             :desc "Insert" "i" 'org-noter-insert-note
             ))
   )
  )

;;; LaTeX
;; Problem with processing using
;; https://github.com/doomemacs/doomemacs/issues/235#issuecomment-334974537
;; (add-hook! LaTeX-mode
;;   (add-to-list TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
;;   (setq TeX-command-default "XeLaTeX"
;;         TeX-save-query nil
;;         TeX-show-compilation t))

;; (setq-default TeX-engine 'xetex
;;               TeX-PDF-mode t)

;;; Flyspell
;; Check spelling error
;; Both aspell and hunspell can be install via scoop on Windows. Else use the instruction below.
(after! flyspell
  ;; This setting specifically for Windows
  ;; http://juanjose.garciaripoll.com/blog/my-emacs-windows-configuration/
  ;; https://www.reddit.com/r/emacs/comments/8by3az/how_to_set_up_sell_check_for_emacs_in_windows/
  ;; general guide for downloading hunspell http://www.nextpoint.se/?p=656
  ;; Dictionary https://github.com/LibreOffice/dictionaries
  ;; Define dictionary path with DICPATH https://github.com/hunspell/hunspell/blob/master/src/tools/hunspell.cxx#L2040-L2072
  ;; When installing via scoop, needs to install these as well.. via MSYS2
  ;; pacman -S base-devel mingw-w64-x86_64-toolchain mingw-w64-x86_64-libtool
  (when IS-WINDOWS
    ;; Dictionary folder. Download from https://github.com/LibreOffice/dictionaries
    ;; copy all nb* files for Bokmål to DICPATH below
    ;; (setenv "DICPATH" "H:/dropbox/hunspell-1.3.2-3-w32/share/hunspell")
    (setenv "DICPATH" (concat doom-user-dir "hunspell"))
    (setq ispell-program-name "C:/Users/ybka/Git-personal/dropbox/hunspell-1.3.2-3-w32/bin/hunspell.exe")
    ;; (setq ispell-program-name "C:/Users/ybka/scoop/apps/hunspell/current/bin/hunspell.exe") ;use prog installed via scoop
    ;; ;;use the newest version installed via MSYS2
    ;; (ispell-program-name "C:/Users/ybka/scoop/apps/msys2/2020-09-03/mingw64/bin/hunspell.exe")
    (setq lang-norsk "nb_NO")
    (setq lang-eng "nb_GB")
    )

  (setq cache-h-drive (concat fhi-dir-c "Git-personal/dropbox/cache/"))

  ;; (setq ispell-extra-args '("--sug-mode=ultra" ;normal|fast|ultra for speed
  ;;                           "--lang=en_GB"
  ;;                           "-p" ,(expand-file-name "hundspell" cache-h-drive) ;Save dict common location
  ;;                           ))


  ;; Install language from Config language-support if using hunspell ie. standard
  ;; If language support not able to find dictonary, run
  ;; sudo apt install $(check-language-support)
  ;; If using Aspell as here then need to download the language first from
  ;; https://ftp.gnu.org/gnu/aspell/dict/0index.html
  ;; Then install manually with the following command from ~/Downloads
  ;; bzip2 -d aspell6-no-6.0.tar.bz2
  ;; tar -xvf aspell6-no-6.0.tar
  ;; cd aspell6-no-6.0/
  ;; ./configure
  ;; make
  ;; sudo make install
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
;;;; popup
;; This is when enable in :ui popup
;; https://github.com/doomemacs/doomemacs/blob/develop/modules/ui/popup/README.org
;; https://github.com/doomemacs/doomemacs/blob/fb13b902b01783ade0fafc78f0c5f7f786d5fce4/modules/ui/popup/autoload/settings.el
;; Close popup with ESC or C-g
;; Select other popup with C-x p

;; (set-popup-rules!
;;   '(("^ \\*" :slot 1 :vslot -1 :size #'+popup-shrink-to-fit)
;;     ("^\\*"  :slot 1 :vslot -1 :select t))
;;   '(("^\\*Completions" :slot -1 :vslot -2 :ttl 0)
;;     ("^\\*Compil\\(?:ation\\|e-Log\\)" :size 0.3 :ttl 0 :quit t))
;;   '(("^\\*\\(?:scratch\\|Messages\\)" :ttl t)
;;     ("^\\*Help" :slot -1 :size 0.2 :select t)
;;     ("^\\*doom:"
;;      :size 0.35 :select t :modeline t :quit t :ttl t))
;;   ;; Mode specific
;;   '(("^\\*R:*\\*$" :side 'right :ttl nil :select t :size #'+popup-shrink-to-fit))
;;   )

;; show default mode-line in popup
;; (plist-put +popup-defaults :modeline t)

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

;;;; Translate language
;; https://github.com/atykhonov/google-translate
(use-package! google-translate)
(use-package! google-translate-smooth-ui
  :after google-translate
  :config
  (setq google-translate-translation-directions-alist
        '(("en" . "no")
          ("no" . "en")))
  )

;; https://github.com/lorniu/go-translate
;; M-x gts-do-translate
(use-package! go-translate
  :config
  ;; This configuration means:
  ;; Initialize the default translator, let it translate between en and fr via Google Translate,
  ;; and the result will be displayed in the Echo Area.
  (setq gt-langs '(en no))
  (setq gt-default-translator (gt-translator :engines (gt-google-engine)))

  ;; This configuration means:
  ;; Initialize the default translator, let it send all paragraphs in the buffer to Bing and Google,
  ;; and output the results with a new Buffer.
  (setq gt-default-translator
        (gt-translator
         :taker   (gt-taker :text 'buffer :pick 'paragraph)  ; config the Taker
         :engines (list (gt-bing-engine) (gt-google-engine)) ; specify the Engines
         :render  (gt-buffer-render)))                       ; config the Render
  )

(map! :leader
      (:prefix ("=" . "Translate")
       :desc "google-translate"
       "g" #'google-translate-smooth-translate
       :desc "go-translate"
       "t" #'gts-do-translate
       :desc "other lang"
       "l" #'google-translate-at-point
       :desc "query translate"
       "L" #'google-translate-query-translate))

;;; Extended keybindings
(after! magit
  (map! :leader
        :nv "gv" #'vc-refresh-state
        ;; ;; problem with keybind SPC m g v
        ;; :localleader
        ;; "gv" #'vc-refresh-state
        ))

;; Back to normal mode like ESC and jk
(general-define-key
 :keymaps '(insert visual normal)
 "S-SPC" 'evil-force-normal-state)

;; Selection like 'viw' but better
(map! :nvig "C-'" #'er/expand-region)

;; https://micro.rousette.org.uk/2021/01/03/a-useful-binding.html
(map!
 (:map 'override
  :v "v" #'er/expand-region
  :v "V" #'er/contract-region))

;;;; Speed config
;; Configuration to speed up start up especially for Windows based on
;; https://discourse.doomemacs.org/t/why-is-emacs-doom-slow/83/3

;; Speed up deamon
;; https://github.com/doomemacs/doomemacs/issues/3063
(when IS-WINDOWS
  (setq inhibit-compacting-font-caches nil)
  (setq doom-incremental-load-immediately nil))

;;;; Daemon and client
;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs)
  )

;; Guide on daemon and client https://emacs.stackexchange.com/a/69793/10811
(defun emacs-sucide ()
  "Kill all Emacs processes."
  (interactive)
  (let ((cmd "taskkill /f /fi \"IMAGENAME eq emacs.exe\" /fi \"MEMUSAGE gt 15000\""))
    (shell-command cmd)))

;; Guide to use Daemon and Client for Windows
;; Create a EmacsClient shortcut on desktop eg. EmacsClient
;; Add in Target: C:\path\to\emacsclientw.exe -n -c --a ""
;; Alternatively add shortcut key with Ctrl + Alt + E
;; Start in is where Emacs will start with dired or M-x find-file

;; Open startup folder by running the command shell:startup in file explorer
;; Create a shortcut inside startup folder and rename to .bat ie. batch file
;; Add the codes below in the batch file where rem is "remark" for comment
;; rem Sets HOME for current shell
;; rem %APPDATA% is where C:\Users\<username>\AppData\Roaming is
;; set HOME=%HOME%

;; rem Clean previous server file info first
;; del /q ""%HOME%\\.emacs.d\\server\\*""

;; rem Start the Emacs daemon/server with HOME as the default directory
;; C:\Users\ybka\scoop\apps\emacs\current\bin\runemacs.exe --daemon

;; rem Open a client frame
;; start "" "C:\Users\%USERNAME%\Desktop\emacsclientw.exe - Shortcut.lnk"
