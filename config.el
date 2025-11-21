;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Yusman Kamaleri"
      buser-mail-address "ykamamaleri@gmail.com")


;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; (setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
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
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;; =============================
;;; OS-specific Directory Settings
;;; =============================
(when IS-LINUX
  (setq hdir-dir-o "/mnt/o"
        hdir-dir-h "/mnt/H"
        hdir-dir-c "~/"))

(when IS-WINDOWS
  (setq hdir-dir-o "O:"
        hdir-dir-h "H:"
        hdir-dir-c "C:/Users/ykama/"))

;;; =============================
;;; OneDrive Paths
;;; =============================
(when IS-LINUX
  (setq onedrive "OneDrive/"
        shortcutonedrive (concat hdir-dir-c "OneDrive/")))

(when IS-WINDOWS
  (setq onedrive "C:/Users/ykama/OneDrive - Helsedirektoratet/"
        shortcutonedrive "C:/Users/ykama/OneDrive - Helsedirektoratet/"))

(set-eshell-alias! "cdo" (concat "cd " shortcutonedrive))

;;; =============================
;;; Git and Shell Configuration
;;; =============================
(after! exec-path
  ;; Add Git binaries to exec-path and PATH for subprocesses
  (add-to-list 'exec-path "C:/Program Files/Git/usr/bin")
  (setenv "PATH" (concat "C:/Program Files/Git/usr/bin;" (getenv "PATH"))))

(after! eshell
  ;; Configure Bash shell for Windows
  (setq explicit-shell-file-name "C:/Program Files/Git/bin/bash.exe"
        shell-file-name explicit-shell-file-name))

;;; =============================
;;; General Settings
;;; =============================
(setq evil-want-fine-undo t) ; Fine-grained undo in Evil mode

;;; =============================
;;; Corfu Completion (Lazy Loading)
;;; =============================
(use-package! corfu
  :hook (prog-mode . corfu-mode)
  :config
  (setq corfu-auto t
        corfu-auto-delay 0.2))

(custom-set-faces! '((corfu-popupinfo) :height 0.9))

;;; =============================
;;; Format on Save (Selective)
;;; =============================
(setq +format-on-save-enabled-modes '(python-mode r-mode emacs-lisp-mode))

;;; =============================
;;; Eshell Aliases (Fixed for Eshell)
;;; =============================
(map! :leader "o x" #'+eshell/frame)
(dolist (alias `(("dsync" "~/.emacs.d/bin/doom sync")
                 ("cdc" ,(concat "cd " hdir-dir-c "; ls -a"))
                 ("cdo" ,(concat "cd " hdir-dir-o "; ls -a"))
                 ("cdh" ,(concat "cd " hdir-dir-h "; ls -a"))
                 ("cdp" ,(concat hdir-dir-o "/Prosjekt/Rusdata; ls -a"))
                 ("cd1" ,(concat shortcutonedrive "; ls -a"))
                 ("cdm" ,(concat "cd " hdir-dir-h "/meetings; ls -a"))))
  (set-eshell-alias! (car alias) (cadr alias)))

;;; =============================
;;; Fonts
;;; =============================
(when IS-WINDOWS
  (setq doom-font (font-spec :family "Consolas" :size 17)
        doom-big-font (font-spec :family "Consolas" :size 30)))

;;; =============================
;;; UI and Themes
;;; =============================
(setq fancy-splash-image (expand-file-name "img/doom-emacs.png" doom-user-dir))

(setq my-themes '(doom-palenight doom-gruvbox doom-one-light doom-flatwhite))
(setq my-theme-index 0)

(defun cycle-themes ()
  "Cycle through my-themes without modifying the list."
  (interactive)
  (disable-theme (nth my-theme-index my-themes))
  (setq my-theme-index (mod (1+ my-theme-index) (length my-themes)))
  (load-theme (nth my-theme-index my-themes) :no-confirm)
  (message "Tema dipakai: %s" (nth my-theme-index my-themes)))

;; Load the first theme
(load-theme (nth my-theme-index my-themes) :no-confirm)

;;; =============================
;;; Focus Mode
;;; =============================
(use-package! focus
  :commands focus-mode) ;; Lazy load for performance

;;; =============================
;;; Split Windows Behavior
;;; =============================
(setq evil-vsplit-window-right t
      evil-split-window-below t)

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  ;; Use Consult for buffer switching
  (consult-buffer))

;;; =============================
;;; Evil Escape Settings
;;; =============================
(after! evil-escape
  (setq evil-escape-key-sequence "jk"
        evil-escape-delay 0.2)) ;; Adjust delay as needed

;;; =============================
;;; Rainbow Delimiters for Lisp, Stata, ESS, and All Prog Modes
;;; =============================
(use-package! rainbow-delimiters
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (ado-mode . rainbow-delimiters-mode)
         (ess-mode . rainbow-delimiters-mode)      ;; For ESS major mode
         (ess-r-mode . rainbow-delimiters-mode)    ;; For R editing
         (prog-mode . rainbow-delimiters-mode)))   ;; For all programming modes

;;; =============================
;;; ESS Configuration
;;; =============================
;; Check R version quickly
(defun check-r-version ()
  "Display the R version used by Emacs."
  (interactive)
  (message "R version: %s"
           (car (split-string (shell-command-to-string "R --version") "\n"))))

;; Disable line numbers in inferior ESS mode
(setq-hook! 'inferior-ess-mode-hook display-line-numbers nil)

;; Helper functions for R coding
(defun my-add-column () (interactive) (insert " := "))
(defun my-add-match ()  (interactive) (insert " %in% "))
(defun my-add-pipe ()
  "Insert pipe operator and newline with indentation."
  (interactive)
  (just-one-space 1)
  (insert "%>%")
  (ess-newline-and-indent))

;; Retrieve previous commands from R process
(defun ess-readline ()
  "Copy previous command from R process for editing."
  (interactive)
  (if (eq last-command 'ess-readline)
      (setq ess-readline-count (1+ ess-readline-count))
    (setq ess-readline-count 1))
  (comint-goto-process-mark)
  (goto-char (point-max))
  (comint-kill-input)
  (comint-previous-prompt ess-readline-count)
  (comint-copy-old-input)
  (setq this-command 'ess-readline))

(after! ess
  ;; Disable workspace save prompt
  (setq inferior-R-args "--no-save --no-restore-history")

  ;; ;; Enable rainbow delimiters for programming modes
  ;; (add-hook! 'prog-mode-hook #'rainbow-delimiters-mode)

  ;; Keybindings for ESS
  (map! (:map ess-mode-map
         :localleader
         "T" #'test-R-buffer
         "s" #'ess-indent-region-with-styler
         "g" #'run-ess-r-newest
         "c '" #'polymode-toggle-chunk-narrowing)
        (:map ess-r-mode-map
         :i "M--" #'ess-cycle-assign
         :i "M-+" #'my-add-column
         :i "M-'" #'my-add-match
         :i "M-\\" #'my-add-pipe
         :i "C-c '" #'polymode-toggle-chunk-narrowing
         :n "C-c '" #'polymode-toggle-chunk-narrowing)
        (:map inferior-ess-r-mode-map
         :i "M--" #'ess-cycle-assign
         :i "M-+" #'my-add-column
         :n "C-<up>" #'ess-readline)))

;;; =============================
;;; Outline Folding for ESS & Markdown
;;; =============================
(after! evil
  (defun +custom/enable-outline-folding ()
    "Enable outline folding for comment headings."
    (outline-minor-mode 1)
    (setq-local outline-regexp "##+\\s-*")
    (setq-local outline-level (lambda () (length (match-string 0))))
    (setq-local +fold-provider-text 'outline))

  ;; Apply to ESS R mode
  (add-hook 'ess-r-mode-hook #'+custom/enable-outline-folding)

  ;; Apply to Markdown mode
  (add-hook 'markdown-mode-hook
            (lambda ()
              (outline-minor-mode 1)
              (setq-local outline-regexp "#+\\s-*")
              (setq-local outline-level (lambda () (length (match-string 0))))
              (setq-local +fold-provider-text 'outline)))

  ;; Doom-style folding keybindings
  (map! :map outline-minor-mode-map
        :n "z a" #'outline-toggle-children
        :n "z c" #'outline-hide-subtree
        :n "z o" #'outline-show-subtree
        :n "z m" #'outline-hide-body
        :n "z r" #'outline-show-all))

;;; =============================
;;; Quarto Integration
;;; =============================
(after! ess
  (map! (:map markdown-mode-map
         :localleader
         :n "v" #'quarto-preview)))

;;; =============================
;;; Stata Mode
;;; =============================
(use-package! ado-mode
  :mode (("\\.do\\'" . ado-mode)
         ("\\.ado\\'" . ado-mode))
  :config
  (setq ado-stata-home "C:/Program Files/Stata18"))

;;; =============================
;;; Translation Tools
;;; =============================
;; Google Translate (basic)
(use-package! google-translate
  :commands (google-translate-smooth-translate google-translate-at-point google-translate-query-translate))

(use-package! google-translate-smooth-ui
  :after google-translate
  :config
  ;; Set translation directions
  (setq google-translate-translation-directions-alist
        '(("en" . "no")
          ("no" . "en"))))

;; Go-Translate (modern alternative)
(use-package! go-translate
  :commands gts-do-translate
  :config
  ;; Default languages
  (setq gt-langs '(en no))
  ;; Default translator: Google
  (setq gt-default-translator (gt-translator :engines (gt-google-engine)))
  ;; Advanced translator: Bing + Google, render in buffer
  (setq gt-default-translator
        (gt-translator
         :taker   (gt-taker :text 'buffer :pick 'paragraph)
         :engines (list (gt-bing-engine) (gt-google-engine))
         :render  (gt-buffer-render)))

  ;; Optional: Add keybindings for quick access
  (map! :leader
        (:prefix ("=" . "Translate")
         :desc "Google Translate" "g" #'google-translate-smooth-translate
         :desc "Go Translate" "t" #'gts-do-translate
         :desc "Translate at point" "l" #'google-translate-at-point
         :desc "Query Translate" "L" #'google-translate-query-translate)))

;;; =============================
;;; Region Selection (Expand/Contract)
;;; =============================
(use-package! expand-region
  :commands (er/expand-region er/contract-region)
  :config
  ;; Better selection than viw
  (map! :nvig "C-'" #'er/expand-region)
  (map! (:map 'override
         :v "v" #'er/expand-region
         :v "V" #'er/contract-region)))

;;; =============================
;;; Window Management
;;; =============================
;; Rotate windows (requires `rotate` package in init.el)
(map! :map evil-window-map
      "SPC" #'rotate-layout
      ;; Swap windows
      "C-<left>"  #'+evil/window-move-left
      "C-<down>"  #'+evil/window-move-down
      "C-<up>"    #'+evil/window-move-up
      "C-<right>" #'+evil/window-move-right)

;;; =============================
;;; Beacon (Cursor Blink)
;;; =============================
(use-package! beacon
  :commands beacon-blink
  :config
  (map! :leader "c b" #'beacon-blink))

;;; =============================
;;; Flyspell Configuration
;;; =============================
(after! flyspell
  ;; Windows-specific settings
  (when IS-WINDOWS
    (setenv "DICPATH" "C:/Emacstillegg/dictionaries")
    (setq ispell-program-name "C:/Emacstillegg/hunspell-1.3.2-3-w32-bin/bin/hunspell.exe"
          lang-norsk "nb_NO"
          lang-eng "en_GB"))

  ;; Linux-specific settings
  (when IS-LINUX
    (setq ispell-program-name "aspell"
          lang-norsk "norsk"
          lang-eng "english"))

  ;; Functions to switch language
  (defun lang-norsk ()
    "Switch Flyspell to Norwegian."
    (interactive)
    (ispell-change-dictionary lang-norsk)
    (flyspell-buffer))

  (defun lang-eng ()
    "Switch Flyspell to English."
    (interactive)
    (ispell-change-dictionary lang-eng)
    (flyspell-buffer)))

;;; =============================
;;; Outshine for Emacs Lisp Navigation
;;; =============================
(use-package! outshine
  :hook (emacs-lisp-mode . outshine-mode))

;;; =============================
;;; Eshell Aliases
;;; =============================
(set-eshell-alias! "cdl" "cd $1; ls")

;; Git aliases
(dolist (alias '(("cgw" . "/Git-hdir/$1")
                 ("cgk" . "/Git-kh/$1")
                 ("cgp" . "/Git-personal/$1")
                 ("cgwl" . "/Git-hdir")
                 ("cgkl" . "/Git-kh")
                 ("cgpl" . "/Git-personal")))
  (set-eshell-alias! (car alias) (concat "cd " hdir-dir-c (cdr alias) "; ls -a")))

(set-eshell-alias!
 "gc" "git checkout $1"
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

;;; =============================
;;; Misc: Copy Current File Path
;;; =============================
(defun xah-copy-file-path (&optional dir-path-only-p)
  "Copy current buffer's file path or directory to kill-ring.
If DIR-PATH-ONLY-P is non-nil, copy only the directory path."
  (interactive "P")
  (let ((fpath (if (eq major-mode 'dired-mode)
                   (mapconcat 'identity (dired-get-marked-files) "\n")
                 (or (buffer-file-name) default-directory))))
    (kill-new
     (if dir-path-only-p
         (progn
           (message "Directory path copied: %s" (file-name-directory fpath))
           (file-name-directory fpath))
       (progn
         (message "File path copied: %s" fpath)
         fpath)))))
