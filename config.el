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

;;;; Directory
(when IS-LINUX
  (setq hdir-dir-o "/mnt/o")
  (setq hdir-dir-h "/mnt/H")
  (setq hdir-dir-c "~/"))

(when IS-WINDOWS
  (setq hdir-dir-o "O:")
  (setq hdir-dir-h "H:")
  (setq hdir-dir-c "C:/Users/ykama/"))

;;;; OneDrive
(when IS-LINUX
  (setq onedrive "OneDrive/"
        shortcutonedrive (concat hdir-dir-c "OneDrive/")))

(when IS-WINDOWS
  (setq onedrive "C:/Users/ykama/OneDrive - Helsedirektoratet/"
        shortcutonedrive "C:/Users/ykama/OneDrive\\ -\\ Helsedirektoratet/"))

(set-eshell-alias! "cdo" (concat "cd " shortcutonedrive))

;;; Use other functions in Git eg. bash, ediff etc
;; exec-path is used by Emacs to find executables
;; PATH is used by subprocesses (like ediff) spawned by Emacs
;; Add diff.exe directory to exec-path
(add-to-list 'exec-path "C:/Program Files/Git/usr/bin")
;; Add diff.exe directory to PATH environment variable
(setenv "PATH" (concat "C:/Program Files/Git/usr/bin;" (getenv "PATH")))

(setq explicit-shell-file-name "C:/Program Files/Git/bin/bash.exe") ; Path to Bash
(setq shell-file-name explicit-shell-file-name)
;; (add-to-list 'exec-path "C:/Program Files/Git/bin") ; Add Git Bash to PATH

;;; General
;;;; Undo
(setq evil-want-fine-undo t)

;;;; Corfu
;; Delay suggestion or disabled
;; (after! corfu
;;   ;; (setq corfu-auto nil)                     ;; disable auto popup
;;   (setq corfu-auto-delay 0.2)            ;; or use this instead for delay
;;   ;; (map! :i "M-SPC" #'completion-at-point)   ;; manual trigger
;;   )

;; (setq corfu-auto-delay 0.5)
;; (map! :i "M-SPC" #'completion-at-point)   ;; manual trigger

;; ;; Enable corfu in the minibuffer
;; (use-package! corfu
;;   :config
;;   (defun corfu-enable-in-minibuffer ()
;;     "Enable Corfu in the minibuffer if `completion-at-point' is bound."
;;     (when (where-is-internal #'completion-at-point (list (current-local-map)))
;;       ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
;;       (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
;;                   corfu-popupinfo-delay nil)
;;       (corfu-mode 1)))
;;   (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer))

;; ;; orderless matching styles to include char-fold-to-regexp
;; (use-package! orderless
;;   :config
;;   (add-to-list 'orderless-matching-styles 'char-fold-to-regexp))

;; smaller popup
(custom-set-faces! '((corfu-popupinfo) :height 0.9))

;;;; Save
;; Enable format-on-save globally
;; Alternative is to use aggresive-indent
(setq +format-on-save t)

;; Save when in visual mode
(map! :leader
      (:desc "Save" :n "j"  (lambda () (interactive) (evil-normal-state) (save-buffer)))
      )

;;; Shell and alias
;; eshell under :term in init.el need to be activated
;; to use sh then need to install shfmt and shellcheck via scoop

(map! :leader "o x" #'+eshell/frame) ;open shell at doc path
(dolist (alias `(("dsync" "~/.emacs.d/bin/doom sync")
                 ("cdc" ,(concat "cd " hdir-dir-c " && ls -a"))
                 ("cdo" ,(concat "cd " hdir-dir-o " && ls -a"))
                 ("cdh" ,(concat "cd " hdir-dir-h " && ls -a"))
                 ("cdp" ,(concat hdir-dir-o "/Prosjekt/Rusdata && ls -a"))
                 ("cd1" ,(concat shortcutonedrive " && ls -a"))
                 ("cdm" ,(concat "cd " hdir-dir-h "/meetings && ls -a"))
                 ))
  (set-eshell-alias! (car alias) (cadr alias)))

;; (map! :leader "o x" #'+eshell/frame) ;open shell at doc path
;; (set-eshell-alias! "dsync" "~/.config/emacs/bin/doom sync")
;; (set-eshell-alias! "cdc" (concat "cd " hdir-dir-c " && ls -a"))
;; (set-eshell-alias! "cdo" (concat "cd " hdir-dir-o " && ls -a"))
;; (set-eshell-alias! "cdp" (concat hdir-dir-o "/Prosjekt/Rusdata && ls -a"))
;; (set-eshell-alias! "cd1" (concat shortcutonedrive " && ls -a"))
;; (set-eshell-alias! "cdss" "ssh -i ~/.ssh/id_rsa_work ybk@shiny.fhi-api.com")
;; (set-eshell-alias! "cds" "/ssh:shiny:/home/ybk/ShinyApps")

;;;; Font
(when IS-WINDOWS
  ;; Installed from https://github.com/be5invis/Iosevka
  (setq doom-font (font-spec :family "Consolas" :size 17)
        doom-big-font (font-spec :family "Consolas" :size 30)))

;;;; UI and logo
;; (setq fancy-splash-image (expand-file-name "img/doom-emacs-cute.png" doom-user-dir))
(setq fancy-splash-image (expand-file-name "img/doom-emacs.png" doom-user-dir))
;;; Display and themes
;;;; Themes
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

;; Ref https://stackoverflow.com/questions/9900232/changing-color-themes-emacs-24-order-matters/18796138#18796138
(setq my-themes '(
                  doom-palenight
                  ;; doom-dracula
                  doom-gruvbox-light
                  doom-gruvbox
                  doom-one-light
                  doom-flatwhite
                  ))

(setq my-cur-theme nil)
(defun cycle-themes ()
  "Cycle through a list of themes, my-themes."
  (interactive)
  (if (null my-themes)
      (message "No themes available to cycle.")
    (when my-cur-theme
      (disable-theme my-cur-theme)
      (setq my-themes (append my-themes (list my-cur-theme))))
    (setq my-cur-theme (pop my-themes))
    (load-theme my-cur-theme :no-confirm)
    (message "Tema dipakai: %s" my-cur-theme)))

;; Switch to the first theme in the list above
(cycle-themes)

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

;;; Evil related and general tings
;; Activate jk key
(after! evil-escape
  (setq evil-escape-key-sequence "jk"
        evil-escape-delay 0.2))  ;; Adjust delay as needed


;;; ESS
;; ess-switch-process use to choose R process when eval codes with many running processes
;; SPC-m-Shift-TAB or C-c C-s

;; Check R version
(defun check-r-version ()
  "Check the R version used by Emacs."
  (interactive)
  (let ((output (shell-command-to-string "R --version")))
    (message "R version: %s" (car (split-string output "\n")))))

;; ;; Deactivate temporarily due to error message ‘ess--idle-timer-function’
;; (setq ess-use-idle-timer nil)

;; Line number can make you mad
(setq-hook! 'inferior-ess-mode-hook display-line-numbers nil)

;; Define helper functions first
(defun my-add-column ()
  "Adds a data.table update."
  (interactive)
  (insert " := "))

(defun my-add-match ()
  "Adds match."
  (interactive)
  (insert " %in% "))

(defun my-add-pipe ()
  "Adds a pipe operator %>% with one space to the left and then starts a newline with proper indentation."
  (interactive)
  (just-one-space 1)
  (insert "%>%")
  (ess-newline-and-indent))

;; Get commands run from script or console
;; https://stackoverflow.com/questions/27307757/ess-retrieving-command-history-from-commands-entered-in-essr-inferior-mode-or
(defun ess-readline ()
  "Move to previous command entered from script *or* R-process and copy to prompt for execution or editing."
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

;; ;; Set default ESS R session to the default org-mode R session
;; ;; https://www.fredgruber.org/post/ess_emacs_default_r_sessionv2/
;; (defun get-org-current-rsession()
;;   "When you are in an org file get the current R session based on global header, subtree, property or source code :session property whichever is relevant"
;;   (interactive)
;;   (let*
;;       ((mylist (org-babel-get-src-block-info))
;;        (prop (nth 2 mylist))
;;        )
;;     (progn
;;       (setq lang (nth 0 mylist))
;;       (when (string= "R" lang)
;;         (setq session_name (cdr (assq :session prop)))
;;         (message "R session name: %s" session_name)
;;         (set-other-window-func session_name)
;;         )
;;       )
;;     )
;;   )

;; (defun set-ess-R-process()
;;   "set the process to session stored in session_name variable"
;;   (when (string= "R" lang)
;;     (message "Setting R session to %s" session_name)
;;     (setq ess-local-process-name (process-name (get-buffer-process session_name)))
;;     )
;;   )

;; (advice-add 'org-edit-special :before #'get-org-current-rsession)
;; (advice-add 'org-edit-special :after #'set-ess-R-process)

;; Now configure ESS and keybindings
(after! ess
  (add-hook! 'prog-mode-hook #'rainbow-delimiters-mode)
  ;; (add-hook! 'ess-mode-hook #'run-ess-r-newest)

  (map! (:map ess-mode-map
         :localleader
         "T" #'test-R-buffer
         "s" #'ess-indent-region-with-styler
         "g" #'run-ess-r-newest
         "c '" #'polymode-toggle-chunk-narrowing
         )
        (:map ess-r-mode-map
         :i "M--" #'ess-cycle-assign
         :i "M-+" #'my-add-column
         :i "M-'" #'my-add-match
         :i "M-\\" #'my-add-pipe
         ;; Same function with M-n C-t
         :i "C-c '" #'polymode-toggle-chunk-narrowing
         :n "C-c '" #'polymode-toggle-chunk-narrowing
         )
        (:map inferior-ess-r-mode-map
         :i "M--" #'ess-cycle-assign
         :i "M-+" #'my-add-column
         :n "C-<up>" #'ess-readline))
  )

;; Enable outline-based folding for ESS R buffers and integrate with Doom's fold system
;; Fold include ## comments

;; (after! ess-r-mode
;;   ;; Enable outline-minor-mode
;;   (add-hook 'ess-r-mode-hook #'outline-minor-mode)

;;   ;; Make outline settings buffer-local
;;   (add-hook 'ess-r-mode-hook
;;     (lambda ()
;;       (setq-local outline-regexp "##+\\s-*")
;;       (setq-local outline-level
;;                   (lambda ()
;;                     (length (match-string 0))))
;;       ;; Tell Doom to use outline as the folding provider
;;       (setq-local +fold-provider-text 'outline)))
;;   )

;; ;; --- Doom Folding Integration for R and Markdown ---
;; --- Custom Folding for R, Markdown, and Comment Headings ---
(after! evil
  ;; Universal function to enable outline folding
  (defun +custom/enable-outline-folding ()
    "Enable outline-minor-mode with custom settings for comment headings."
    (outline-minor-mode 1)
    ;; Match headings like ##, ###, #### with optional space
    (setq-local outline-regexp "##+\\s-*")
    ;; Compute heading level based on number of #
    (setq-local outline-level
                (lambda ()
                  (length (match-string 0))))
    ;; Tell Doom to use outline as the folding provider
    (setq-local +fold-provider-text 'outline))

  ;; Apply to ESS R mode
  (add-hook 'ess-r-mode-hook #'+custom/enable-outline-folding)

  ;; Apply to Markdown mode
  (add-hook 'markdown-mode-hook
            (lambda ()
              (outline-minor-mode 1)
              (setq-local outline-regexp "#+\\s-*")
              (setq-local outline-level
                          (lambda ()
                            (length (match-string 0))))
              (setq-local +fold-provider-text 'outline)))

  ;; Optional: Add Doom-style keybindings for outline folding
  (map! :map outline-minor-mode-map
        :n "z a" #'outline-toggle-children
        :n "z c" #'outline-hide-subtree
        :n "z o" #'outline-show-subtree
        :n "z m" #'outline-hide-body
        :n "z r" #'outline-show-all))



;;; Quarto
;; Quarto is already included in Doom settings for ESS

;; ;; Check tools that required
;; (defconst QUARTO-P (executable-find "quarto"))

;; ;; Replacement for Rmarkdown
;; ;; Add to PATH in Windows to be able to compile
;; ;; C:\Users\ybka\scoop\apps\quarto\current\bin\quarto.cmd
;; (use-package! quarto-mode
;;   :when QUARTO-P)

;; Enable tree-sitter globally
;; Alternative to font-lock
;; (setq +tree-sitter-enabled-modes '(poly-quarto-mode)) ;instead of font-lock

;; (use-package! quarto-mode
;;   :config
;;   (setq +tree-sitter-enabled-modes '(poly-quarto-mode)) ;instead of font-lock
;;   )

;; alternative to running "quarto preview your-file.qmd"
(after! ess
  (map! (:map markdown-mode-map
         :localleader
         :n "v" #'quarto-preview)))

;;; Stata
(use-package! ado-mode
  :config
  (dolist (pattern '(("\\.do\\'" . ado-mode)
                     ("\\.ado\\'" . ado-mode)))
    (add-to-list 'auto-mode-alist pattern))
  (setq ado-stata-home "C:/Program Files/Stata18"))





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
(use-package! gt
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
       :desc "gt"
       "t" #'gts-do-translate
       :desc "other lang"
       "l" #'google-translate-at-point
       :desc "query translate"
       "L" #'google-translate-query-translate))

;;;; Select region
;; Selection like 'viw' but better
(map! :nvig "C-'" #'er/expand-region)

;; https://micro.rousette.org.uk/2021/01/03/a-useful-binding.html
(map!
 (:map 'override
  :v "v" #'er/expand-region
  :v "V" #'er/contract-region))

;; (package! rotate) needed to be added in init.el
;; Use SPC w SPC to rotate if not using Doom default SPC w r/R
(map! :map evil-window-map
      "SPC" #'rotate-layout
      ;; Swapping windows
      "C-<left>"       #'+evil/window-move-left
      "C-<down>"       #'+evil/window-move-down
      "C-<up>"         #'+evil/window-move-up
      "C-<right>"      #'+evil/window-move-right)

;; Install beacon (package! beacon) in init.el
(map! :leader "c b" #'beacon-blink) ;makes cursor blink when needed

;;; Flyspell
;; Activate in init.el under :checkers (spell +flyspell)
;; Check spelling error. Start by activating (Space t s) then change lang as needed (Space y n/e)
;; Both aspell and hunspell can be installed via scoop on Windows. Else use the instruction below.
(after! flyspell
  ;; This setting specifically for Windows
  ;; Download hunspell and zip to to anyway from here https://sourceforge.net/projects/ezwinports/files/
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
    (setenv "DICPATH" "C:/Emacstillegg/dictionaries")
    ;; (setq ispell-program-name "C:/Users/ybka/Git-personal/dropbox/hunspell-1.3.2-3-w32/bin/hunspell.exe")
    ;; (setq ispell-program-name "C:/Users/ybka/scoop/apps/hunspell/current/bin/hunspell.exe") ;use prog installed via scoop
    (setq ispell-program-name "C:/Emacstillegg/hunspell-1.3.2-3-w32-bin/bin/hunspell.exe")
    (setq lang-norsk "nb_NO")
    (setq lang-eng "en_GB")
    )

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


;;; Outshine header
;; For nativation like Org major-mode. Use <S-Tab> or <C-M i> on the header to fold
(use-package! outshine
  :hook (emacs-lisp-mode . outshine-mode))

;;;; Alias
(set-eshell-alias! "cdl" "cd $1; ls")

;;;; Git alias
(set-eshell-alias!
 "cgw" (concat "cd " (concat hdir-dir-c "/Git-hdir/$1 && ls -a"))
 "cgk" (concat "cd " (concat hdir-dir-c "/Git-kh/$1 && ls -a"))
 "cgp" (concat "cd " (concat hdir-dir-c "/Git-personal/$1 && ls -a"))
 "cgwl" (concat "cd " (concat hdir-dir-c "/Git-hdir && ls -a"))
 "cgkl" (concat "cd " (concat hdir-dir-c "/Git-kh && ls -a"))
 "cgpl" (concat "cd " (concat hdir-dir-c "/Git-personal && ls -a")))

(set-eshell-alias! "gc" "git checkout $1"
                   "gcb" "git checkout -b $1"
                   "gb" "git branch"
                   "gbd" "git branch -d $1" ;delete branch
                   "gbD" "git branch -D $1" ;force delete branch
                   "gf" "git fetch $1"
                   "gm" "git merge $1"
                   "gmf" "git merge --no-ff $1" ;merge fast forward
                   "gpusho" "git push origin"
                   "gpush" "git push origin $1"
                   "gpull" "git pull"
                   "gpushs" "git push origin master --recurse-submodules=on-demand"
                   "gpulls" "git pull --recurse-submodules")


;; ;;; Lisp mode
;; ;; Ensure these extentions use lisp-mode
;; ;; else it's using ado-mode. There is misconfiguration somewhere :-(
;; (dolist (ext '("\\.lisp\\'" "\\.el\\'"))
;;   (add-to-list 'auto-mode-alist (cons ext 'lisp-mode)))

;;; Misc
;; Copy current file path
(defun xah-copy-file-path (&optional @dir-path-only-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.
Result is full path.
If `universal-argument' is called first, copy only the dir path.

If in dired, copy the file/dir cursor is on, or marked files.

If a buffer is not file and not dired, copy value of `default-directory' (which is usually the “current” dir when that buffer was created)

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
           (message "Directory path copied: 「%s」" (file-name-directory $fpath))
           (file-name-directory $fpath))
       (progn
         (message "File path copied: 「%s」" $fpath)
         $fpath )))))

;; (global-set-key (kbd "C-c d") 'xah-copy-file-path)

;;; External settings
;; Load my custom org settings
(load! "+bindings.el")
(load! "+org.el")


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
