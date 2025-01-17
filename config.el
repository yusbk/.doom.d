;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Yusman Kamaleri"
      buser-mail-address "ybkamaleri@gmail.com")


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
(setq org-directory "~/org/")


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
  (setq fhi-dir-h "/mnt/H")
  (setq fhi-dir-f "/mnt/F")
  (setq fhi-dir-n "/mnt/N")
  (setq fhi-dir-c "~/"))

(when IS-WINDOWS
  (setq fhi-dir-h "H:")
  (setq fhi-dir-f "F:")
  (setq fhi-dir-n "N:")
  (setq fhi-dir-c "C:/Users/ybka/"))

;;;; OneDrive
(when IS-LINUX
  (setq onedrive "OneDrive/"
        shortcutonedrive (concat fhi-dir-c "OneDrive/")))

(when IS-WINDOWS
  (setq onedrive "C:/Users/ybka/OneDrive - Folkehelseinstituttet/"
        shortcutonedrive "C:/Users/ybka/OneDrive\\ -\\ Folkehelseinstituttet/"))

(set-eshell-alias! "cdo" (concat "cd " shortcutonedrive))

;;; Shell and alias
;; eshell under :term in init.el need to be activated
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

;;;; Font
(when IS-WINDOWS
  ;; Installed from https://github.com/be5invis/Iosevka
  (setq doom-font (font-spec :family "Consolas" :size 17)
        doom-big-font (font-spec :family "Consolas" :size 30)))

;;;; UI and logo
;; (setq fancy-splash-image (expand-file-name "img/doom-emacs-cute.png" doom-user-dir))
(setq fancy-splash-image (expand-file-name "img/emacs-e.png" doom-user-dir))
;;; Display and themes
;;;; Themes
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

;; Ref https://stackoverflow.com/questions/9900232/changing-color-themes-emacs-24-order-matters/18796138#18796138
(setq my-themes '(
                  doom-gruvbox
                  doom-gruvbox-light
                  doom-one
                  tango
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

;;; ESS
;; ess-switch-process use to choose R process when eval codes with many running processes
;; SPC-m-Shift-TAB or C-c C-s
(after! ess
  (add-hook! 'prog-mode-hook #'rainbow-delimiters-mode)
  (map! (:map ess-mode-map
         :localleader
         "T" #'test-R-buffer
         ;; :nv "v" nil
         ;; "n" 'ess-dev-map ;; renamed from doom default ie. "v"
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
  )


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

;;;; Select region
;; Selection like 'viw' but better
(map! :nvig "C-'" #'er/expand-region)

;; https://micro.rousette.org.uk/2021/01/03/a-useful-binding.html
(map!
 (:map 'override
  :v "v" #'er/expand-region
  :v "V" #'er/contract-region))


;;; External settings
;; Load my custom org settings
(load! "+bindings.el")


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
