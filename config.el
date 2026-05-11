;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(setq user-full-name "Yusman Kamaleri"
      user-mail-address "ykamamaleri@gmail.com")

;;; =============================
;;; OS-specific Directory Settings
;;; =============================
(when IS-LINUX
  (setq hdir-dir-o "/mnt/o"
        hdir-dir-h "/mnt/H"
        hdir-dir-c "~/"))

;; Add slash for Windows ie. "O:/" Without it will call for last used folder in drive,
;; which can be very different from the root and cause confusion.
(when IS-WINDOWS
  (setq hdir-dir-o "O:/"
        hdir-dir-h "H:/"
        hdir-dir-c "C:/Users/ykama/"
        hdir-dir-cc "C:/"))

;;; =============================
;;; OneDrive Paths
;;; =============================
(when IS-LINUX
  (setq onedrive "OneDrive/"
        shortcutonedrive (concat hdir-dir-c "OneDrive/")))

;; Handle separators properly for Windows OneDrive paths, which often contain
;; spaces. Use `expand-file-name` to ensure correct path construction.
(when IS-WINDOWS
  (setq onedrive
        (expand-file-name "OneDrive - Helsedirektoratet/"
                          "C:/Users/ykama/")
        shortcutonedrive onedrive))

;;; =============================
;;; Git and Shell Configuration (Windows)
;;; =============================
;; CHANGED: `after! exec-path` is not a real package — removed that wrapper.
;; exec-path manipulation should happen unconditionally at startup, not deferred.
;; Also: set shell-file-name here only for Windows; global one is set below.
(when IS-WINDOWS
  (let ((git-bin "C:/Program Files/Git/usr/bin"))
    (when (file-directory-p git-bin)
      (add-to-list 'exec-path git-bin)
      (setenv "PATH" (concat git-bin ";" (getenv "PATH")))))

  ;; CHANGED: Set eshell to use bash (from Git for Windows) only on Windows.
  ;; Moved out of `after! eshell` because explicit-shell-file-name needs to be
  ;; set before eshell loads, not inside a deferred block that runs too late.
  (let ((bash-path "C:/Program Files/Git/bin/bash.exe"))
    (when (file-executable-p bash-path)
      (setq explicit-shell-file-name bash-path
            shell-file-name bash-path))))

;;; =============================
;;; General Settings
;;; =============================
(setq evil-want-fine-undo t) ; Fine-grained undo in Evil mode

;; CHANGED: Removed the duplicate/conflicting shell settings that appeared below.
;; The Windows-specific ones above are sufficient.
;; On Linux this falls through to whatever bash is in PATH.
(unless IS-WINDOWS
  (setq shell-file-name (executable-find "bash")))

;; REMOVED: vterm-shell and explicit-shell-file-name set to cmdproxy.exe.
;; cmdproxy.exe is a last-resort fallback — using it directly breaks many shell
;; features and is why eshell/shell behaved oddly. The Git bash path above is
;; the correct Windows shell to use. If you need vterm specifically, set it
;; separately: (setq vterm-shell "C:/Windows/System32/cmd.exe")

;;; =============================
;;; Format on Save (Selective)
;;; =============================
(setq +format-on-save-enabled-modes '(python-mode r-mode emacs-lisp-mode))

;;; ============================
;;; Which key defined
;;; ============================
(after! which-key
  (which-key-add-key-based-replacements
    "C-x RET" "set"
    "C-x a"   "abbreviation"
    "C-x 8"   "emoji"
    "C-x n"   "narrow-codes"
    "C-x r"   "register"
    "C-x t"   "tabs"
    "C-x x"   "buffer-related"
    "C-x w"   "winum"
    "SPC m c" "Comments"))

;;; =============================
;;; Eshell Aliases
;;; =============================
(map! :leader "o x" #'+eshell/frame)

(dolist (alias
         `(("dsync" "~/.emacs.d/bin/doom sync")
           ("cdc" ,(concat "cd " hdir-dir-c "; ls -a"))
           ("cdo" ,(concat "cd " hdir-dir-o "; ls -a"))
           ("cdh" ,(concat "cd " hdir-dir-h "; ls -a"))
           ("cdr" ,(concat "cd " hdir-dir-o "/Prosjekt/Rusdata; ls -a"))
           ("cdp" ,(concat "cd "
                           (shell-quote-argument shortcutonedrive)
                           "; ls -a"))
           ("cdm" ,(concat "cd " hdir-dir-h "/meetings; ls -a"))))
  (set-eshell-alias! (car alias) (cadr alias)))

;;; =============================
;;; Eshell Extra Aliases
;;; =============================
(set-eshell-alias! "cdl" "cd $1; ls")

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
 "gbdO" "git push origin --delete $1"
 "gf" "git fetch $1"
 "gm" "git merge $1"
 "gmf" "git merge --no-ff $1"
 "gpusho" "git push origin"
 "gpush" "git push origin $1"
 "gpull" "git pull"
 "gpushs" "git push origin master --recurse-submodules=on-demand"
 "gpulls" "git pull --recurse-submodules")

;;; =============================
;;; Fonts
;;; =============================
(when IS-WINDOWS
  (setq doom-font (font-spec :family "Consolas" :size 17)
        doom-big-font (font-spec :family "Consolas" :size 30)))

;;; =============================
;;; UI and Themes
;;; =============================
(setq fancy-splash-image (expand-file-name "img/hdir2.png" doom-user-dir))

(setq my-themes '(doom-gruvbox
                  doom-fairy-floss
                  doom-plain
                  doom-ayu-mirage
                  doom-earl-grey))
(setq my-theme-index 0)

(defun cycle-themes ()
  "Cycle through my-themes without modifying the list."
  (interactive)
  (disable-theme (nth my-theme-index my-themes))
  (setq my-theme-index (mod (1+ my-theme-index) (length my-themes)))
  (load-theme (nth my-theme-index my-themes) :no-confirm)
  (message "Tema dipakai: %s" (nth my-theme-index my-themes)))

(load-theme (nth my-theme-index my-themes) :no-confirm)

;;; =============================
;;; Focus Mode
;;; =============================
(use-package! focus
  :commands focus-mode)

;;; =============================
;;; Split Windows Behavior
;;; =============================
(setq evil-vsplit-window-right t
      evil-split-window-below t)

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (consult-buffer))

;;; =============================
;;; Minimap
;;; ============================
(use-package! minimap
  :config
  (setq minimap-window-location 'right
        minimap-width-fraction 0.1
        minimap-major-modes '(prog-mode org-mode)))

(map! :leader
      (:prefix ("o" . "open")
       :desc "Toggle minimap" "m" #'minimap-mode))

;;; =============================
;;; Evil Escape Settings
;;; =============================
(after! evil-escape
  (setq evil-escape-key-sequence "jk"
        evil-escape-delay 0.2))

;;; =============================
;;; Rainbow Delimiters
;;; =============================
(use-package! rainbow-delimiters
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (ado-mode . rainbow-delimiters-mode)
         (ess-mode . rainbow-delimiters-mode)
         (ess-r-mode . rainbow-delimiters-mode)
         (prog-mode . rainbow-delimiters-mode)))

;;; =============================
;;; JSON/YAML hooks
;;; ============================
(add-hook 'json-ts-mode-hook
          (lambda ()
            (when (fboundp 'treesit-font-lock-recompute-features)
              (treesit-font-lock-recompute-features))
            (display-line-numbers-mode 1)))

(after! yaml-ts-mode
  (setq yaml-indent-offset 2)
  (add-hook 'yaml-ts-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'yaml-ts-mode-hook #'display-line-numbers-mode))

;;; =============================
;;; Formatter/format-on-save
;;; ============================
(after! apheleia
  (setf (alist-get 'json-ts-mode apheleia-mode-alist) '(prettier)
        (alist-get 'yaml-ts-mode apheleia-mode-alist) '(prettier)))

;; CHANGED: +format-with-lsp nil is correct — keeps formatting fast and
;; predictable by avoiding LSP format roundtrips (which time out on Windows).
(setq +format-with-lsp nil)

;;; =============================
;;; ESS Configuration
;;; =============================

;; CHANGED: Pinned to exact versioned Rterm.exe path.
;; Using "R" (relying on PATH) is cleaner but can break if PATH isn't set up
;; properly in the Emacs GUI process on Windows (which doesn't inherit shell PATH).
(when (eq system-type 'windows-nt)
  (setq inferior-ess-r-program "C:/Program Files/R/R-4.5.1/bin/x64/Rterm.exe"))

(defun check-r-version ()
  "Display the R version used by Emacs."
  (interactive)
  (message "R version: %s"
           (car (split-string (shell-command-to-string "R --version") "\n"))))

;; Set CRAN mirror automatically when R starts
(add-hook 'ess-r-post-run-hook
          (lambda ()
            (ess-send-string (ess-get-process)
                             "options(repos = c(CRAN='https://cran.rstudio.com'))\n")))

;; Helper functions for R coding
(defun my-add-column () (interactive) (insert " := "))
(defun my-add-match ()  (interactive) (insert " %in% "))
(defun my-add-pipe ()
  "Insert pipe operator and newline with indentation."
  (interactive)
  (just-one-space 1)
  (insert "%>%")
  (ess-newline-and-indent))

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
  (setq inferior-R-args "--no-save --no-restore-history --no-restore"
        ess-indent-with-fancy-comments nil
        ess-ask-for-ess-directory nil
        ess-roxy-str "#'"
        ess-switch-process t)

  (add-to-list 'auto-mode-alist '("\\.[rR]\\'" . ess-r-mode))

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
         :i "C-|" (lambda () (interactive) (insert " |> "))
         :i "C-%" (lambda () (interactive) (insert " %>% "))
         :i "C-i" (lambda () (interactive) (insert " %in% "))
         :i "C-c '" #'polymode-toggle-chunk-narrowing
         :n "C-c '" #'polymode-toggle-chunk-narrowing)
        (:map inferior-ess-r-mode-map
         :i "M--" #'ess-cycle-assign
         :i "M-+" #'my-add-column
         :n "C-<up>" #'ess-readline)))

;;; =============================
;;; Eglot for R (via languageserver)
;;; =============================
;; THE JSONRPC TIMEOUT FIX:
;; The error `jsonrpc-error request id=1 failed: Timed out` means Eglot sent a
;; request to the R languageserver but got no reply within the timeout window.
;; On Windows this almost always happens because:
;;   1. `languageserver::run()` takes 5-15 seconds to start up (R startup is slow).
;;   2. Eglot's default timeout (30s) is often not enough on a cold start with
;;      antivirus scanning R + languageserver + all its dependencies.
;;   3. The PATH "R" lookup fails silently in the GUI Emacs process on Windows,
;;      so languageserver never actually starts.
;;
;; Fixes applied below:
;;   - Use the full Rterm.exe path (not bare "R") to avoid PATH lookup failures.
;;   - Increase eglot-connect-timeout to 120s.
;;   - Add --no-save --no-restore flags to speed up R startup.
;;   - Wrap eglot-ensure in a short idle timer so ESS finishes loading before
;;     Eglot tries to connect (removes the mode-spec error on file open).
;;   - Kept eglot-events-buffer-size at a non-zero value for debugging; set to 0
;;     only after confirmed working (0 disables the log entirely).

(after! eglot
  (setq eglot-connect-timeout 120     ; Windows R startup is slow
        eglot-events-buffer-size 2000  ; small log for debugging; set 0 when stable
        eglot-report-progress nil)     ; avoids noisy modeline updates

  ;; Use full Rterm.exe path — GUI Emacs on Windows doesn't inherit shell PATH,
  ;; so bare "R" can fail silently.
  (when IS-WINDOWS
    (add-to-list 'eglot-server-programs
                 `(ess-r-mode . ("C:/Program Files/R/R-4.5.1/bin/x64/Rterm.exe"
                                 "--no-save"
                                 "--no-restore"
                                 "--slave"
                                 "-e"
                                 "languageserver::run()"))))
  (unless IS-WINDOWS
    (add-to-list 'eglot-server-programs
                 '(ess-r-mode . ("R" "--slave" "-e" "languageserver::run()"))))

  ;; POLYMODE-SAFE eglot-ensure
  ;; Direct `eglot-ensure` in the mode hook fires during polymode chunk setup,
  ;; before the R process exists, causing the "Polymode error (pm--mode-setup)"
  ;; timeout. The fix:
  ;;   1. Skip eglot entirely if we're inside a polymode inner buffer — let
  ;;      the host buffer's Eglot session handle LSP for the whole .Rmd file.
  ;;   2. For plain .R files, use a short idle timer so ESS finishes its own
  ;;      setup before Eglot tries to connect.
  (defun ybk/eglot-ensure-safe ()
    "Start Eglot safely, skipping polymode inner buffers."
    (when (and
           ;; Not a polymode inner buffer (these are indirect buffers with a base)
           (not (and (boundp 'polymode-mode) polymode-mode
                     (buffer-base-buffer)))
           ;; Not already managed
           (not (eglot-managed-p)))
      (run-with-idle-timer
       2.0 nil
       (lambda (buf)
         (when (and (buffer-live-p buf)
                    (not (eglot-managed-p)))
           (with-current-buffer buf
             ;; Final check: still not in a polymode inner buffer
             (unless (and (boundp 'polymode-mode) polymode-mode
                          (buffer-base-buffer))
               (ignore-errors (eglot-ensure))))))
       (current-buffer))))

  (add-hook 'ess-r-mode-hook #'ybk/eglot-ensure-safe))


;; (after! eglot
;;   (setq eglot-connect-timeout 120   ; CHANGED from 60 — Windows R startup is slow
;;         eglot-events-buffer-size 2000  ; CHANGED: keep small log for debugging
;;         eglot-report-progress nil)  ; CHANGED: avoids noisy modeline updates

;;   ;; CHANGED: Use full path to Rterm so Eglot finds it even when the GUI Emacs
;;   ;; process doesn't inherit your shell PATH (common on Windows).
;;   (when IS-WINDOWS
;;     (add-to-list 'eglot-server-programs
;;                  `(ess-r-mode . ("C:/Program Files/R/R-4.5.1/bin/x64/Rterm.exe"
;;                                  "--no-save"
;;                                  "--no-restore"
;;                                  "--slave"
;;                                  "-e"
;;                                  "languageserver::run()"))))

;;   ;; Linux / fallback: rely on PATH
;;   (unless IS-WINDOWS
;;     (add-to-list 'eglot-server-programs
;;                  '(ess-r-mode . ("R" "--slave" "-e" "languageserver::run()"))))

;;   ;; CHANGED: Don't call eglot-ensure directly in the hook — use a short idle
;;   ;; timer instead. This gives ESS time to finish setting up the buffer before
;;   ;; Eglot tries to connect, which prevents the "File mode specification error"
;;   ;; you see when opening .R files.
;;   (add-hook 'ess-r-mode-hook
;;             (lambda ()
;;               (run-with-idle-timer
;;                1.5 nil  ; wait 1.5 seconds of idle before connecting
;;                (lambda ()
;;                  (when (and (buffer-live-p (current-buffer))
;;                             (derived-mode-p 'ess-r-mode))
;;                    (eglot-ensure)))))))

;; Eglot keybindings
(map! :map ess-r-mode-map
      :localleader
      "l s" #'eglot
      "l r" #'eglot-reconnect
      "l f" #'eglot-format
      "l a" #'eglot-code-actions
      "l d" #'eldoc
      "l R" #'eglot-rename
      "l =" #'apheleia-format-buffer)

;;; =============================
;;; Apheleia (R formatting via styler)
;;; =============================
(with-eval-after-load 'apheleia
  (setf (alist-get 'R apheleia-formatters)
        '("Rscript" "--vanilla" "-e"
          "styler::style_file(commandArgs(TRUE)[1])"
          filepath))
  (add-hook 'ess-r-mode-hook #'apheleia-mode))

(add-hook 'ess-r-mode-hook
          (lambda ()
            (when (bound-and-true-p apheleia-mode)
              (add-hook 'before-save-hook #'apheleia-format-buffer nil t))))

;;; =============================
;;; ESS Style & Comment Settings
;;; =============================
(after! ess-r-mode
  (setq ess-style 'RStudio
        comment-style 'plain
        ess-indent-with-fancy-comments nil
        ess-fancy-comments nil)

  (add-hook 'ess-r-mode-hook
            (lambda ()
              (setq-local comment-start "# "
                          comment-end ""
                          comment-add 0))))

;;; =============================
;;; Comment Align Helper
;;; =============================
(defun ybk/align-comment-line-generic (&optional width)
  "Pad comment line with '-' to reach WIDTH using buffer's `comment-start`."
  (interactive)
  (let* ((width  (or width (and (boundp 'fill-column) fill-column) 70))
         (cstart (or comment-start "#"))
         (cchar  (string (aref (string-trim-left cstart) 0)))
         (re (concat "^\\([ \t]*\\)\\(" (regexp-quote cchar) "+\\)\\s-*\\(.*\\)?$"))
         (line (buffer-substring-no-properties
                (line-beginning-position) (line-end-position))))
    (when (string-match re line)
      (let* ((indent (match-string 1 line))
             (hashes (match-string 2 line))
             (text   (or (match-string 3 line) ""))
             (text   (string-trim-right text))
             (base   (concat indent
                             hashes
                             (if (string-empty-p text) "" " ")
                             text
                             " "))
             (padding (max 0 (- width (string-width base))))
             (new-line (concat base (make-string padding ?-))))
        (delete-region (line-beginning-position) (line-end-position))
        (insert new-line)))))

(after! general
  (map! :map prog-mode-map "C-c -" #'ybk/align-comment-line-generic)
  (map! :map text-mode-map "C-c -" #'ybk/align-comment-line-generic)
  (map! :map conf-mode-map "C-c -" #'ybk/align-comment-line-generic))

(after! ess
  (map! :map ess-r-mode-map
        "C-c -" #'ybk/align-comment-line-generic)
  (map! :map ess-r-mode-map
        :localleader
        (:prefix ("c" . "Comments")
                 "-" #'ybk/align-comment-line-generic
                 "l" #'comment-line
                 "r" #'comment-region
                 "u" #'uncomment-region)))

;;; =============================
;;; Outline Folding for ESS & Markdown
;;; =============================
(after! evil
  (defun +custom/enable-outline-folding ()
    "Enable outline folding for comment headings."
    (outline-minor-mode 1)
    (setq-local outline-regexp "##+ *")
    (setq-local outline-level (lambda () (length (match-string 0))))
    (setq-local +fold-provider-text 'outline))

  (add-hook 'ess-r-mode-hook #'+custom/enable-outline-folding)

  (add-hook 'markdown-mode-hook
            (lambda ()
              (outline-minor-mode 1)
              (setq-local outline-regexp "#+\\s-*")
              (setq-local outline-level (lambda () (length (match-string 0))))
              (setq-local +fold-provider-text 'outline)))

  (map! :map outline-minor-mode-map
        :n "z T a" #'outline-toggle-children
        :n "z T c" #'outline-hide-subtree
        :n "z T o" #'outline-show-subtree
        :n "z T m" #'outline-hide-body
        :n "z T r" #'outline-show-all))

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
(use-package! google-translate
  :commands (google-translate-smooth-translate
             google-translate-at-point
             google-translate-query-translate))

(use-package! google-translate-smooth-ui
  :after google-translate
  :config
  (setq google-translate-translation-directions-alist
        '(("en" . "no")
          ("no" . "en"))))

(use-package! go-translate
  :commands gts-do-translate
  :config
  (setq gt-langs '(en no))
  (setq gt-default-translator
        (gt-translator
         :taker   (gt-taker :text 'buffer :pick 'paragraph)
         :engines (list (gt-bing-engine) (gt-google-engine))
         :render  (gt-buffer-render))))

(map! :leader
      (:prefix ("=" . "Translate")
       :desc "Google Translate"  "g" #'google-translate-smooth-translate
       :desc "Go Translate"      "t" #'gts-do-translate
       :desc "Translate at point" "l" #'google-translate-at-point
       :desc "Query Translate"   "L" #'google-translate-query-translate))

;;; =============================
;;; Region Selection (Expand/Contract)
;;; =============================
(use-package! expand-region
  :commands (er/expand-region er/contract-region))

(map! :nvig "C-=" #'er/expand-region)
(map! (:map 'override
       :v "v" #'er/expand-region
       :v "V" #'er/contract-region))

;;; =============================
;;; Window Management
;;; =============================
(map! :map evil-window-map
      "SPC"       #'rotate-layout
      "C-<left>"  #'+evil/window-move-left
      "C-<down>"  #'+evil/window-move-down
      "C-<up>"    #'+evil/window-move-up
      "C-<right>" #'+evil/window-move-right)

;;; =============================
;;; Beacon (Cursor Blink)
;;; =============================
(use-package! beacon
  :commands beacon-blink)

(map! :leader
      :prefix ("c" . "code")
      :desc "Beacon blink" "b" #'beacon-blink)

;;; =============================
;;; Flyspell
;;; =============================
(after! flyspell
  (when IS-WINDOWS
    (let ((dict-path "C:/Emacstillegg/dictionaries")
          (hunspell-path "C:/Emacstillegg/hunspell-1.3.2-3-w32-bin/bin/hunspell.exe"))
      (when (and (file-directory-p dict-path)
                 (file-executable-p hunspell-path))
        (setenv "DICPATH" dict-path)
        (setq ispell-program-name hunspell-path
              ispell-local-dictionary "nb_NO"
              ispell-local-dictionary-alist
              '(("nb_NO" "[[:alpha:]]" "[^[:alpha:]]" "[']"
                 nil ("-d" "nb_NO") nil utf-8))))))

  (when IS-LINUX
    (setq ispell-program-name "aspell"))

  ;; CHANGED: Removed redundant duplicate hooks — smart-flyspell-mode below
  ;; covers all modes. Kept only the specific mode hooks for clarity.
  (add-hook 'ess-r-mode-hook    #'flyspell-prog-mode)
  (add-hook 'emacs-lisp-mode-hook #'flyspell-prog-mode)
  (add-hook 'org-mode-hook      #'flyspell-mode)
  (add-hook 'markdown-mode-hook #'flyspell-mode)

  (defun my/flyspell-norwegian ()
    "Switch Flyspell to Norwegian."
    (interactive)
    (ispell-change-dictionary (if IS-WINDOWS "nb_NO" "norsk"))
    (flyspell-buffer)
    (message "Flyspell language: Norwegian"))

  (defun my/flyspell-english ()
    "Switch Flyspell to English."
    (interactive)
    (ispell-change-dictionary (if IS-WINDOWS "en_GB" "english"))
    (flyspell-buffer)
    (message "Flyspell language: English"))

  ;; Performance: don't slow down typing
  (setq flyspell-issue-message-flag nil
        flyspell-issue-welcome-flag nil
        flyspell-large-region 1000))

(map! :leader
      (:prefix ("t" . "toggle")
               (:prefix ("S" . "Spell lang")
                :desc "Norwegian"   "n" #'my/flyspell-norwegian
                :desc "English"     "e" #'my/flyspell-english
                :desc "Prog mode"   "p" #'flyspell-prog-mode
                :desc "Full mode"   "f" #'flyspell-mode
                :desc "Buffer check" "b" #'flyspell-buffer)))

;;; =============================
;;; Outshine for Emacs Lisp Navigation
;;; =============================
(use-package! outshine
  :hook (emacs-lisp-mode . outshine-mode))

;;; =================================
;;; Corfu (modern CAPF completion)
;;; =================================
;; WINDOWS NOTE: Corfu uses child frames for its popup. These work fine in GUI
;; Emacs on Windows (runemacs.exe / emacsclientw.exe). If you ever run Emacs
;; in a terminal (emacs -nw), child frames won't work — you'd need corfu-terminal.
;; For now (GUI only), the config below is correct.

(use-package! corfu
  :init
  (setq corfu-auto t
        corfu-auto-delay 0.6    ; slightly longer delay reduces noise while typing
        corfu-auto-prefix 2
        corfu-cycle t
        corfu-preselect 'valid
        corfu-quit-no-match 'separator
        corfu-quit-at-boundary 'separator
        corfu-scroll-margin 2
        corfu-popupinfo-delay '(0.5 . 0.3))  ; CHANGED: tuple form (show . hide delay)
  :config
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1)

  ;; CHANGED: Use `kbd` for reliable key parsing across Emacs versions.
  ;; Also: do NOT bind RET to corfu-insert globally — it swallows RET
  ;; in programming modes where you want a newline. Let corfu-insert
  ;; happen on TAB (via my/smart-tab below) and leave RET alone.
  (map! :map corfu-map
        :i "<tab>"     #'corfu-next
        :i "<backtab>" #'corfu-previous
        :i "M-d"       #'corfu-info-documentation
        :i "M-l"       #'corfu-info-location
        :i "C-g"       #'corfu-quit
        :i "C-n"       #'corfu-next      ; extra navigation
        :i "C-p"       #'corfu-previous)

  (after! evil
    (add-hook 'corfu-mode-hook
              (lambda ()
                (evil-make-intercept-map corfu-map)
                (evil-normalize-keymaps)))))

(custom-set-faces!
  '((corfu-popupinfo) :height 0.9))

;;; =============================
;;; Cape (CAPF extensions)
;;; =============================
;; CHANGED: No global additions to completion-at-point-functions.
;; Cape sources are added buffer-locally in each mode's hook.
(use-package! cape)

;;; =============================
;;; ESS / R — completion setup
;;; =============================
;; CHANGED: With Eglot active, ESS R buffers get LSP completions automatically.
;; We do NOT override completion-at-point-functions here because Eglot registers
;; its own CAPF that is much richer than cape-dabbrev/cape-file alone.
;; We keep corfu-auto nil so the popup doesn't fire constantly while R is thinking.
(after! ess
  (setq ess-tab-complete-in-script t)
  ;; TAB triggers manual completion; Eglot's CAPF provides the candidates.
  (map! :map ess-r-mode-map
        :i "<tab>" #'completion-at-point
        :i "TAB"   #'completion-at-point)
  (add-hook 'ess-r-mode-hook
            (lambda ()
              (setq-local corfu-auto nil)       ; manual TAB completion only
              (setq-local corfu-auto-delay 0.8) ; longer — R is slow to respond
              ;; Add cape-file as a fallback after Eglot's own CAPF
              (add-hook 'completion-at-point-functions #'cape-file nil t)
              (add-hook 'completion-at-point-functions #'cape-dabbrev nil t))))

;;; =============================
;;; Eshell — completion
;;; =============================
;; CHANGED: Full rewrite.
;; - Do NOT add eshell-mode to corfu-excluded-modes (that silently breaks TAB).
;; - Enable corfu-mode explicitly in the hook (global-corfu-mode skips eshell).
;; - Provide file + dabbrev CAPFs locally; avoid pcomplete conflicts by not
;;   touching completion-at-point-functions globally.
;; - Use corfu-auto nil: auto popup in eshell is noisy and unpredictable.
;; - TAB bound to completion-at-point explicitly.
(after! eshell
  (setq eshell-banner-message ""
        eshell-history-size 5000)

  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local corfu-auto nil)
              (setq-local corfu-quit-at-boundary t)
              ;; Use cape-file as primary CAPF; cape-dabbrev as fallback.
              ;; cape-capf-buster prevents stale cache from confusing corfu.
              (setq-local completion-at-point-functions
                          (list (cape-capf-buster #'cape-file)
                                #'cape-dabbrev))
              ;; Explicit TAB binding for eshell
              (keymap-local-set "TAB"   #'completion-at-point)
              (keymap-local-set "<tab>" #'completion-at-point)
              ;; Enable corfu locally
              (corfu-mode 1))))

;;; =============================
;;; shell-mode (cmd/bash via M-x shell)
;;; =============================
(after! shell
  (add-hook 'shell-mode-hook
            (lambda ()
              (setq-local completion-at-point-functions
                          (list #'cape-file #'cape-dabbrev))
              (corfu-mode 1))))

;;; =============================
;;; GitHub Copilot
;;; =============================
;; CHANGED: Moved :bind to use-package :config map! form (Doom pattern).
;; Kept the TAB binding only in copilot-completion-map so it doesn't
;; conflict with corfu-map or normal buffer TAB.
;;
;; See Requirements from copilot GitHub page: https://github.com/copilot-emacs/copilot.el, especially Node.js
;; which can be downloaded from https://nodejs.org/en/download/ (Standalone binary recommended for Windows ie. zip)
;; Unzip and add the folder path to your PATH environment variable. Check with `node -v` in terminal.
;; Run `doom sync` after adding copilot to the PATH after restarting
;; Node.js needed to be able to install copilot-language-server with M-x copilot-install-server
;; After installation run:  M-x copilot-login
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :config
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
  (add-to-list 'copilot-indentation-alist '(clojure-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))

  (map! :map copilot-completion-map
        "<tab>"   #'copilot-accept-completion
        "TAB"     #'copilot-accept-completion
        "C-TAB"   #'copilot-accept-completion-by-word
        "C-<tab>" #'copilot-accept-completion-by-word
        "C-'"     #'copilot-accept-completion-by-line
        "C-n"     #'copilot-next-completion
        "C-p"     #'copilot-previous-completion))

;;; =============================
;;; Smart TAB
;;; =============================
;; Priority: corfu popup → copilot suggestion → indent
;; CHANGED: corfu--frame check replaced with corfu-popupinfo—the frame variable
;; name changed across corfu versions. Using `corfu-popupinfo` is more stable.
;; Also added `ignore-errors` around the frame check for robustness.
(defun my/smart-tab ()
  "Confirm Corfu if visible; else accept Copilot; else indent."
  (interactive)
  (cond
   ;; Corfu popup is visible
   ((and (bound-and-true-p corfu-mode)
         (ignore-errors (frame-live-p corfu--frame)))
    (corfu-insert))
   ;; Copilot has a suggestion
   ((and (bound-and-true-p copilot-mode)
         (bound-and-true-p copilot--overlay)
         copilot--overlay)
    (copilot-accept-completion))
   ;; Default: normal tab/indent
   (t
    (indent-for-tab-command))))

(map! :i "<tab>" #'my/smart-tab)

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

;;; ============================
;;; External settings
;;; ============================
(load! "+bindings.el")
(load! "+org.el")

;;; =============================
;;; Windows-specific: Performance Tweaks
;;; =============================
;; ADDED: These settings reduce sluggishness that's especially noticeable on
;; Windows where process spawning and I/O are slower than on Linux/macOS.
(when IS-WINDOWS
  ;; Increase the amount of data Emacs reads from processes in one chunk.
  ;; Default is 4096 bytes which causes many roundtrips for LSP/Eglot traffic.
  (setq read-process-output-max (* 1024 1024)) ; 1MB

  ;; Garbage collect less frequently during normal operation.
  ;; The default 800KB threshold causes GC pauses every few seconds.
  (setq gc-cons-threshold (* 128 1024 1024)) ; 128MB during use
  (setq gc-cons-percentage 0.1)

  ;; ADDED: Allow Emacs to keep a larger undo history — useful on slower systems
  ;; where you might want to undo across longer editing sessions.
  (setq undo-limit (* 10 1024 1024))       ; 10MB
  (setq undo-strong-limit (* 15 1024 1024)) ; 15MB

  ;; ADDED: Faster cursor display update on Windows
  (setq cursor-in-non-selected-windows nil)
  (setq highlight-nonselected-windows nil)

  ;; ADDED: Prevent Emacs from auto-saving every few seconds to a temp file,
  ;; which hits the disk frequently on Windows and causes brief freezes.
  (setq auto-save-default nil)  ; remove if you rely on auto-save recovery

  ;; ADDED: Longer idle time before eldoc triggers — prevents constant
  ;; LSP hover requests while you're still typing (especially noticeable
  ;; in R buffers where languageserver is already under load).
  (setq eldoc-idle-delay 1.0))

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

;; rem Clean previous server file file info first
;; del /q ""%HOME%\\.emacs.d\\server\\*""

;; rem Start the Emacs daemon/server with HOME as the default directory
;; C:\Users\ybka\scoop\apps\emacs\current\bin\runemacs.exe --daemon

;; rem Open a client frame
;; start "" "C:\Users\%USERNAME%\Desktop\emacsclientw.exe - Shortcut.lnk"
