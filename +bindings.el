;;; +bindings.el -*- lexical-binding: t; -*-

;; Global Keys
(global-set-key [f7] 'eshell) ; eshell is nice and soes what I need
(global-set-key [f8] 'dired)  ; dired, works well with eshell, better than neotree

;;; Personal keybindings
(map! :leader
      (:prefix ("y" . "My keys")
       :desc "fold/toggle"    ;folds keys accessable with z in normal mode too
       "a" #'+fold/toggle     ;with similar keys but less explicit prefix
       :desc "blog"
       "b" #'easy-hugo
       :desc "Encoding" ;Need when pasting from external
       "c" #'set-clipboard-coding-system
       :desc "copy absolute dir path"
       "d" #'xah-copy-file-path
       :desc "English"
       "e" #'my/flyspell-english
       :desc "Inbox"
       "i" #'open-inbox-file
       ;; :desc "file-other-window"
       ;; "f" #'find-file-other-window
       :desc "focus"
       "f" #'focus-mode
       ;; "p" #'ranger-copy-current-dir-path
       :desc "git-branch-refresh"  ;refresh branch name in modeline. Now use Magit "gv"
       "g" #'vc-refresh-state
       :desc "LaTeX preview"
       "l" #'latex-preview-pane
       :desc "fold/close-all"
       "m" #'+fold/close-all
       :desc "Norsk"
       "n" #'my/flyspell-norwegian
       :desc "org-present"
       "p" #'org-present
       :desc "fold/open-all"
       "r" #'+fold/open-all
       :desc "org link show"
       "s" #'org-toggle-link-display
       :desc "load-theme"
       "t" #'cycle-themes
       :desc "vectorise"
       "v" #'vectorise
       :desc "hide cursor"
       "x" #'org-present-hide-cursor
       :desc "show cursor"
       "y" #'org-present-show-cursor
       :desc "maximize buffer/window"
       "w" #'toggle-frame-maximized
       :desc "shutdown-server"
       "q" #'server-shutdown
       :desc "kill-emacs"
       "Q" #'kill-emacs)
      )

;; Default is [ SPC or ] SPC
(map! :leader
      ;;; <leader> i --- insert
      (:prefix-map ("i" . "insert")
       :desc "Insert line above"  "k"   #'+evil/insert-newline-above
       :desc "Insert line below"  "j"   #'+evil/insert-newline-below)
      )

;; Default to scale increase is C-M-= and reset is C-+
(map! :n "C-|"    #'doom/reset-font-size
      ;; Buffer-local font resizing
      :n "C-+"    #'doom/increase-font-size
      :n "C--"    #'doom/decrease-font-size
      ;; :n "C-+"    #'text-scale-increase
      ;; :n "C--"    #'text-scale-decrease
      )
