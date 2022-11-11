;;; +bindings.el -*- lexical-binding: t; -*-

;; Global Keys
(global-set-key [f7] 'eshell) ; eshell is nice and soes what I need
(global-set-key [f8] 'dired)  ; dired, works well with eshell, better than neotree

;;; Personal keybindings
(map! :leader
      (:prefix ("y" . "My keys")
       :desc "Inbox"
       "i" #'open-inbox-file
       :desc "file-other-window"
       "f" #'find-file-other-window
       :desc "git-branch-refresh"  ;refresh branch name in modeline. Now use Magit "gv"
       "g" #'vc-refresh-state
       :desc "Norsk"
       "n" #'lang-norsk
       :desc "English"
       "e" #'lang-eng
       :desc "Encoding" ;Need when pasting from external
       "c" #'set-clipboard-coding-system
       :desc "LaTeX preview"
       "l" #'latex-preview-pane
       :desc "fold/toggle"    ;folds keys accessable with z in normal mode too
       "a" #'+fold/toggle     ;with similar keys but less explicit prefix
       :desc "fold/open-all"
       "r" #'+fold/open-all
       :desc "fold/close-all"
       "m" #'+fold/close-all
       :desc "vectorise"
       "v" #'vectorise
       :desc "load-theme"
       "t" #'cycle-themes
       :desc "maximize buffer/window"
       "w" #'toggle-frame-maximized
       :desc "shutdown-server"
       "q" #'server-shutdown)
      )
