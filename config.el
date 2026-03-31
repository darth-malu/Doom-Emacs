;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(use-package! python
  :custom
  (python-shell-interpreter "ipython")
  ;; (python-shell-interpreter-args "-i --simple-prompt" ) ; --no-color-info
  ;; (python-shell-prompt-detect-failure-warning nil)
  ;; :config
  ;; (setq +python-ipython-repl-args '("-i" "--simple-prompt" "--no-color-info"))
  ;; (setq +python-jupyter-repl-args '("--simple-prompt"))
  (lsp-pyright-langserver-command "basedpyright")
  :hook
  (python-ts-mode . (lambda ()
                   (setq-local lsp-pyright-langserver-command "basedpyright") ;; pyright or basedpyright
                   (setq-local +format-with 'black)
                   ;; (lsp-deferred)
                   (local-set-key (kbd "C-c r") 'python-shell-send-region))))

(org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)
                                                          (julia . t)
                                                          (python . t)
                                                          (jupyter . t)))

(setq org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                    (:session . "py")
                                                    (:kernel . "python3")))

(after! ccls
  (setq ccls-initialization-options '(:index (:comments 2) :completion (:detailedLabel t)))
  (set-lsp-priority! 'ccls 1))

(use-package! nix-mode
:after lsp-mode
:hook
(nix-mode . lsp-deferred) ;; So that envrc mode will work
:custom
(lsp-disabled-clients '((nix-mode . nix-nil))) ;; Disable nil so that nixd will be used as lsp-server
:config
(setq lsp-nix-nixd-server-path "nixd"
      lsp-nix-nixd-formatting-command [ "nixfmt" ]
      lsp-nix-nixd-server-arguments '("--inlay-hints=true" "--semantic-tokens=true")
      ;; lsp-nix-nixd-nixpkgs-expr "import <nixpkgs> { }"
      lsp-nix-nixd-nixos-options-expr "(builtins.getFlake \"/home/malu/Shibuya\").nixosConfigurations.carthage.options"
      lsp-nix-nixd-home-manager-options-expr "(builtins.getFlake \"/home/malu/Shibuya\").nixosConfigurations.carthage.options.home-manager.users.type.getSubOptions []"))

(setq treesit-language-source-alist
      '((astro "https://github.com/virchau13/tree-sitter-astro")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")))

(dolist (lang '(astro css tsx))
  (unless (treesit-language-available-p lang)
    (treesit-install-language-grammar lang)))

(use-package! astro-ts-mode
  :mode "\\.astro\\'"
  :hook
  (astro-ts-mode . (lambda () (abbrev-mode) (diff-hl-mode -1) (spell-fu-mode -1)))
  :init
  (when (modulep! +lsp)
    (add-hook 'astro-ts-mode-hook #'lsp! 'append))
  :config
  (after! lsp-mode
    (add-to-list 'lsp-language-id-configuration '(astro-ts-mode . "astro-ts"))

    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection "astro-ls")
                      :activation-fn (lsp-activate-on "astro-ts")
                      :server-id 'astro-ls))))
;; (setq prettier-js-command "prettierd"
;;       prettier-js-args '("--no-editorconfig")))

;; (use-package! prettier-js
;;   :hook
;;   (web-mode . prettier-js-mode)
;;   ;; (astro-ts-mode . prettier-js-mode)
;;   (js-mode . prettier-js-mode))

(after! apheleia
  (set-formatter! 'prettier-astro
    '("bunx" "prettier" "--parser=astro" ;
      (apheleia-formatters-indent "--use-tabs" "--tab-width" 'astro-ts-mode-indent-offset))
    :modes '(astro-ts-mode))
  (add-to-list 'apheleia-mode-alist '(astro-ts-mode . prettier-astro)
               ))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-ts-mode))

;; (use-package! lsp-tailwindcss :after lsp-mode)

(use-package! lsp-tailwindcss
  ;; :when (modulep! +lsp)
  :after lsp-mode
  :init
  (setq lsp-tailwindcss-add-on-mode t)

  :config
  (add-to-list 'lsp-tailwindcss-major-modes 'astro-ts-mode))

(use-package! qml-ts-mode
  :after lsp-mode
  :config
  (add-to-list 'lsp-language-id-configuration '(qml-ts-mode . "qml-ts"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("qmlls"))
                    :activation-fn (lsp-activate-on "qml-ts")
                    :server-id 'qmlls))
  :hook
  (qml-ts-mode . (lambda () 
                   (setq-local electric-indent-chars '(?\n ?\( ?\) ?{ ?} ?\[ ?\] ?\; ?,))
                   (lsp-headerline-breadcrumb-mode 1)
                   (lsp-deferred))))

(use-package! direnv
 :config
 (direnv-mode))

(use-package yeetube
  :init (define-prefix-command 'my/yeetube-map)
  :bind (("C-c y" . 'my/yeetube-map)
          :map my/yeetube-map
          ("RET" . 'yeetube-play)     
          ("s" . 'yeetube-search)
          ("b" . 'yeetube-play-saved-video)
          ("d" . 'yeetube-download-videos)
          ("p" . 'yeetube-mpv-toggle-pause)
          ("v" . 'yeetube-mpv-toggle-video)
          ("V" . 'yeetube-mpv-toggle-no-video-flag)
          ("k" . 'yeetube-remove-saved-video)))

(define-abbrev global-abbrev-table "meab" "This is my first abbrev")

(defun my-current-time ()
  (insert (format-time-string "%A,%B %e%t%T")))

(define-abbrev org-mode-abbrev-table "mytime" "" 'my-current-time)

(set-popup-rules!
  '(("\\*Occur\\*" :select t :side bottom :actions (display-buffer-in-side-window) :ttl 5 :quit t)
    ("\\*doom:scratch*" :quit t)
    ("\\*info*" :quit t :side right :select t :width +popup-shrink-to-fit)
    ("\\*ielm*" :quit t :side right :select t :width +popup-shrink-to-fit)
    ("^\\*WoMan.*\\*" :quit t :side right :width 70)
    ;; ("\\*ein: http.*\\*" :select t :side left :width 80)
    ;; ("\\*ein:notebooklist.*\\*" :select t :side bottom :actions (display-buffer-in-side-window))
    ))

;;; :ui doom-dashboard
(setq fancy-splash-image (file-name-concat doom-user-dir "emacs.png")
      ;; initial-buffer-choice #'eshell
      ;; +doom-dashboard-name "maluware"
      +dashboard-name "maluware"
      +dashboard-functions '(+dashboard-widget-banner +dashboard-widget-shortmenu))
;; Hide the menu for as minimalistic a startup screen as possible.

(setq doom-symbol-font (font-spec :family "Symbols Nerd Font")
       doom-font (font-spec :family "JetBrains Mono"
                           :size 15
                           :weight 'regular)
       doom-emoji-font (font-spec :family "Noto Color Emoji")
       doom-variable-pitch-font (font-spec :family "VictorMono Nerd Font" :size 15 :weight 'semibold))

;; (set-font-ligatures! '(org-mode) ">>=" ">>-")
(after! org
  (set-ligatures! 'org-mode
    :def "ƒ"))

(custom-set-faces!
  '(mode-line :family "Mononoki Nerd Font" :box nil :overline nil)
  ;; '(doom-modeline-buffer-modified :foreground "green") ; color of modified buffer indicator
  '(mode-line-inactive :family "VictorMono Nerd Font" :weight bold :height 0.95))

(setq projectile-known-projects-file (expand-file-name "tmp/projectile-bookmarks.eld" user-emacs-directory)
      lsp-session-file (expand-file-name "tmp/.lsp-session-v1" user-emacs-directory))

(setq backup-directory-alist '(("." . "~/.local/share/Trash/files"))) ; delete to trash instead of create backup files with .el~ suffix (alot of clutter)
;; (setq user-emacs-directory (expand-file-name "~/.cache/emacs")) ;default is in .emacs dir cache

;; Auto-save-mode doesn't create the path automatically!
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)

(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

(use-package! emacs
  :init
  (setq custom-file (expand-file-name "custom.el" doom-user-dir))
  :custom
  ;; latex
  ;; (org-latex-compiler 'lualatex)        ;pdflatex::
  ;; (org-preview-latex-default-process 'dvisvgm) ;dvipng
  ;; (org-super-agenda-mode t)
  (epg-pinentry-mode 'loopback)
  (tab-width 2)
  (tab-always-indent 'complete)
  (tab-first-completion nil) ;; word 'word-or-paren-or-punct
  ;; (completion-styles '(orderless basic partial-completion emacs22)) ; flex (orderless basic) ; flex -fuzzy find
  (display-line-numbers-type nil) ;numbers, relative , - perfomance enhance...turn on if needed
  ;; (auto-save-default t)
  ;; (auto-save-timeout 10) ;;30:-:
  ;; (auto-save-interval 200) ;; NOTE: Must be -gt 20...emacs will behave as if value is 20 if less
  ;; (undo-limit 80000000)
  (delete-by-moving-to-trash t) ; use system trash can
  ;; (x-stretch-cursor t) ; see if needed really
  (bookmark-save-flag 1) ; TODO see docs
  ;; (uniquify-buffer-name-style 'post-forward) ;nil::

  (inhibit-startup-message t)           ;Tutorial Page lol---useless with doom?

  ;; (doom-fallback-buffer-name " ") ;last page...creat if does not exist

  (+evil-want-o/O-to-continue-comments nil) ; o/O does not continue comment to next new line 😸
  (+default-want-RET-continue-comments nil)
  ;; (evil-move-cursor-back nil)               ; don't move cursor back one CHAR when exiting insert mode

  (evil-shift-width 2)
  (eros-eval-result-prefix "⟹ ") ; default =>
  (user-full-name "Justin Malu") ; foor GPG config, email clients, file templates & snippets ; optional
  (user-mail-address "justinmalu@gmail.com")

  ;; magit
  (magit-view-git-manual-method 'woman)

  ;;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Scrolling.html
  (scroll-margin 8)
  (scroll-conservatively 101)

  ;; TODO: check what the numbers mean in doom
  ;; (doom-modeline-check-simple-format t)
  ;; (doom-modeline-check-icon t)
  ;; (doom-modeline-height 3)             ;5:: -If charheight is larger its respected instead
  ;; (doom-modeline-hud t)       ;Only respected in GUI
  ;;; ON BY DEFAULT
  ;; (doom-modeline-buffer-file-name-style 'auto)
  ;; (doom-modeline-major-mode-color-icon t)
  ;; (doom-modeline-lsp-icon t)
  ;; (doom-modeline-buffer-name t)
  ;; Whether display the minor modes in the mode-line.
  ;; (doom-modeline-minor-modes nil)
  ;; (doom-modeline-vcs-icon t)
  ;; (doom-modeline-vcs-max-length 15)
  (doom-modeline-modal nil)
  ;; (doom-modeline-modal-icon t)
  ;; (doom-modeline-modal-modern-icon t) ;; Whether display the modern icons for modals.
  
  (doom-modeline-always-show-macro-register t) ;show register name when recording a macro

  ;; Major modes in which to display word count continuously.
  ;; Also applies to any derived modes. Respects `doom-modeline-enable-word-count'.
  ;; If it brings the sluggish issue, disable `doom-modeline-enable-word-count' or
  ;; remove the modes from `doom-modeline-continuous-word-count-modes'.
  ;; (doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
  

  ;; (lsp-ui-sideline-show-code-actions t) ;nil::

  ;;(display-time-mode 1)                             ; Enable time in the line-mode
  ;; (display-time-format "%H:%M")
  ;;(display-time-default-load-average nil)
  ;; (display-time-day-and-date 1)

  :config
  (defalias 'man 'woman)
  ;; (global-set-key [escape] 'keyboard-escape-quit) ; By default, Emacs requires you to hit ESC three times to escape quit the minibuffer. ; test this further
  (global-auto-revert-mode t)
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys)
  (customize-set-variable 'uniquify-buffer-name-style 'post-forward)
  (customize-set-variable 'uniquify-separator " ❄ ") ;💎 🧿💢
  ;; (customize-set-variable 'ein:jupyter-server-use-command 'server)
  ;; (customize-set-variable 'ein:jupyter-server-use-subcommand "server")

  :bind
  ((
   :map evil-normal-state-map
        ;;;misc
        ("M-;" . save-buffer)      
        ;; ("M-s" . save-buffer)
        ;; ("C-s" . save-buffer)
        ("<mouse-8>" . previous-buffer)
        ("<mouse-9>" . next-buffer)
        ("C-M-o" . consult-outline)

        ;;; EOL, BOL
        ("M-l" . end-of-line) ; clash with other settings - capitalise, org-metaright
        ("M-h" . beginning-of-line-text)
        ("M-S-l" . end-of-visual-line)
        ("M-S-h" . beginning-of-visual-line)

        ;;; insert newline below/above
        ("M-o" . +evil/insert-newline-below)
        ("M-O" . +evil/insert-newline-above)

        ;;; NOTE Too much conflict just use native C-w
        ;; Unify Kitty + Emacs window focusing
        ("C-M-l" . evil-window-right)
        ("C-M-h" . evil-window-left)
        ("C-M-k" . evil-window-up)
        ("C-M-j" . evil-window-down)
        ("C-M-j" . previous-window)
        ("C-M-RET" . evil-window-vsplit) ; Hook This up only in prog mode to prevent conflict with org

        ("C-'" . olivetti-mode)

    :map evil-insert-state-map
      ("M-/" . #'org-comment-dwim) 

    :map doom-leader-map
      ("to" . hl-todo-occur)
      ("I" . ielm)
      ("SPC" . ace-window)
    )))

(customize-set-variable '+format-on-save-disabled-modes '(nxml-mode)) ;Android studio

(setq backward-delete-char-untabify-method 'all)

(use-package! elcord
  :commands elcord-mode
  :custom
  (elcord-display-elapsed nil)
  (elcord-idle-message "Sipo Kwenye Keyboard...👻")
  :config
  ;; (elcord-mode 1)
  (setq elcord--editor-name "Church of Emacs"
        elcord-use-major-mode-as-main-icon t
        ))

(use-package! org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  ;; (setq org-auto-tangle-default t) ; set auto_tangle: nil for buffers not to auto tangle
  (setq org-auto-tangle-babel-safelist '("~/system.org" "~/test.org")))

(use-package! corfu
  :init
  (customize-set-variable 'corfu-auto nil))

;; Trying to save workspaces
(after! persp
  ;; Auto-save workspaces when Emacs exits
  (setq persp-auto-save-opt 1
         ;; Save all workspace info including window configurations
         persp-set-last-persp-for-new-frames nil
         persp-reset-windows-on-nil-window-conf nil
         ;; Load workspaces automatically on startup
         persp-auto-resume-time -1.0))

;; (after! spell-fu
;;   (setq spell-fu-idle-delay 0.5)  ; default is 0.25
;;   ;; (setq spell-fu-faces nil)
;;   (setq spell-fu-ignore-modes (list 'org-mode) )
;;   )

(use-package! spell-fu
  :defer t
  :custom
  (spell-fu-ignore-modes (list 'org-mode 'markdown-mode 'html-mode 'prog-mode 'astro-ts-mode))
  (spell-fu-global-ignore-buffer (lambda (buf)
                                   (buffer-local-value 'buffer-read-only buf)))
  ;; (spell-fu-global-mode nil)
  :config
  (spell-fu-global-mode -1)
  ;; :hook
  ;; (emacs-lisp-mode . (lambda () (spell-fu-mode)))
  ;; (org-mode . (lambda ()
  ;;             (setq-local spell-fu-faces-exclude
  ;;               '(org-block-begin-line
  ;;                 org-block-end-line
  ;;                 org-code
  ;;                 org-date
  ;;                 org-drawer org-document-info-keyword
  ;;                 org-ellipsis
  ;;                 org-link
  ;;                 org-meta-line
  ;;                 org-properties
  ;;                 org-properties-value
  ;;                 org-special-keyword
  ;;                 org-src
  ;;                 org-tag
  ;;                 org-verbatim))))
)

(use-package! evil-nerd-commenter
  :defer t
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package! hl-todo
  :hook (org-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces `(("TODO"       warning bold)
                                ("FIXME"      error bold)
                                ("!TODO"    error bold)
                                ("HACK"       font-lock-constant-face bold)
                                ("SEEKGOD"       font-lock-constant-face bold)
                                ("KESHO"       font-lock-constant-face bold)
                                ("REVIEW"     font-lock-keyword-face bold)
                                ("NOTE"       success bold)
                                ("DEPRECATED" font-lock-doc-face bold))))

(custom-set-faces!
  '(aw-leading-char-face
    :foreground "white" :background "red"
    :weight bold :height 2.5 :box (:line-width 10 :color "red")))

(use-package! projectile
  :init
  (setq projectile-project-search-path '(
                                         ;; ("~/Development" . 1)
                                         "~/Development/SkunkWorks"
                                         ;; ("~/USIU" . 1)
                                         "~/USIU/2026"
                                         "~/Development/Quickshell-Inspiration"
                                         ))
  :custom
  (projectile-auto-cleanup-known-projects t))

(defun my-org-mode-setup ()
  (abbrev-mode)
  (spell-fu-mode -1)
  (diff-hl-mode -1))

(defvar my/usiu-files
  (directory-files-recursively "~/USIU/2026" "\\.org$"))

(use-package! org
  :hook
  (org-mode . my-org-mode-setup)
  :init
  (setq org-directory (expand-file-name "~/Documents/IMPORTANT/Org")
        org-agenda-files `(,org-directory ,(file-name-concat org-directory "roam") ,@my/usiu-files)
        org-noter-notes-search-path (list (file-name-concat org-directory "notes"))
        org-default-notes-file (expand-file-name  ".notes" org-directory))
  (load! "maluware-org-agenda") 

  :custom
  (org-log-done 'time) ; task done with timestamp
  ;; (org-log-done-with-time nil)
  ;; (org-log-done 'note) ;task done with note prompted to user
  (org-log-into-drawer t)
  (org-hide-emphasis-markers t)

  (org-tag-alist
      '(("@home" . ?h) ("@school" . ?s)

        ("@carthage" . ?C) ("@tangier" . ?T)

        ("@work" . ?w) ("@pyrple" . ?p) ("@youtubr" . ?y)
        ("@emacs" . ?e) ("@linux" . ?l) ("@nix" . ?n)))

  (org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w!)"  "|" "DONE(d!)" "CANCEL(c!)")))

;; ((sequence "TODO(t)" "PROJ(p)" "LOOP(r)" "STRT(s)" "WAIT(w)" "HOLD(h)" "IDEA(i)"
;;            "|" "DONE(d)" "KILL(k)")
;;  (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
;;  (sequence "|" "OKAY(o)" "YES(y)" "NO(n)"))

  (org-agenda-custom-commands
        `(("S" "School Tasks" tags-todo "@school")
          ("n" "Linux + Nix" tags-todo "@nix+@linux")
          ("d" "Today's view"
            ((tags-todo "+PRIORITY=\"A\"" ((org-agenda-block-separator nil)
                                           (org-agenda-overriding-header "\nDaily agenda view 😀\n\nHigh PRIORITY tasks 🔥")))
             (agenda ""
                     ((org-agenda-block-separator nil)
                      (org-agenda-span 1)
                      (org-agenda-overriding-header "\n")
                      (org-agenda-start-day nil)))
                      
             (todo "WAIT"
                   ((org-agenda-block-separator nil)
                    (org-agenda-overriding-header "\nTasks on hold ⏳")))))
          ("u" "untagged tasks" tags-todo "-{.+}" ((org-agenda-overriding-header "Untagged Tasks")))
          ("p" "Protesilaos" ,maluware-custom-org-daily-agenda))))


  ;; (calendar-week-start-day 1)  ; 0 - sun, 1 -mon
  ;; (org-todo-keywords
  ;;     '((sequence "TODO(t)" "|" "DONE(d)")
  ;;       (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")))

(after! org-roam
  (setq org-roam-directory (expand-file-name "roam" org-directory)
        org-roam-db-location (file-name-concat org-roam-directory ".org-roam.db")
        org-roam-dailies-directory (expand-file-name "Journal" org-roam-directory))
  (org-roam-db-autosync-mode))

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(use-package! org-capture
  :bind ("C-c c" . org-capture)
  :custom
  ;; (require 'prot-org)
  (org-capture-templates '(
                           ("e" "EMACs" plain
                            (file+headline "EmacsTODO.org" "TODO emacs") ;Create if does not exist otherwise ins under it
                            "+ [ ] %?")

                           ;; NixOs
                           ("n" "nixOs" plain
                            (file+headline "nixTODO.org" "TODO nixOs")
                            "+ [ ] %?"
                            :prepend t)

                           ("a" "App Ideas" plain
                            (file+headline "appsTODO.org" "TODO App Ideas")
                            "+ [ ] %?"
                            :prepend t)

                           ;; Quickshell
                           ("Q" "Quickshell + QML")
                           ("QQ" "Quickshell" plain
                            (file+headline "QuickshellTODO.org" "TODO Quickshell")
                            "+ [ ] %?")
                           ("QM" "QML" plain
                            (file+headline "QuickshellTODO.org" "TODO QML")
                            "+ [ ] %?")

                           ;; BUCKET LIST
                           ("b" "Bucket List [ movies anime books youtube]") ; group 'em up
                           ("bm" "movies" plain
                            (file+headline "bucket-list.org" "Movies")
                            "+ [ ] %?"
                            :prepend t)
                            ("ba"  "Anime List (Movies + Series)")  ;; Just a label, optional dummy
                            ("bam" "Anime Movie"  plain
                            (file+headline "bucket-list.org" "Anime")
                            "+ [ ] %?" :prepend t)
                            ;; ANIME
                            ("bas" "Anime Series" plain
                            (file+headline "bucket-list.org" "Anime")
                            "+ [ ] %?" :prepend t)
                           ("bb" "books" plain
                            (file+headline "bucket-list.org" "Books")
                            "+ [ ] %?"
                            :prepend t)
                           ;;YOUTUBE
                           ("by" "youtube" plain
                            (file+headline "bucket-list.org" "YouTube")
                            "+ [ ] %?"
                            :prepend t)

                           ;; Learning New Languages
                           ("l" "Learn Programming Languages")
                           ("lp" "Python" plain
                            (file+headline "ProgrammingLanguagesTODO.org" "TODO PythonLearning")
                            "+ [ ] %?")
                           ("lc" "C" plain
                            (file+headline "ProgrammingLanguagesTODO.org" "TODO C")
                            "+ [ ] %?")
                           ("ln" "Nix" plain
                            (file+headline "ProgrammingLanguagesTODO.org" "TODO Nix")
                            "+ [ ] %?")

                           ;; School
                           ("s" "School - USIU" plain
                            (file+headline "USIU_TODO.org" "TODO school work")
                            "+ [ ] %?")

                           ;; Life's Morsels
                           ("d" "Life's Morsels")
                           ("dw" "words [w]" plain
                            (file+headline "diction.org" "Words") ;TODO see if this can support yassnippets
                            "\n\n %?"
                            :empty-lines 1
                            :prepend t)
                           ("di" "idioms [i]" plain
                            (file+headline "diction.org" "Idioms")
                            "+ %?"
                            :empty-lines 1
                            :prepend t)
                            ("dq" "quotes [q]" plain
                            (file+headline "diction.org" "Quotes")
                            ;; Template string starts here
                            "#+begin_quote\n%i%?\n#+end_quote\n"
                            :empty-lines 1
                            :prepend t)
                            ("dp" "phrases [p]" plain
                            (file+headline "diction.org" "Phrases")
                            ;; Template string starts here
                            "#+begin_quote\n%i%?\n#+end_quote\n"
                            :empty-lines 1
                            :prepend t)
                           ("v" "Video Ideas 📷")
                           ("ve" "Emacs 📃")
                           ("vei" "Emacs ideas ✔️" plain
                            (file+headline "emacsVideoIdea.org" "Random Idea")
                            " %?"
                            :prepend t
                            :empty-lines 1
                            )
                           ("vej" "emacs jolts ✏️" plain
                            (file+headline "emacsVideoIdea.org" "Random Jolt")
                            " %?"
                            :prepend t
                            :empty-lines 1)
                           )))



(setq emacs-everywhere-frame-name-format "emacs-everywhere")

;; Function to be run when org-agenda is opened
(defun org-agenda-open-hook()
  "Hook to be run when org-agenda is opened"
  (olivetti-mode))

;; Adds hooks to org agenda mode, making follow mode active in org agenda
(add-hook 'org-agenda-mode-hook 'org-agenda-open-hook)

(defun my/markdown-toggler ()
  (interactive)
  (if (eq #'markdown-view-mode)
      (markdown-mode)
    (markdown-view-mode)))
