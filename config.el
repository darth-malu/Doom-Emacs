;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

(use-package! elpy
  :defer t
  :init
  (advice-add 'python-ts-mode :before 'elpy-enable)
  :config
  ;; (elpy-folding-fringe-indicators t)
  ;; (elpy-modules (delq 'elpy-module-flymake elpy-modules))
  ;; (setq elpy-modules (delq 'elpy-module-highlight-indentation elpy-modules)) ; disable elpy indentation guide
  ;; (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (setq elpy-modules (cl-set-difference
                      elpy-modules
                      '(elpy-module-highlight-indentation
                        elpy-module-flymake)))
  :hook
  (elpy-mode . flycheck-mode))

(use-package! python
  :custom
  (python-shell-interpreter "ipython")
  ;; (python-shell-interpreter-args "-i --simple-prompt" ) ; --no-color-info
  ;; (python-shell-prompt-detect-failure-warning nil)
  ;; :config
  ;; (setq +python-ipython-repl-args '("-i" "--simple-prompt" "--no-color-info"))
  ;; (setq +python-jupyter-repl-args '("--simple-prompt"))
  :hook
  (python-ts-mode . (lambda ()
                   (setq-local lsp-pyright-langserver-command "basedpyright") ;; pyright or basedpyright
                   (setq +format-with 'black)
                   (lsp-deferred)
                   (local-set-key (kbd "C-c r") 'python-shell-send-region))))

(setq lsp-pyright-langserver-command "basedpyright")

(after! lsp-mode
  (setq lsp-enable-symbol-highlighting nil
        lsp-enable-suggest-server-download nil))

(use-package lsp-treemacs
  :after lsp-mode)                           ; TODO check if need lsp or lsp-mode

(after! ccls
  (setq ccls-initialization-options '(:index (:comments 2) :completion (:detailedLabel t)))
  (set-lsp-priority! 'ccls 1))

(use-package! lsp-nix
  ;; :ensure lsp-mode
  :after (lsp-mode)
  :demand t
  :custom
  (lsp-disabled-clients '((nix-mode . nix-nixd))) ;; TODO test if nixdd is on or need disabling
  (lsp-nix-nil-formatter ["nixfmt"]))

(use-package qml-ts-mode
  :after lsp-mode
  :config
  (add-to-list 'lsp-language-id-configuration '(qml-ts-mode . "qml-ts"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("qmlls"))
                    :activation-fn (lsp-activate-on "qml-ts")
                    :server-id 'qmlls))
  :hook
  (qml-ts-mode (lambda () (setq-local electric-indent-chars '(?\n ?\( ?\) ?{ ?} ?\[ ?\] ?\; ?,)
                                 ;; lsp-headerline-breadcrumb-mode t
                                 )
                    (lsp-deferred))))

(setq
  doom-symbol-font (font-spec :family "Symbols Nerd Font")
  doom-font (font-spec :family "JetBrains Mono"
                       :size 15
                       :weight 'regular)
  doom-emoji-font (font-spec :family "Noto Color Emoji")
  doom-variable-pitch-font (font-spec :family "VictorMono Nerd Font" :size 15 :weight 'semibold))

(defun darth-select-window (window)
    "NOTE Docuementation expects a window arg"
    (select-window window))

(add-to-list 'display-buffer-alist
        ;; List of display functions (Emacs will use the first successful one)
        `("\\*Occur\\*"
        (display-buffer-reuse-mode-window display-buffer-below-selected)
        ;; Parameters
        (window-height . fit-window-to-buffer)
        (dedicated . t)
        (body-function . darth-select-window)))

(custom-set-faces!
  ;; '(mode-line :family "Iosevka Comfy" :size 15)
  '(mode-line :family "Mononoki Nerd Font" :box nil :overline nil)
  ;; '(doom-modeline-buffer-modified :foreground "green") ; color of modified buffer indicator
  '(mode-line-inactive :family "Iosevka Comfy"))

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
  ;; Put Emacs auto-generated junk in a separate file
  ;; TODO see this in action --- PS its not junk lol who wrote this
  (setq custom-file (expand-file-name "custom.el" doom-user-dir))
  :custom
  ;; (org-super-agenda-mode t)
  (epg-pinentry-mode 'loopback)
  (tab-width 2)
  (tab-always-indent 'complete)
  (tab-first-completion nil) ;; word 'word-or-paren-or-punct)
  ;; (completion-styles '(orderless basic partial-completion emacs22)) ; flex (orderless basic) ; flex -fuzzy find
  (display-line-numbers-type nil) ;numbers, relative , - perfomance enhance...turn on if needed
  ;; (auto-save-default t)
  ;; (auto-save-timeout 10)
  ;; (auto-save-interval 200)
  ;; (undo-limit 80000000)
  (delete-by-moving-to-trash t) ; use system trash can
  ;; (x-stretch-cursor t) ; see if needed really
  (bookmark-save-flag 1) ; TODO see docs
  ;; (uniquify-buffer-name-style 'post-forward) ;nil::

  ;; (inhibit-startup-message t)           ;Tutorial Page lol---useless with doom?

  (doom-fallback-buffer-name "Doom Emacs") ; *doom*
  (+doom-dashboard-name "Darth Doom")

  (+evil-want-o/O-to-continue-comments nil) ; o/O does not continue comment to next new line 😸
(+default-want-RET-continue-comments nil)
  ;; (evil-move-cursor-back nil)               ; don't move cursor back one CHAR when exiting insert mode

  (evil-shift-width 2)

  (user-full-name "Justin Malu") ; foor GPG config, email clients, file templates & snippets ; optional
  (user-mail-address "justinmalu@gmail.com")

  ;;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Scrolling.html
  (scroll-margin 18)
  (scroll-conservatively 101)

  (doom-modeline-modal nil)             ;display mode -> NORMAL,INSERT,VISUAL
  (doom-modeline-check-simple-format t)

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

  :bind
  ((
   :map evil-normal-state-map
        ;;;misc
        ("M-;" . save-buffer)
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

    :map doom-leader-map ("to" . hl-todo-occur)
    ) ; bind conses
    ); end bind
  )
;; ("U" . evil-redo)

(customize-set-variable 'uniquify-buffer-name-style 'post-forward)
(customize-set-variable 'uniquify-separator " ❄ ") ;💎 🧿💢

(customize-set-variable '+format-on-save-disabled-modes '(nxml-mode)) ;Android studio

(setq backward-delete-char-untabify-method 'all)

(defun split-and-follow-horizontally ()
	(interactive)
	(split-window-below)
	(balance-windows)
	(other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
	(interactive)
	(split-window-right)
	(balance-windows)
	(other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

;;; :ui doom-dashboard
(setq fancy-splash-image (file-name-concat doom-user-dir "gorl.jpg"))
;; Hide the menu for as minimalistic a startup screen as possible.
(setq +doom-dashboard-functions '(doom-dashboard-widget-banner))

(use-package! corfu
  :init
  (customize-set-variable 'corfu-auto nil))

(use-package! evil-nerd-commenter
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

(use-package! elcord
  :commands elcord-mode
  :custom
  (elcord-display-elapsed nil)
  (elcord-idle-message "Sipo Kwenye Keyboard...👻")
  :config
  ;; (elcord-mode 1)
  (setq elcord--editor-name "Church of Emacs"
        elcord-use-major-mode-as-main-icon t))

(custom-set-faces!
  '(aw-leading-char-face
    :foreground "white" :background "red"
    :weight bold :height 2.5 :box (:line-width 10 :color "red")))

(use-package! org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  ;; :hook (org-src-mode . org-auto-tangle-mode)
  :config
  ;; (setq org-auto-tangle-default t) ; set auto_tangle: nil for buffers not to auto tangle
  (setq org-auto-tangle-babel-safelist '("~/system.org" "~/test.org")))

(use-package! projectile
  :init
  (setq projectile-project-search-path '(("~/.code/" . 1) "~/.code/SkunkWorks/"))
  :custom
  (projectile-auto-cleanup-known-projects nil))

(use-package! org
  :init
  (setq org-directory "~/org" ; trailing slash important or use expand-file-name(convert file name to absolute and canonicalize/standardize it)
        ;; org-default-notes-file (concat org-directory "/notes.org")
        org-agenda-files (list org-directory)
        org-default-notes-file (expand-file-name  "notes.org" org-directory))
  :hook
  (org-mode . (lambda ()
                ;; (vi-tilde-fringe-mode -1)
                ;; (display-line-numbers-mode -1) ;; in emacs init now so no need
                (abbrev-mode)
                (spell-fu-mode -1)
                (diff-hl-mode -1)))
  :config
  (defun my-current-time ()
    (insert (format-time-string "%A,%B %e%t%T")))
  (define-abbrev org-mode-abbrev-table "mytime" "" 'my-current-time)
  :custom
  ;; (org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕"))
  (org-log-done 'time) ; task done with timestamp
  ;; (org-log-done-with-time nil)
  ;; (org-log-done 'note) ;task done with note prompted to user
  (org-log-into-drawer t)
  (org-hide-emphasis-markers t)

  (org-tag-alist
      '(;;Places
        ("@home" . ?H)
        ("@school" . ?S)
        ("@babe" . ?B)
        ;;devices
        ("@carthage" . ?C)
        ("@tangier" . ?T)
        ;;activites
        ("@work" . ?W)
        ("@pyrple" . ?P)
        ("youtubr" . ?Y)
        ("@emacs" . ?E)
        ("@nix" . ?N)))

  (org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w!)"  "|" "DONE(d!)" "CANCEL(c!)"))))
  ;; (org-todo-keywords
  ;;     '((sequence "TODO(t)" "|" "DONE(d)")
  ;;       (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")))

;; (setq org-roam-directory (file-truename "~/org/roam"))
(setq org-roam-directory (file-truename "~/org/roam")
      org-roam-db-location (file-name-concat org-roam-directory ".org-roam.db")
      org-roam-dailies-directory "journal/") ;
  ;; :custom
  ;; (org-roam-completion-everywhere t) ;default t
  ;; :bind (("C-c n l" . org-roam-buffer-toggle)
  ;;        ("C-c n f" . org-roam-node-find)
  ;;        ("C-c n i" . org-roam-node-insert)
  ;;        ("C-c n c" . org-roam-capture)
         ;; Dailies
         ;; ("C-c n j" . org-roam-dailies-capture-today))
  ;; :config
  ;; (org-roam-db-autosync-enable))

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
                           ("l" "Programming Languages")
                           ("lp" "Python" plain
                            (file+headline "ProgrammingLanguagesTODO.org" "TODO PythonLearning")
                            "+ [ ] %?")
                           ("lc" "C" plain
                            (file+headline "ProgrammingLanguagesTODO.org" "TODO C")
                            "+ [ ] %?")
                           ("ln" "Nix" plain
                            (file+headline "ProgrammingLanguagesTODO.org" "TODO Nix")
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

(setq org-agenda-files
      (list "~/org")
      ;; '("~/org/agenda")
      ;; (list "inbox.org")
      )

(load! "maluware-org-agenda") ; imports maluware-orgAgenda.el

;; (setq org-agenda-files (list "inbox.org"))

(setq org-agenda-custom-commands
      `(
        ("D" "Today's view"
         ((todo "WAIT"
                ((org-agenda-overriding-header "Tasks on hold\n")))
          (agenda ""
                  ((org-agenda-block-separator nil) ;"*"
                   (org-agenda-span 1) ;7:: how many days should it span
                   (org-deadline-warning-days 0) ; remove warnings for events not for today
                   ;; (org-agenda-day-face-function (lambda (date) 'org-agenda-date)) ; remove underline on todays date
                   ;; (org-agenda-format-date "%A %-e %B %Y") ;modify date
                   ;; (org-agenda-fontify-priorities nil)
                   (org-agenda-start-day nil)
                   (org-agenda-overriding-header "\nDaily agenda view\n")))
          ))
        ("P" "Protesilaos"
         ,maluware-custom-org-daily-agenda)
        ))

;; Function to be run when org-agenda is opened
(defun org-agenda-open-hook()
  "Hook to be run when org-agenda is opened"
  (olivetti-mode))

;; Adds hooks to org agenda mode, making follow mode active in org agenda
(add-hook 'org-agenda-mode-hook 'org-agenda-open-hook)

(after! org
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (jupyter . t))))

(setq magit-view-git-manual-method 'woman)

(map! :leader :desc "Dashboard" "d" #'+doom-dashboard/open)

(setq eros-eval-result-prefix "⟹ ") ; default =>
