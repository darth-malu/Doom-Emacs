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

(use-package! python
  :config
  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt"
        python-shell-prompt-detect-failure-warning nil
        )
  (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")

  :hook
  (python-mode . (lambda ()
                   (setq-local lsp-pyright-langserver-command "basedpyright") ;; pyright or basedpyright
                   ;; (require 'lsp-pyright)
                   (setq +format-with 'black)
                   (lsp-deferred)
                   (local-set-key (kbd "C-c r") 'python-shell-send-region))))

(use-package! elpy
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))
;; :custom
;; (elpy-folding-fringe-indicators t)
;; (elpy-modules (delq 'elpy-module-highlight-indentation elpy-modules)) ; disable elpy indentation guide (ANNOYING)
;; :config

(after! elpy
  (setq elpy-modules (delq 'elpy-module-highlight-indentation elpy-modules))) ; disable elpy indentation guide (ANNOYING)

(use-package lsp-treemacs
  :after lsp)

(after! lsp-mode
  (setq lsp-enable-symbol-highlighting nil
        lsp-enable-suggest-server-download nil))

(after! lsp-clangd
  (setq lsp-clients-clangd-args
        '("-j=3"
          "--background-index"
          "--clang-tidy"
          "--completion-style=detailed"
          "--header-insertion=never"
          "--header-insertion-decorators=0"))
  (set-lsp-priority! 'clangd 2))

(use-package! lsp-nix
  ;; :ensure lsp-mode
  :after (lsp-mode)
  :demand t
  :custom
  (lsp-disabled-clients '((nix-mode . nix-nixd))) ;; TODO test if nixdd is on or need disabling
  (lsp-nix-nil-formatter ["nixfmt"]))

  (setq
    doom-symbol-font (font-spec :family "Symbols Nerd Font")
    doom-font (font-spec :family "JetBrains Mono"
                         :size (if (string-equal (system-name) "tangier") 15 15)
                         :weight (if (string-equal (system-name) "tangier") 'regular 'regular)) ;medium
    doom-emoji-font (font-spec :family "Noto Color Emoji")
    doom-variable-pitch-font (font-spec :family "VictorMono Nerd Font" :size 17))

(custom-set-faces!
  ;; '(mode-line :family "Iosevka Comfy" :size 15)
  '(mode-line :family "Mononoki Nerd Font" :box nil :overline nil)
  ;; '(doom-modeline-buffer-modified :foreground "green") ; color of modified buffer indicator
  '(mode-line-inactive :family "Iosevka Comfy"))

(use-package! emacs
  ;; :init
  :custom
  (tab-width 2)
  (tab-always-indent 'complete)
  (tab-first-completion 'word-or-paren-or-punct)
  (display-line-numbers-type nil) ;numbers, relative , - perfomance enhance...turn on if needed
  ;; (auto-save-default t)
  ;; (auto-save-timeout 10)
  ;; (auto-save-interval 200)
  ;; (undo-limit 80000000)
  (delete-by-moving-to-trash t) ; use system trash can
  ;; (x-stretch-cursor t) ; see if needed really
  (bookmark-save-flag 1) ; TODO see docs
  ;; (uniquify-buffer-name-style 'post-forward) ;nil::
  (inhibit-startup-message t)           ;Hide th startup message

  (doom-fallback-buffer-name "Doom Emacs") ; *doom*
  (+doom-dashboard-name "Doom Dashboard")

  (+evil-want-o/O-to-continue-comments nil) ; o/O does not continue comment to next new line 😸
  ;; (evil-move-cursor-back nil)               ; don't move cursor back one CHAR when exiting insert mode

  (evil-shift-width 2)

  (user-full-name "Justin Malu") ; foor GPG config, email clients, file templates & snippets ; optional
  (user-mail-address "justinmalu@gmail.com")

  ;;; using ultra scroll block now
  (scroll-margin 18) ; Adjust the number as needed
  (scroll-conservatively 101) ; TODO test usefulness

  (doom-modeline-modal nil)             ;display mode -> NORMAL,INSERT,VISUAL
  (doom-modeline-check-simple-format t)
  ;;(display-time-mode 1)                             ; Enable time in the line-mode
  ;; (display-time-format "%H:%M")
  ;;(display-time-default-load-average nil)
  ;; (display-time-day-and-date 1)

  :config
  ;; (global-set-key [escape] 'keyboard-escape-quit) ; By default, Emacs requires you to hit ESC three times to escape quit the minibuffer. ; test this further
  (global-auto-revert-mode t)
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys)
  ;; (vi-tilde-fringe-mode -1)

  :bind
  (:map evil-normal-state-map
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
        ("M-O" . +evil/insert-newline-above)))
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
  ;; :config
  ;; :custom
  :init
  (customize-set-variable 'corfu-auto nil))
  ;; (corfu-auto nil))

(after! spell-fu
  (setq spell-fu-idle-delay 0.5))  ; default is 0.25

(use-package! centaur-tabs
  :defer t
  ;; :demand ; for when you need it immediately
  ;; :init
  ;; (setq centaur-tabs-mode nil)
  ;; (centaur-tabs-change-fonts "arial" 112)
  ;; (centaur-tabs-headline-match) ; FIXME does not work causes error
  ;; (require 'projectile)
  ;; (centaur-tabs-group-by-projectile-project) ; group tabs by projectile
  :config
  (setq centaur-tabs-set-bar nil ; left, over, under
        centaur-tabs-style 'bar ;alternate, bar, box(x), wave, zigzag, chamfer FIXME...slant does not work
        centaur-tabs-icon-type 'all-the-icons ; or nerd-icons
        centaur-tabs-set-icons t
        ;; centaur-tabs-close-button "X"
        ;; centaur-tabs-modified-marker "•" - Also
        ;; centaur-tabs-set-close-button nil
        ;; centaur-tabs-plain-icons t ; for same color as text
        ;; centaur-tabs-show-navigation-buttons t
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-cycle-scope 'tabs ; default::, tabs , groups
        centaur-tabs-height 15)
  :hook (
         ;; (nix-mode  . centaur-tabs-mode)
         ;; (python-mode  . centaur-tabs-mode)
         ;; (prog-mode  . centaur-tabs-mode)
         (pdf-view-mode . centaur-tabs-local-mode)
         (org-mode . centaur-tabs-local-mode)) ; no centaur tabs on org documents
  :bind
  (:map evil-normal-state-map
        ("g t" . centaur-tabs-forward)
        ("g T" . centaur-tabs-backward)))

(use-package! org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  ;; :hook (org-src-mode . org-auto-tangle-mode)
  :config
  ;; (setq org-auto-tangle-default t) ; set auto_tangle: nil for buffers not to auto tangle
  (setq org-auto-tangle-babel-safelist '("~/system.org" "~/test.org")))

(use-package! hl-todo
  :hook (org-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces `(("TODO"       warning bold)
                                ("FIXME"      error bold)
                                ("NEVERDO"    warning bold)
                                ("HACK"       font-lock-constant-face bold)
                                ("REVIEW"     font-lock-keyword-face bold)
                                ("NOTE"       success bold)
                                ("DEPRECATED" font-lock-doc-face bold))))

(use-package! elcord
  ;; :commands elcord-mode
  :custom
  (elcord-display-elapsed nil)
  (elcord-idle-message "Sipo Kwenye Keyboard...👻")
  :config
  ;; (elcord-mode 1)
  (setq elcord--editor-name "Church of Emacs"
        elcord-use-major-mode-as-main-icon t))

(use-package! all-the-icons
  :if (display-graphic-p))

(map! :leader :desc "Dashboard" "d" #'+doom-dashboard/open)

(setq eros-eval-result-prefix "⟹ ") ; default =>

(use-package! org
  :init
  (setq org-directory "~/org" ; trailing slash important or use expand-file-name(convert file name to absolute and canonicalize/standardize it)
        ;; org-default-notes-file (concat org-directory "/notes.org")
        org-default-notes-file (expand-file-name  "notes.org" org-directory))
  :hook
  (org-mode . (lambda ()
                ;; (vi-tilde-fringe-mode -1)
                (display-line-numbers-mode -1)
                ;; (spell-fu-mode -1)
                (diff-hl-mode -1)))
  :custom
  ;; (org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕"))
  (org-log-done 'time) ; task done with timestamp
  ;; (org-log-done-with-time nil)
  ;; (org-log-done 'note) ;task done with note prompted to user
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
        ("@emacs" . ?E)
        ("@nix" . ?N)))
  (org-todo-keywords
      '((sequence "TODO" "WORKING"  "|" "DONE" "CONSIDER"))))
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
           (file+headline "EmacsTODO.org" "TONEVERDO list - emacs [/]")
           "+ [ ] %?")

          ("n" "nixOs" plain
           (file+headline "nixTODO.org" "TONEVERDO nixOs [/]")
           "+ [ ] %?")

          ("b" "Bucket List [ movies books youtube]") ; group 'em up
          ("bm" "movies" plain
           (file+headline "bucket-list.org" "Movies")
           "+ [ ] %?")
          ("bb" "books" plain
           (file+headline "bucket-list.org" "Books")
           "+ [ ] %?")
          ("by" "youtube" plain
           (file+headline "bucket-list.org" "YouTube")
           "+ [ ] %?")

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
           " %?"
           :empty-lines 1
           :prepend t)
          ("dp" "phrases [p]" plain
           (file+headline "diction.org" "Phrases")
           "+ %?"
           :empty-lines 1
           :prepend t))))

(load! "maluware-org-agenda") ; imports maluware-orgAgenda.el

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
