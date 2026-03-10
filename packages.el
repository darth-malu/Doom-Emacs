;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;;; ORG
(package! olivetti)
(package! org-super-agenda)
(package! org-auto-tangle)
(package! drag-stuff)
(package! org-fragtog)

;;; LSP / Dev
(package! jupyter)
(package! elpy)
(package! lsp-pyright)
(package! blacken)
(package! ein)
(package! direnv)
(package! tmr)
;; (package! exec-path-from-shell) ; NOTE use doom env lol
(package! prettier-js)
(package! qml-ts-mode
  :recipe (:host github :repo "xhcoding/qml-ts-mode" :files ("*.el")))
(package! astro-ts-mode)
(package! lsp-tailwindcss :recipe (:host github :repo "merrickluo/lsp-tailwindcss"))

;; (package! spotify)
;; (package! emms-player-spotify)
;; (package! obsidian)
(package! tldr)
(package! all-the-icons) ;TODO add desc
(package! elcord)
;; (package! wttrin)
