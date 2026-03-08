;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; (package! obsidian)
(package! tldr)
(package! all-the-icons) ;TODO add desc
(package! elcord)
;; (package! wttrin)

;;; ORG
(package! olivetti)
(package! org-super-agenda)
(package! jupyter)
(package! org-auto-tangle)
(package! drag-stuff)
(package! org-fragtog)

;;; LSP
(package! elpy)
(package! lsp-pyright)
(package! blacken)
(package! ein)
(package! direnv)
(package! tmr)
(package! exec-path-from-shell)

(package! prettier-js)
;; (package! py-autopep8)
(package! qml-ts-mode
  :recipe (:host github :repo "xhcoding/qml-ts-mode" :files ("*.el")))
(package! astro-ts-mode)
(package! lsp-tailwindcss :recipe (:host github :repo "merrickluo/lsp-tailwindcss"))
;; (package! spotify)
;; (package! emms-player-spotify)
