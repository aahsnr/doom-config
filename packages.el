;;; package --- Summary
;;; -*- lexical-binding: t; -*-

;; Theme & UI
(package! catppuccin-theme)
(package! mixed-pitch)
(package! visual-fill-column)
(package! info-colors)
(package! colorful-mode)
(package! rainbow-identifiers)
(package! rainbow-delimiters)

;; Evil
(package! evil-goggles)
(package! evil-escape)
(package! evil-cleverparens
  :recipe (:host github :repo "tomdl89/evil-cleverparens" :branch "fix/delete-escaped-parens"))
(package! paredit)

;; Org & Roam
(package! org-roam-ui)
(package! doct)
(package! evil-org)
(package! org-super-agenda)
(package! org-fragtog) ; For toggling LaTeX fragment previews

;; Citations & LaTeX
(package! citar-org-roam)
(package! org-roam-bibtex)
(package! consult-bibtex
  :recipe (:host github :repo "mohkale/consult-bibtex"))

;; Development & System
(package! embark-vc)
(package! multi-vterm)
(package! feature-mode)
(package! systemd)
(package! eglot-booster
  :recipe (:host github :repo "jdtsmith/eglot-booster"))

;; Dependencies for LSP/Org-Jupyter
(package! zmq)
(package! websocket)
(package! simple-httpd)
