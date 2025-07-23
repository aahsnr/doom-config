;;; -*- lexical-binding: t; -*-

;; Theme & UI
(package! all-the-icons)
(package! catppuccin-theme)
(package! visual-fill-column)
(package! colorful-mode)
(package! nerd-icons)
(package! rainbow-delimiters)

;; Evil
(package! evil-goggles)
(package! evil-escape)

;; Org, Roam, LaTeX, Citation
(package! org-roam-ui)
(package! doct)
(package! evil-org)
(package! org-super-agenda)
(package! org-fragtog)
(package! citar-org-roam)
(package! mixed-pitch)
(package! laas)
(package! ox-chameleon :recipe (:host github :repo "tecosaur/ox-chameleon"))
(package! engrave-faces :recipe (:host github :repo "tecosaur/engrave-faces"))

;; Spell Checking
(package! jinx)

;; Completion & Snippets
(package! consult-yasnippet)

;; Citations & LaTeX
;; Development & System
(package! embark-vc)
(package! multi-vterm)
(package! feature-mode)
(package! systemd)

;; Dependencies for LSP/Org-Jupyter
(package! zmq)
(package! websocket)

;; Ignored Packages
(package! hydra :ignore t)
(package! dape :ignore t)
(package! helm-bibtex :ignore t)
