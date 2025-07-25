;;; -*- lexical-binding: t; -*-

;; Theme & UI
(package! all-the-icons)
(package! catppuccin-theme)
(package! visual-fill-column)
(package! colorful-mode)
(package! nerd-icons)
(package! rainbow-delimiters)

;; Editor/Evil
(package! evil-goggles)
(package! evil-escape)
(package! pulsar)
(package! jinx)

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

;; Completion & Snippets
(package! consult-yasnippet)

;; Development & System
(package! embark-vc)
(package! feature-mode)
(package! systemd)

;; Dependencies for LSP/Org-Jupyter
(package! zmq)
(package! websocket)

;; Ignored Packages
(package! hydra :ignore t)
(package! dape :ignore t)
(package! drag-stuff :ignore t)
(package! helm-bibtex :ignore t)
(package! org-yt :ignore t)

;; Unpin Packages
(unpin! evil-collection)
