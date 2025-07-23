;; In your config.el

;; Ensure show-paren-mode is enabled globally or for relevant modes.
;; You already have this in your `after! smartparens` block, which is good:
;; (show-paren-mode 1)

;; Add a hook to `org-src-mode-hook` to ensure show-paren-mode is enabled
;; when editing source blocks. This hook fires for the *temporary buffer*
;; created for editing the source block.
(add-hook 'org-src-mode-hook
          (lambda ()
            (show-paren-mode 1)))

;; You can also customize the highlight face for better visibility,
;; if you haven't already. This is generally good practice.
(custom-set-faces
 '(show-paren-match ((t (:background "#3b4261" :weight bold)))) ; Example: TokyoNight's `comment` background
 '(show-paren-mismatch ((t (:background "red" :foreground "white" :weight bold)))))
