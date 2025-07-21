
;;; Commentary:
;;; This file configures both rainbow-delimiters and smartparens for Doom Emacs.
;;; It uses direct hex color codes from the TokyoNight theme, with manual
;;; adjustments for enhanced contrast.

(use-package! rainbow-delimiters
  :config
  ;; Enable rainbow-delimiters mode for programming, Org, and LaTeX modes.
  (add-hook! '(prog-mode-hook org-mode-hook LaTeX-mode-hook) #'rainbow-delimiters-mode)

  ;; Integrate with `mixed-pitch-mode`  
  (add-hook! 'mixed-pitch-mode-hook
    (lambda ()
      (when (or (derived-mode-p 'org-mode) (derived-mode-p 'latex-mode))
        (rainbow-delimiters-mode))))

  ;; TokyoNight theme Enhanced Contrast
  (custom-set-faces
   '(rainbow-delimiters-depth-1-face ((t (:foreground "#c9aaff"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "#95e7ff"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "#b0f085")))) 
   '(rainbow-delimiters-depth-4-face ((t (:foreground "#ffc092"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "#ff9cb0"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "#58e4f0"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "#ffe08c"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "#7f88a8"))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "#6a728b"))))
   '(rainbow-delimiters-depth-10-face ((t (:foreground "#565d6e"))))))

(after! smartparens
  ;; Enable show-pair mode globally for bracket highlighting.
  (show-smartparens-global-mode 1)

  ;; TokyoNight theme Enhanced Contrast
  (custom-set-faces
   '(sp-show-pair-face ((t (:background "#4e567f" :foreground "#b4bbde" :underline nil))))
   '(sp-show-pair-match-face ((t (:background "#4e567f" :foreground "#b4bbde" :weight bold))))
   '(sp-show-pair-mismatch-face ((t (:background "#ff8a9f" :foreground "#161720" :weight bold)))))) ; Orig red: #f7768e, Orig bg: #1a1b26

