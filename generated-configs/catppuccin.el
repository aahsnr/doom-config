;;; init.el --- Summary
;;
;; This file contains the updated Doom Emacs configuration,
;; applying the Catppuccin Macchiato color palette,
;; fixing PDF highlight settings, and enhancing solaire-mode contrast.
;;
;; Author: Gemini
;;; Commentary:
;;
;; This configuration aims to provide a visually consistent and
;; functional Emacs setup with the Catppuccin Macchiato theme.
;;
;; Changes include:
;; - Updated hex codes for cursor colors and Org TODO keywords to Macchiato.
;; - Corrected PDF highlight configuration using `pdf-annot-default-annotation-properties`.
;; - Adjusted `pdf-view-midnight-colors` to use a darker background for enhanced solaire-mode contrast.
;; - Explicitly set `solaire-default-face` background for a darker overall solaire-mode contrast.
;;
;;; Code:

;; --- THEME: CATPPUCCIN (MACCHIATO) ---
;; Set the Catppuccin flavor to 'macchiato.
(setq catppuccin-flavor 'macchiato
      catppuccin-highlight-matches t
      catppuccin-italic-comments t
      catppuccin-italic-variables t
      catppuccin-italic-functions t
      catppuccin-italic-keywords t)

;; Load the catppuccin theme.
(setq doom-theme 'catppuccin)

;; --- SOLAIRE-MODE DARKER CONTRAST ---
;; To ensure a darker background for the whole solaire-mode setup
;; for "ancillary" buffers (like *Messages*, *scratch*, popups, etc.),
;; explicitly set the background of `solaire-default-face` to a darker
;; color from the Macchiato palette (Crust: #232634).
(custom-set-faces
 '(solaire-default-face ((t (:background "#232634"))))) ; Macchiato Crust

;; --- UI CUSTOMIZATIONS WITH HEX CODES (Macchiato Palette) ---

;; Custom cursor colors for Evil mode states.
;; Using Macchiato Peach for normal and visual, Red for insert.
(setq evil-normal-state-cursor `(box "#f5a97f")      ; Macchiato Peach
      evil-insert-state-cursor `(bar "#ed8796")      ; Macchiato Red
      evil-visual-state-cursor `(hollow "#f5a97f"))  ; Macchiato Peach

;; Org TODO keyword faces with Macchiato colors.
(setq org-todo-keyword-faces
      '(("☛ TODO"      . (:foreground "#ed8796" :weight bold))   ; Macchiato Red
        ("⚡ NEXT"      . (:foreground "#eed49f" :weight bold))   ; Macchiato Yellow
        ("🔄 PROG"      . (:foreground "#8bd5ca" :weight bold))   ; Macchiato Teal
        ("⏳ WAIT"      . (:foreground "#c6a0f6" :weight bold))   ; Macchiato Mauve
        ("✅ DONE"      . (:foreground "#a6da95" :weight bold))   ; Macchiato Green
        ("❌ CANCELLED" . (:foreground "#a5adce" :weight bold)) ; Macchiato Subtext0
        ("🎯 GOAL"      . (:foreground "#babbf1" :weight bold))   ; Macchiato Lavender
        ("🚀 ACTIVE"    . (:foreground "#f5a97f" :weight bold))   ; Macchiato Peach
        ("⏸ PAUSED"    . (:foreground "#838ba7" :weight bold))   ; Macchiato Overlay1
        ("🏆 ACHIEVED"  . (:foreground "#85c1dc" :weight bold))   ; Macchiato Sapphire
        ("🚫 DROPPED"   . (:foreground "#626880" :weight bold)))) ; Macchiato Surface2

;; PDF Tools colors and highlight fix.
(after! pdf-tools
  ;; Set midnight mode colors for PDF viewer.
  ;; Using Macchiato Crust for background (darker contrast)
  ;; and Macchiato Text for foreground.
  (setq pdf-view-midnight-colors (cons "#232634" "#c6d0f5")) ; Macchiato Crust, Macchiato Text

  ;; Fix for pdf-view-highlight-face:
  ;; The linter error indicates 'pdf-view-highlight-face' might not be the correct way
  ;; to customize PDF highlights. Instead, we modify `pdf-annot-default-annotation-properties`
  ;; to set the highlight color for annotations.
  ;; This ensures compatibility and correct behavior.
  (setq pdf-annot-default-annotation-properties
        (delq nil
              (cl-loop for (type props) in pdf-annot-default-annotation-properties
                       collect (cond
                                ((eq type 'highlight)
                                 `(highlight (color . "#8caaee"))) ; Macchiato Blue for highlight
                                (t `(,type ,props))))))

  ;; If `pdf-annot-default-annotation-properties` was empty or didn't have highlight,
  ;; ensure it's added.
  (unless (assoc 'highlight pdf-annot-default-annotation-properties)
    (add-to-list 'pdf-annot-default-annotation-properties '(highlight (color . "#8caaee")))))


;; Org Modern tag styling with Macchiato colors.
(after! org-modern
  (setq org-modern-tag-faces
        `((:foreground "#c6d0f5" :weight bold :box (:line-width (1 . -1) :color "#414559"))))) ; Macchiato Text, Macchiato Surface0

;; --- MARGINALIA (Unchanged, as it uses dynamic color blending) ---
(after! marginalia
  (setq marginalia-censor-variables nil)

  ;; The functions below use `doom-blend` to dynamically create colors.
  ;; This is theme-independent and should not be replaced with static hex codes.
  (defadvice! +marginalia--anotate-local-file-colorful (cand)
    "Just a more colourful version of `marginalia--anotate-local-file'."
    :override #'marginalia--annotate-local-file
    (when-let (attrs (file-attributes (substitute-in-file-name
                                       (marginalia--full-candidate cand))
                                      'integer))
      (marginalia--fields
       ((marginalia--file-owner attrs)
        :width 12 :face 'marginalia-file-owner)
       ((marginalia--file-modes attrs))
       ((+marginalia-file-size-colorful (file-attribute-size attrs))
        :width 7)
       ((+marginalia--time-colorful (file-attribute-modification-time attrs))
        :width 12))))

  (defun +marginalia--time-colorful (time)
    (let* ((seconds (float-time (time-subtract (current-time) time)))
           (color (doom-blend
                   (face-attribute 'marginalia-date :foreground nil t)
                   (face-attribute 'marginalia-documentation :foreground nil t)
                   (/ 1.0 (log (+ 3 (/ (+ 1 seconds) 345600.0)))))))
      (propertize (marginalia--time time) 'face (list :foreground color))))

  (defun +marginalia-file-size-colorful (size)
    (let* ((size-index (/ (log10 (+ 1 size)) 7.0))
           (color (if (< size-index 10000000) ; 10m
                      (doom-blend 'orange 'green size-index)
                    (doom-blend 'red 'orange (- size-index 1)))))
      (propertize (file-size-human-readable size) 'face (list :foreground color)))))

;;; catppuccin.el ends here
