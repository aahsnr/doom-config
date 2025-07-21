;;; generated-configs/tokyonight.el -*- lexical-binding: t; -*-

;;; init.el --- Summary
;;
;; This file contains the updated Doom Emacs configuration,
;; applying the Doom TokyoNight color palette,
;; fixing PDF highlight settings, and ensuring consistent UI colors.
;;
;; Author: Gemini
;;; Commentary:
;;
;; This configuration aims to provide a visually consistent and
;; functional Emacs setup with the Doom TokyoNight theme.
;;
;; Changes include:
;; - Switched theme to 'doom-tokyo-night'.
;; - Set a slightly lighter custom base background color.
;; - Removed explicit `solaire-default-face` customization to use theme's default.
;; - Updated hex codes for cursor colors and Org TODO keywords to TokyoNight.
;; - Corrected PDF highlight configuration using `pdf-annot-default-annotation-properties`.
;; - Adjusted `pdf-view-midnight-colors` to match a dark TokyoNight background.
;;
;;; Code:

;; --- THEME: DOOM TOKYONIGHT ---
(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  ;; Ensure treemacs theme is compatible with doom-tokyo-night.
  (doom-themes-treemacs-theme "doom-tokyo-night")
  :config
  ;; Load the doom-tokyo-night theme.
  (load-theme 'doom-tokyo-night t)

  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; --- CUSTOM BASE BACKGROUND COLOR ---
;; Set the default background face to a shade lighter than TokyoNight 'Night' (#1a1b26)
;; but different from 'Storm' (#24283b).
;; Using #1c1d29 for a subtle lighter effect.
(custom-set-faces
 '(default ((t (:background "#1c1d29"))))) ; A shade lighter than TokyoNight Night

;; --- SOLAIRE-MODE (Using Doom's default integration for TokyoNight) ---
;; The `doom-tokyo-night` theme is designed to integrate with `solaire-mode`
;; by default. Explicitly setting `solaire-default-face` is not needed
;; and could interfere with the theme's intended behavior.
;; The theme will handle the contrast for ancillary buffers.

;; --- UI CUSTOMIZATIONS WITH HEX CODES (TokyoNight Palette) ---

;; Custom cursor colors for Evil mode states.
;; Using TokyoNight Magenta for normal/visual, Red for insert.
(setq evil-normal-state-cursor `(box "#bb9af7")      ; TokyoNight Magenta
      evil-insert-state-cursor `(bar "#f7768e")      ; TokyoNight Red
      evil-visual-state-cursor `(hollow "#bb9af7"))  ; TokyoNight Magenta

;; Org TODO keyword faces with TokyoNight colors.
(setq org-todo-keyword-faces
      '(("☛ TODO"      . (:foreground "#f7768e" :weight bold))   ; TokyoNight Red
        ("⚡ NEXT"      . (:foreground "#ff9e64" :weight bold))   ; TokyoNight Orange
        ("🔄 PROG"      . (:foreground "#73daca" :weight bold))   ; TokyoNight Teal
        ("⏳ WAIT"      . (:foreground "#bb9af7" :weight bold))   ; TokyoNight Magenta
        ("✅ DONE"      . (:foreground "#9ece6a" :weight bold))   ; TokyoNight Green
        ("❌ CANCELLED" . (:foreground "#565f89" :weight bold)) ; TokyoNight Comments (darker for cancelled)
        ("🎯 GOAL"      . (:foreground "#7dcfff" :weight bold))   ; TokyoNight Blue/Cyan
        ("🚀 ACTIVE"    . (:foreground "#ff9e64" :weight bold))   ; TokyoNight Orange
        ("⏸ PAUSED"    . (:foreground "#565f89" :weight bold))   ; TokyoNight Comments (darker for paused)
        ("🏆 ACHIEVED"  . (:foreground "#2ac3de" :weight bold))   ; TokyoNight Cyan
        ("🚫 DROPPED"   . (:foreground "#565f89" :weight bold)))) ; TokyoNight Comments (darker for dropped)

;; PDF Tools colors and highlight fix.
(after! pdf-tools
  ;; Set midnight mode colors for PDF viewer.
  ;; Using TokyoNight Night Editor Background for a dark contrast,
  ;; and TokyoNight Editor Foreground for text.
  (setq pdf-view-midnight-colors (cons "#1a1b26" "#c0caf5")) ; TokyoNight Night Editor Background, Editor Foreground

  ;; Fix for pdf-view-highlight-face:
  ;; Modify `pdf-annot-default-annotation-properties` to set the highlight color.
  (setq pdf-annot-default-annotation-properties
        (delq nil
              (cl-loop for (type props) in pdf-annot-default-annotation-properties
                       collect (cond
                                ((eq type 'highlight)
                                 `(highlight (color . "#7aa2f7"))) ; TokyoNight Blue for highlight
                                (t `(,type ,props))))))

  ;; Ensure highlight property is present if it was not initially.
  (unless (assoc 'highlight pdf-annot-default-annotation-properties)
    (add-to-list 'pdf-annot-default-annotation-properties '(highlight (color . "#7aa2f7")))))


;; Org Modern tag styling with TokyoNight colors.
(after! org-modern
  (setq org-modern-tag-faces
        `((:foreground "#c0caf5" :weight bold :box (:line-width (1 . -1) :color "#414868"))))) ; TokyoNight Editor Foreground, Terminal Black (darker box)

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

;;; tokyonight.el ends here
