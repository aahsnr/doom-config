;; --- THEME: CATPPUCCIN (MOCHA) ---
(setq catppuccin-flavor 'mocha)
(setq doom-theme 'catppuccin)

;; --- UI CUSTOMIZATIONS WITH HEX CODES ---

;; Custom cursor colors
(setq evil-normal-state-cursor `(box "#fab387")      ; Peach
      evil-insert-state-cursor `(bar "#f38ba8")      ; Red
      evil-visual-state-cursor `(hollow "#fab387"))  ; Peach

;; Org TODO keyword faces
(setq org-todo-keyword-faces
      '(("☛ TODO"      . (:foreground "#f38ba8" :weight bold))   ; Red
        ("⚡ NEXT"      . (:foreground "#f9e2af" :weight bold))   ; Yellow
        ("🔄 PROG"      . (:foreground "#94e2d5" :weight bold))   ; Teal
        ("⏳ WAIT"      . (:foreground "#cba6f7" :weight bold))   ; Mauve
        ("✅ DONE"      . (:foreground "#a6e3a1" :weight bold))   ; Green
        ("❌ CANCELLED" . (:foreground "#a6adc8" :weight bold))   ; Subtext0
        ("🎯 GOAL"      . (:foreground "#b4befe" :weight bold))   ; Lavender
        ("🚀 ACTIVE"    . (:foreground "#fab387" :weight bold))   ; Peach
        ("⏸ PAUSED"    . (:foreground "#7f849c" :weight bold))   ; Overlay1
        ("🏆 ACHIEVED"  . (:foreground "#74c7ec" :weight bold))   ; Sapphire
        ("🚫 DROPPED"   . (:foreground "#585b70" :weight bold)))) ; Surface2

;; PDF Tools colors
(after! pdf-tools
  (setq pdf-view-midnight-colors (cons "#1e1e2e" "#cdd6f4")) ; Base, Text
  (set-face-attribute 'pdf-view-highlight-face nil :background "#89b4fa")) ; Sky

;; Org Modern tag styling
(after! org-modern
  (setq org-modern-tag-faces
        `((:foreground "#cdd6f4" :weight bold :box (:line-width (1 . -1) :color "#313244"))))) ; Text, Surface0

;; --- MARGINALIA (This part is unchanged) ---
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

