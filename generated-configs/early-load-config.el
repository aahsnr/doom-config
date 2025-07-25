;;; -*- lexical-binding: t; -*-

(setq user-full-name "Ahsanur Rahman"
      user-mail-address "ahsanur041@proton.me")

(defalias 'yes-or-no-p 'y-or-n-p)

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

;; (setq catppuccin-flavor 'macchiato
;;       catppuccin-highlight-matches t
;;       catppuccin-italic-comments t
;;       catppuccin-italic-variables t
;;       catppuccin-italic-functions t
;;       catppuccin-italic-keywords t)

;; ;; Load the catppuccin theme.
;; (setq doom-theme 'catppuccin)

;; (custom-set-faces
;;  '(solaire-default-face ((t (:background "#232634"))))) ; Macchiato Crust

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14.5 :weight 'medium)
      doom-variable-pitch-font (font-spec :family "JetBrainsMono Nerd Font" :size 14.5)
      doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 26.0))

(setq doom-unicode-font (font-spec :family "JetBrainsMono Nerd Font" :size 14.5))

(add-hook! 'doom-after-init-hook
  (defun +my/setup-font-faces ()
    (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
    (set-face-attribute 'font-lock-keyword-face nil :slant 'italic)))

(setq-default line-spacing 0.00)

(add-hook! '(org-mode-hook term-mode-hook shell-mode-hook eshell-mode-hook)
           #'(lambda () (display-line-numbers-mode -1)))

(setq +doom-dashboard-banner-padding '(0 . 2))
(setq +doom-dashboard-banner-file "~/.config/doom/banner.png")

(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . " \\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . " \\1"))))

(after! doom-modeline
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-display-buffer-encoding nil
        doom-modeline-display-minor-modes nil))

(setq evil-split-window-below t
      evil-vsplit-window-right t
      evil-want-fine-undo t)

(setq evil-normal-state-cursor `(box "#bb9af7")
      evil-insert-state-cursor `(bar "#f7768e")
      evil-visual-state-cursor `(hollow "#bb9af7"))

(after! evil-escape
  (setq evil-escape-key-sequence "jk"
        evil-escape-delay 0.2
        evil-escape-excluded-modes '(dired-mode)))

(after! evil-goggles
  (setq evil-goggles-duration 0.1))

(map! :map evil-normal-state-map
      "j" #'evil-next-visual-line
      "k" #'evil-previous-visual-line)

(after! vertico
  (setq vertico-count 10
        vertico-cycle t))

(after! consult
  ;; Use fd and rg for faster searching, from vanilla config
  (setq consult-find-args "fd --hidden --strip-cwd --type f --color=never"
        consult-ripgrep-args "rg --null --line-buffered --color=never --smart-case --no-heading --line-number --hidden --glob '!.git/'"))

(use-package! consult-yasnippet
  :after (consult yasnippet)
  :config
  (setq consult-yasnippet-category-icon-alist
        '((t . "»")
          ("Emacs Lisp" . "λ")
          ("Text" . "¶")
          ("Org" . "★")
          ("Python" . "🐍"))))

(map! :leader
      :desc "Search snippets" "s y" #'consult-yasnippet)

;; Kill buffer on emacs exit
(setq vterm-kill-buffer-on-exit t)

;; Prevent kill on closing emacs frame
(defun +my/vterm-prevent-kill-on-frame-close-query ()
  "Prevent killing vterm buffers when closing a frame in daemon mode."
  (if (and (daemonp) (eq major-mode 'vterm-mode))
      ;; If in daemon mode and it's a vterm buffer, prevent the kill query
      ;; and keep the buffer alive in the daemon.
      nil
    ;; Otherwise, allow default kill query behavior.
    t))

;; Add the function to the `kill-buffer-query-functions` hook.
(add-hook 'kill-buffer-query-functions #'+my/vterm-prevent-kill-on-frame-close-query)

;; Force kill vterm buffer
(defun +my/vterm-force-kill-current-buffer ()
  (interactive)
  (when (eq major-mode 'vterm-mode)
    (kill-buffer (current-buffer) t)
    (message "Vterm buffer killed forcefully.")))

(map! :leader
      :desc "Toggle vterm locally"  "v t" #'+vterm/toggle
      :desc "Open vterm buffer locally" "v T" #'+vterm/here
      :desc "Force kill current vterm buffer" "o k" #'+my/vterm-force-kill-current-buffer)

(after! marginalia
  (setq marginalia-censor-variables nil)

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
      ;; 1 - log(3 + 1/(days + 1)) % grey
      (propertize (marginalia--time time) 'face (list :foreground color))))

  (defun +marginalia-file-size-colorful (size)
    (let* ((size-index (/ (log10 (+ 1 size)) 7.0))
           (color (if (< size-index 10000000) ; 10m
                      (doom-blend 'orange 'green size-index)
                    (doom-blend 'red 'orange (- size-index 1)))))
      (propertize (file-size-human-readable size) 'face (list :foreground color)))))

(use-package! info-colors
  :after info
  :commands (info-colors-fontify-node)
  :hook (Info-selection . info-colors-fontify-node))

(use-package! jinx
  :defer t
  :hook ((text-mode . jinx-mode)
         ;;(prog-mode . jinx-mode) ; Also useful for checking comments/strings in code
         (org-mode . jinx-mode)
         (markdown-mode . jinx-mode)
         (git-commit-mode . jinx-mode))
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages))
  :init
  (defvar my-jinx-ignored-words
    '("DoomEmacs" "Elisp" "EmacsLisp" "use-package" "tecosaur"
      "jinx-mode" "prog-mode" "conf-mode" "WIP" "regexp" "Ahsanur"
      "Rahman" "toc" "LaTeX" "cleverparens" "parens"))
  :config
  (setq jinx-languages "en_US")
  (setq jinx-delay 0.3)

  (push `(t . (,(concat "\\<\\(" (mapconcat #'regexp-quote my-jinx-ignored-words "\\|") "\\)\\>")))
        jinx-exclude-regexps)
  (push '(org-mode
          ;; All headline levels
          org-level-1 org-level-2 org-level-3 org-level-4
          org-level-5 org-level-6 org-level-7 org-level-8
          ;; Other Org elements
          org-document-title
          org-block
          org-src-block
          org-meta-line
          org-table
          org-link) ; Ignore URLs in links
        jinx-exclude-faces)
  (after! vertico
    (when (boundp 'vertico-multiform-categories)
      (add-to-list 'vertico-multiform-categories '(jinx (vertico-grid-annotate . t))))))

(use-package! rainbow-delimiters
  :config
  (add-hook! '(prog-mode-hook org-mode-hook LaTeX-mode-hook) #'rainbow-delimiters-mode)
    ;; Integrate with `mixed-pitch-mode` to ensure delimiters render correctly
  ;; with variable-pitch fonts, especially in Org and LaTeX modes.
  (add-hook! 'mixed-pitch-mode-hook
    (lambda ()
      (when (or (derived-mode-p 'org-mode) (derived-mode-p 'latex-mode))
        (rainbow-delimiters-mode))))

  ;; Customize the delimiter colors to harmonize with the doom-tokyo-night theme.
  (custom-set-faces
   '(rainbow-delimiters-depth-1-face ((t (:foreground "#bb9af7")))) ; Purple
   '(rainbow-delimiters-depth-2-face ((t (:foreground "#7dcfff")))) ; Blue
   '(rainbow-delimiters-depth-3-face ((t (:foreground "#9ece6a")))) ; Green
   '(rainbow-delimiters-depth-4-face ((t (:foreground "#ff9e64")))) ; Orange
   '(rainbow-delimiters-depth-5-face ((t (:foreground "#f7768e")))) ; Red
   '(rainbow-delimiters-depth-6-face ((t (:foreground "#2ac3de")))) ; Cyan
   '(rainbow-delimiters-depth-7-face ((t (:foreground "#e0af68")))))) ; Ochre/Gold

   

(after! smartparens
  ;; Enable highlighting of matching delimiters
  (show-paren-mode 1)

  ;; Optional: Customize the highlight face for better visibility.
  ;; This uses a background color that complements the doom-tokyo-night theme.
  (set-face-attribute 'show-paren-match
                      nil
                      :background "#3b4261" ; A noticeable background color
                      :weight 'bold))

(after! pdf-tools
  ;; Ensure pdf-tools is initialized early.
  ;; This might not be strictly necessary as it's a module, but ensures hooks are set up.
  (add-hook! 'pdf-view-mode-hook
    (defun +my/pdf-view-mode-setup ()
      (auto-revert-mode 1)
      (pdf-view-continuous-scroll-mode 1)
      (pdf-view-midnight-mode 1)
      (pdf-view-fit-width-to-window)))

  (map! :map pdf-view-mode-map
        ;; Page Navigation
        :n "J"       #'pdf-view-next-page
        :n "K"       #'pdf-view-previous-page
        :n "gg"      #'pdf-view-first-page
        :n "G"       #'pdf-view-last-page
        :n "C-f"     #'pdf-view-scroll-down-or-next-page
        :n "C-b"     #'pdf-view-scroll-up-or-previous-page
        ;; Horizontal Scrolling
        :n "h"       #'pdf-view-scroll-left
        :n "l"       #'pdf-view-scroll-right
        ;; Zooming
        :n "+"       #'pdf-view-scale-up
        :n "-"       #'pdf-view-scale-down
        :n "zi"      #'pdf-view-scale-up      ; mnemonic: zoom in
        :n "zo"      #'pdf-view-scale-down    ; mnemonic: zoom out
        ;; Fitting Commands
        :n "="       #'pdf-view-fit-page-to-window
        :n "zw"      #'pdf-view-fit-width-to-window ; mnemonic: zoom width
        :n "zh"      #'pdf-view-fit-height-to-window; mnemonic: zoom height
        ;; History Navigation (like a web browser)
        :n "C-o"     #'pdf-history-backward
        :n "C-i"     #'pdf-history-forward
        ;; Outline (Table of Contents)
        :n "o"       #'pdf-outline
        ;; SyncTeX (for LaTeX integration)
        :n "gs"      #'pdf-sync-forward-search)) ; mnemonic: go source

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

(after! dired
  ;; Omit files like in the vanilla config
  (setq dired-omit-files "^\\.[^.]\\|^#\\|^\\.$\\|^\\.\\.$\\|\\.pyc$\\|\\.o$")
  (setq dired-listing-switches "-agho --group-directories-first"))

(after! dirvish
  ;; Set quick access directories from vanilla config
  (setq dirvish-quick-access-entries
        '(("h" "~/" "Home")
          ("d" "~/Downloads/" "Downloads")
          ("D" "~/Documents/" "Documents")
          ("p" "~/Projects/" "Projects")
          ("/" "/" "Root")))
  (setq dirvish-attributes '(nerd-icons file-time file-size collapse subtree-state vc-state)))

;; --- New additions for instant startup ---

;; For Dirvish: Open it on startup if you want it immediately visible.
;; This will open a Dired buffer in Dirvish mode.
(add-hook! 'doom-after-init-hook
  (lambda ()
    (unless (daemonp) ; Only open in normal mode, daemon clients can open it manually
      (when (display-graphic-p) ; Only if a graphical display is available
        (run-hook-with-args 'dired-initial-directory-hook "~/") ; Or your preferred initial dir
        (dirvish-mode 1))))) ; Enable dirvish-mode in the initial dired buffer

;; For Vterm: You can start a vterm buffer on startup.
(add-hook! 'doom-after-init-hook
  (lambda ()
    (unless (daemonp) ; Only open in normal mode, daemon clients can open it manually
      (when (display-graphic-p) ; Only if a graphical display is available
        (vterm))))) ; This will create a new vterm buffer

;; For Org-mode: Ensure hooks run on startup, especially if you open an Org file
;; or use org-agenda as your dashboard.
;; Your `ar/org-setup-hook` is already good for when an org file is opened.
;; If you want org-agenda to be your initial view, you can configure doom-dashboard.
(after! doom-dashboard
  (setq doom-dashboard-banner-file "~/.config/doom/banner.png") ; Keep your banner
  (setq doom-dashboard-menu-sections
        '(
          ;; Your existing sections...
          (recents "Recent Files" ?r)
          (projects "Projects" ?p)
          (bookmarks "Bookmarks" ?b)
          (agenda "Org Agenda" ?a (lambda () (org-agenda nil "o"))) ; Add Org Agenda
          (nil "Doom Emacs" nil) ; Separator
          (nil "Quick Access" nil) ; Separator
          (nil "Help" nil) ; Separator
          (nil "Quit" ?q))) ; Quit option
          
  ;; Set Org Agenda as the default dashboard view
  (setq doom-dashboard-startup-hook '(org-agenda nil "o")))


;; For PDF-tools: PDF-tools itself doesn't "start" instantly like a mode.
;; It activates when you open a PDF file. Your `after! pdf-tools` block
;; correctly sets up the `pdf-view-mode-hook`, which will run when a PDF is opened.
;; No further action is needed here for "instant start" on Emacs launch,
;; as it's a file-type specific tool.

;; --- End new additions ---


(defun ar/org-font-setup ()
  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "JetBrainsMono Nerd Font" :weight 'bold :height (cdr face) :slant 'unspecified)))

(defun ar/org-setup-hook ()
  "Modes to enable on org-mode start"
  (org-indent-mode)
  (visual-line-mode 1)
  (+org-pretty-mode)
  (ar/org-font-setup))

(after! org
  (setq org-directory "~/org"
        org-ellipsis " "
        org-startup-with-inline-images t
        org-image-actual-width 600
        org-archive-location "archive/Archive_%s::"
        org-auto-align-tags nil)

  (add-hook! org-mode #'ar/org-setup-hook))

(use-package! org-tempo
  :after org
  :config
  (setq org-src-window-setup 'split-window-below
        org-src-fontify-natively t
        org-src-tab-acts-natively t)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))

(use-package! visual-fill-column
  :hook (org-mode . visual-fill-column-mode)
  :custom
  (visual-fill-column-width 100)
  (visual-fill-column-center-text t))

(setf (alist-get 'height +org-capture-frame-parameters) 15)

(after! org
  (setq org-todo-keywords
        '((sequence "☛ TODO(t)" "⚡ NEXT(n)" "🔄 PROG(p)" "⏳ WAIT(w@/!)" "|" "✅ DONE(d!)" "❌ CANCELLED(c@)")
          (sequence "🎯 GOAL(G)" "🚀 ACTIVE(A)" "⏸ PAUSED(x)" "|" "🏆 ACHIEVED(a)" "🚫 DROPPED(X)")))
 (setq org-todo-keyword-faces
       '(("☛ TODO"      . (:foreground "#f7768e" :weight bold))
         ("⚡ NEXT"      . (:foreground "#ff9e64" :weight bold))
         ("🔄 PROG"      . (:foreground "#73daca" :weight bold))
         ("⏳ WAIT"      . (:foreground "#bb9af7" :weight bold))
         ("✅ DONE"      . (:foreground "#9ece6a" :weight bold))
         ("❌ CANCELLED" . (:foreground "#565f89" :weight bold))
         ("🎯 GOAL"      . (:foreground "#7dcfff" :weight bold))
         ("🚀 ACTIVE"    . (:foreground "#ff9e64" :weight bold))
         ("⏸ PAUSED"    . (:foreground "#565f89" :weight bold))
         ("🏆 ACHIEVED"  . (:foreground "#2ac3de" :weight bold))
         ("🚫 DROPPED"   . (:foreground "#565f89" :weight bold)))))

(after! org-modern
  (setq org-modern-star '("◉" "○" "◈" "◇" "◆" "▷")
        org-modern-hide-stars "· "
        org-modern-list '((43 . "➤") (45 . "–") (42 . "•"))
        org-modern-table-vertical 1
        org-modern-table-horizontal 0.1
        org-modern-block-name '(("src" "»" "«") ("example" "»" "«") ("quote" "❝" "❞"))
        ;; Style tags with a subtle box, inspired by Doom Emacs.
        org-modern-tag-faces
        `((:foreground "#c0caf5" :weight bold :box (:line-width (1 . -1) :color "#414868")))
        ;; Prettier checkboxes
        org-modern-checkbox '((todo . "☐") (done . "☑") (cancel . "☒") (priority . "⚑") (on . "◉") (off . "○"))))

(after! org-appear
  (setq org-appear-autoemphasis t
        org-appear-autolinks t
        org-appear-autosubmarkers t))

(use-package! org-fragtog
  :hook (org-mode . org-fragtog-mode))

(after! org-capture
  (setq org-capture-templates
        (doct `(;; Main Capture Options
                ("Task" :keys "t"
                 :icon ("nf-oct-tasklist" :set "octicon" :color "red")
                 :file "inbox.org"
                 :headline "Tasks"
                 :template ("* ☛ TODO %?"
                            "  :PROPERTIES:"
                            "  :CREATED: %U"
                            "  :END:"))
                ("Note" :keys "n"
                 :icon ("nf-fa-sticky_note" :set "faicon" :color "yellow")
                 :file "inbox.org"
                 :headline "Notes"
                 :template ("* %? :note:"
                            "  :PROPERTIES:"
                            "  :CREATED: %U"
                            "  :END:"))
                ("Journal" :keys "j"
                 :icon ("nf-fa-calendar" :set "faicon" :color "pink")
                 :file "journal.org"
                 :datetree t
                 :template ("* %U %?"))
                ("Meeting" :keys "m"
                 :icon ("nf-mdi-account_group" :set "mdicon" :color "blue")
                 :file "inbox.org"
                 :headline "Meetings"
                 :template ("* Meeting: %? :meeting:"
                            "  :PROPERTIES:"
                            "  :CREATED: %U"
                            "  :ATTENDEES:"
                            "  :END:"
                            "** Agenda"
                            "** Notes"
                            "** Action Items"))
                ;; Long-term Planning
                ("Project" :keys "p"
                 :icon ("nf-oct-repo" :set "octicon" :color "green")
                 :file "projects.org"
                 :headline "Projects"
                 :template ("* 📋 PLAN %? :project:"
                            "  :PROPERTIES:"
                            "  :CREATED: %U"
                            "  :GOAL:"
                            "  :DEADLINE:"
                            "  :END:"
                            "** Goals"
                            "** Tasks"
                            "*** ☛ TODO Define project scope"
                            "** Resources"
                            "** Notes"))
                ("Book" :keys "b"
                 :icon ("nf-mdi-book_open_page_variant" :set "mdicon" :color "orange")
                 :file "reading.org"
                 :headline "Reading List"
                 :template ("* %? :book:read:"
                            "  :PROPERTIES:"
                            "  :CREATED: %U"
                            "  :AUTHOR:"
                            "  :GENRE:"
                            "  :RATING:"
                            "  :END:"
                            "** Summary"
                            "** Key Takeaways"
                            "** Quotes"))
                ("Goal" :keys "g"
                 :icon ("nf-mdi-flag_checkered" :set "mdicon" :color "purple")
                 :file "goals.org"
                 :headline "Goals"
                 :template ("* 🎯 GOAL %? :goal:"
                            "  DEADLINE: %(org-read-date nil nil \"+1y\")"
                            "  :PROPERTIES:"
                            "  :CREATED: %U"
                            "  :END:"
                            "** Why this goal?"
                            "** Success criteria"
                            "** Action steps"
                            "*** ☛ TODO Break down into smaller tasks"))
                ;; Protocol links
                ("Protocol" :keys "P"
                 :icon ("nf-fa-link" :set "faicon" :color "blue")
                 :file "Notes.org"
                 :template ("* ☛ TODO %^{Title}"
                            "Source: %u"
                            "#+BEGIN_QUOTE"
                            "%i"
                            "#+END_QUOTE"
                            "%?"))))))

(after! org-roam
  (setq org-roam-directory (expand-file-name "roam" org-directory))
  (setq org-roam-db-location (expand-file-name ".org-roam.db" org-roam-directory))

  (setq org-roam-node-display-template
        (concat "${title:*} " (propertize "${tags:20}" 'face 'org-tag)))

  ;; Configure the backlinks buffer to open on the right, like in vanilla config
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer)))

  ;; Hook to update modification times, keeping the graph fresh
  (defun +my/org-roam-update-modified-timestamp ()
    "Update modified timestamp in org-roam files before saving."
    (when (and (eq major-mode 'org-mode) (org-roam-file-p))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^#\\+modified:" nil t)
          (delete-region (point) (line-end-position))
          (insert (format " %s" (format-time-string "[%Y-%m-%d %a %H:%M]")))))))
  (add-hook 'before-save-hook #'+my/org-roam-update-modified-timestamp)
  (setq org-roam-dailies-directory "daily/"))

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil))

(after! org-agenda
  (setq org-agenda-files (list org-directory (expand-file-name "roam" org-directory)))
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-agenda-block-separator 'hr
        org-agenda-compact-blocks t)
  (org-super-agenda-mode))

;; The powerful agenda "dashboard" from vanilla config
(setq org-agenda-custom-commands
      '(("o" "Dashboard"
         ((agenda "" ((org-deadline-warning-days 7)
                      (org-agenda-overriding-header "📅 Agenda")))
          (todo "⚡ NEXT" ((org-agenda-overriding-header "⚡ Next Tasks")))
          (tags-todo "project/🚀 ACTIVE" ((org-agenda-overriding-header "🚀 Active Projects")))
          (tags-todo "+PRIORITY=\"A\"" ((org-agenda-overriding-header "🔥 High Priority")))
          (todo "⏳ WAIT" ((org-agenda-overriding-header "⏳ Waiting On")))
          (tags-todo "+habit" ((org-agenda-overriding-header "🔄 Habits")))
          (stuck "" ((org-agenda-overriding-header "🚫 Stuck Projects")))))

        ("p" "Projects Overview"
         ((tags "project" ((org-agenda-overriding-header "📋 All Projects")))))

        ("g" "Goals Review"
         ((tags-todo "goal" ((org-agenda-overriding-header "🎯 Goals")))))))

(setq org-super-agenda-groups
      '((:name "🔥 Overdue" :deadline past)
        (:name "📅 Today" :time-grid t :scheduled today)
        (:name "⚡ Next" :todo "⚡ NEXT")
        (:name "🔴 Important" :priority "A")
        (:name "🚀 Active Projects" :tag "project" :todo "ACTIVE")
        (:name "🎯 Goals" :tag "goal")
        (:name "🔄 Habits" :tag "habit")
        (:name "⏳ Waiting" :todo "WAIT")
        (:discard (:anything t))))

(setq org-archive-location "archive/Archive_%s::")

(defun ar/org-archive-done-tasks ()
  "Attempt to archive all done tasks in file"
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'file))

(map! :map org-mode-map :desc "Archive tasks marked DONE" "C-c DEL a" #'ar/org-archive-done-tasks)

(defun ar/org-remove-kill-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-cut-subtree)
     (pop kill-ring)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/KILL" 'file))

(map! :map org-mode-map :desc "Remove tasks marked as KILL" "C-c DEL k" #'ar/org-remove-kill-tasks)

(defvar my-bib-files
  (list
   (expand-file-name "roam/bibliography.bib" org-directory) ; Main library from Zotero
   (expand-file-name "projects/my-project/project.bib" org-directory)) ; A project-specific file
  "A list of all bibliography files to be used.")

(after! citar
  (setq citar-bibliography (list my-bib-files))
  (setq citar-notes-paths (list (expand-file-name "roam/notes/" org-directory)))
  (setq citar-at-point-function 'embark-act) ; Use Embark for contextual actions

  ;; Enable embark integration for citar
  (citar-embark-mode)

  (setq citar-symbols
        `((file ,(nerd-icons-octicon "nf-oct-file" :face 'nerd-icons-red) . " ")
          (note ,(nerd-icons-octicon "nf-oct-note" :face 'nerd-icons-yellow) . " ")
          (link ,(nerd-icons-octicon "nf-oct-link" :face 'nerd-icons-blue) . " "))))

(after! org-roam-bibtex
  (setq orb-bibtex-file (car my-bib-files)) ; ORB needs one primary file for note creation
  (setq orb-roam-directory (expand-file-name "roam/notes/" org-directory))
  (setq orb-templates
        '(("r" "ref" plain (function org-roam-capture--get-point) ""
           :file-name "${citekey}"
           :head "#+TITLE: ${title}\n#+AUTHOR: ${author-or-editor}\n#+ROAM_KEY: ${ref}\n#+ROAM_TAGS: lit-note\n\n* Summary\n\n* Quotes\n\n* My Thoughts\n\n* Related\n\n- ${citekey}"
           :unnarrowed t))))

(after! org-roam
  (require 'citar-org-roam)
  (citar-org-roam-mode))

(after! tex
  (setq-default TeX-engine 'tectonic)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
  (setq TeX-source-correlate-mode t)
  (setq TeX-source-correlate-start-server t)
  (setq font-latex-fontify-sectioning 1.3) ; Scale section headers for readability

  (defun my/tex-compile-and-clean ()
    "Compile the LaTeX file with Tectonic and clean auxiliary files on success."
    (interactive)
    (let ((TeX-clean-extensions
           '(".aux" ".bbl" ".blg" ".log" ".out" ".toc" ".fls" ".fdb_latexmk" "*-blx.bib" "*.run.xml")))
      (TeX-command-master nil (lambda () (TeX-clean)))))

  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (outline-minor-mode)
              (rainbow-delimiters-mode))))

(after! lsp-mode
  ;; 1. Register texlab as a client for lsp-mode
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("texlab"))
                    :major-modes '(tex-mode latex-mode)
                    :remote? t
                    :server-id 'texlab))

  ;; 2. Configure the settings for texlab using the modern API
  (lsp-register-custom-settings
   '(("texlab.build.executable" "tectonic")
     ("texlab.build.args" ["-Z" "shell-escape" "--outdir=%OUTDIR%" "%FILE%"])
     ("texlab.forwardSearch.executable" "zathura")
     ("texlab.forwardSearch.args" ["--synctex-forward" "%LINE%:%COLUMN%" "%PDF%"]))))

(setq +zen-mixed-pitch-modes '(org-mode LaTeX-mode markdown-mode))
(dolist (hook +zen-mixed-pitch-modes)
  (add-hook (intern (concat (symbol-name hook) "-hook")) #'mixed-pitch-mode))
(add-hook 'org-mode-hook #'org-fragtog-mode)
(after! laas (add-hook 'LaTeX-mode-hook #'laas-mode))

(after! yasnippet
  (let* (;; --- Source Lists for Snippet Generation ---
         (greek-alphabet
          '(("a" . "alpha") ("b" . "beta") ("g" . "gamma") ("d" . "delta")
            ("e" . "epsilon") ("z" . "zeta") ("h" . "eta") ("th" . "theta")
            ("i" . "iota") ("k" . "kappa") ("l" . "lambda") ("m" . "mu")
            ("n" . "nu") ("x" . "xi") ("p" . "pi") ("r" . "rho")
            ("s" . "sigma") ("t" . "tau") ("u" . "upsilon") ("ph" . "phi")
            ("ch" . "chi") ("ps" . "psi") ("o" . "omega")))
         (math-symbols
          '(("!=" . "neq") (">=" . "geq") ("<=" . "leq") ("->" . "to")
            ("<-" . "leftarrow") ("=>" . "Rightarrow") ("<=" . "Leftarrow")
            ("v" . "forall") ("e" . "exists") ("!e" . "nexists")
            ("in" . "in") ("!in" . "notin") ("sub" . "subset") ("sup" . "supset")
            ("sube" . "subseteq") ("supe" . "supseteq") ("0" . "emptyset")
            ("inf" . "infty") ("d" . "partial") ("grad" . "nabla")))
         (math-environments
          '(("eq" . "equation") ("eq*" . "equation*") ("ali" . "align")
            ("ali*" . "align*") ("gat" . "gather") ("gat*" . "gather*")))
         (math-structures
          '(("f" . "\\frac{$1}{$2}$0") ("sq" . "\\sqrt{$1}$0")
            ("sqr" . "\\sqrt[$2]{$1}$0") ("hat" . "\\hat{$1}$0")
            ("bar" . "\\bar{$1}$0") ("vec" . "\\vec{$1}$0") ("til" . "\\tilde{$1}$0")
            ("dot" . "\\dot{$1}$0") ("ddot" . "\\ddot{$1}$0")))
         (section-commands
          '(("ch" . "chapter") ("sec" . "section") ("ssec" . "subsection")
            ("sssec" . "subsubsection") ("par" . "paragraph")))
         (theorem-environments
          '(("thm" . "theorem") ("lem" . "lemma") ("cor" . "corollary")
            ("prop" . "proposition") ("defn" . "definition") ("rem" . "remark"))))

    ;; --- Snippet Definition Logic ---
    (yas-define-snippets 'latex-mode
      (append
       ;; Greek letters (e.g., 'a -> \alpha, 'A -> \Alpha)
       (mapcan (lambda (g)
                 `((,(concat "'" (car g)) ,(concat "\\" (cdr g)))
                   (,(concat "'" (upcase (car g))) ,(concat "\\" (capitalize (cdr g))))))
               greek-alphabet)
       ;; Math blackboard bold (e.g., `R -> \mathbb{R})
       (mapcar (lambda (c) `(,(concat "`" c) ,(concat "\\mathbb{" c "}")))
               '("R" "C" "N" "Z" "Q" "P" "E" "F" "H" "I" "K" "L" "S" "T"))
       ;; General math symbols (e.g., ;!= -> \neq)
       (mapcar (lambda (m) `(,(concat ";" (car m)) ,(concat "\\" (cdr m)))) math-symbols)
       ;; Math environments (e.g., ,eq -> \begin{equation}...)
       (mapcar (lambda (e) `(,(concat "," (car e))
                              ,(format "\\begin{%s}\n  $0\n\\end{%s}" (cdr e) (cdr e))))
               math-environments)
       ;; Math structures (e.g., //f -> \frac{}{})
       (mapcar (lambda (s) `(,(concat "//" (car s)) ,(cdr s))) math-structures)
       ;; Sectioning commands with labels (e.g., sec -> \section{}, secl -> \section{} \label{})
       (mapcan (lambda (s)
                 (let* ((key (car s)) (cmd (cdr s)))
                   `((,key ,(format "\\%s{$1}$0" cmd))
                     (,(concat key "l") ,(format "\\%s{$1} \\label{%s:$2}\n$0" cmd key)))))
               section-commands)
       ;; Theorem-like environments (e.g., Bthm -> \begin{theorem})
       (mapcar (lambda (e) `(,(concat "B" (car e)) ,(format "\\begin{%s}\n  $0\n\\end{%s}" (cdr e) (cdr e))))
               theorem-environments)))))

(defun my/latex-find-project-packages ()
  "Find all .sty files in the project's 'styles' directory or parent directories."
  (let ((search-dirs '("./styles/" "../styles/" "./" "../")))
    (seq-uniq
     (seq-sort #'string-lessp
               (seq-filter #'identity
                           (mapcan (lambda (dir)
                                     (when (file-directory-p dir)
                                       (directory-files dir t "\\.sty$")))
                                   search-dirs))))))

(defun my/latex-insert-project-packages (format-str)
  "Insert project .sty files using FORMAT-STR."
  (let ((packages (my/latex-find-project-packages)))
    (if (not packages)
        (message "No local .sty files found.")
      (insert (string-join (mapcar (lambda (file)
                                     (format format-str
                                             (file-name-sans-extension
                                              (file-relative-name file default-directory))))
                                   packages)
                           "\n")))))

(defun my/latex-insert-packages-tex ()
  "Insert \\usepackage lines for local .sty files."
  (interactive)
  (my/latex-insert-project-packages "\\usepackage{%s}"))

(defun my/latex-insert-packages-org ()
  "Insert #+LATEX_HEADER lines for local .sty files."
  (interactive)
  (my/latex-insert-project-packages "#+LATEX_HEADER: \\usepackage{%s}"))

(after! ox-latex
  (setq org-latex-listings 'engraved)
  (setq org-latex-pdf-process '("tectonic -Z shell-escape --outdir=%o %f"))
  (setq org-latex-default-class "chameleon")
  (setq org-beamer-theme "[progressbar=foot]metropolis")

  (defvar my/org-latex-features
    '(("\\[\\[\\(?:file\\|https?\\):[^]]+?\\.\\(?:eps\\|pdf\\|png\\|jpeg\\|jpg\\)\\]\\]" . image)
      ("^[ \t]*|" . table)
      ("cref:" . cleveref)
      ("^[ \t]*#\\+begin_(?:warning|info|success|error)" . tcolorbox)
      ((lambda (info) (eq 'beamer (org-export-backend-name (plist-get info :back-end)))) . beamer)))

  (defvar my/org-latex-feature-pkgs
    '((image . "\\usepackage{graphicx}")
      (table . "\\usepackage{longtable}\n\\usepackage{booktabs}")
      (cleveref . "\\usepackage[capitalize]{cleveref}")
      (tcolorbox . "\\usepackage[most]{tcolorbox}")
      (beamer . "\\usepackage{scrextend}")))

  (defun my/org-latex-get-preamble (info)
    (mapconcat (lambda (feature) (cdr (assq feature my/org-latex-feature-pkgs)))
               (delete-dups
                (mapcan (lambda (ft)
                          (when (pcase (car ft)
                                  ((pred stringp) (save-excursion (re-search-forward (car ft) nil t)))
                                  ((pred functionp) (funcall (car ft) info)))
                            (if (listp (cdr ft)) (cdr ft) (list (cdr ft)))))
                        my/org-latex-features))
               "\n"))

  (defvar-local my/org-latex-info-buffer nil)
  (advice-add #'org-latex-make-preamble :before
              (defun my/org-latex-save-info-advice (info &rest _)
                (setq my/org-latex-info-buffer info)))

  (advice-add #'org-splice-latex-header :around
              (defun my/org-splice-latex-header-advice (orig-fn &rest args)
                (let ((header (apply orig-fn args)))
                  (if (plist-get (car (last args)) :latex-snippets) header
                    (concat header "\n%% Dynamic Preamble\n"
                            (my/org-latex-get-preamble my/org-latex-info-buffer)
                            "\n%% End Dynamic Preamble\n")))))

  (add-to-list 'org-latex-classes
               '("chameleon"
                 "\\documentclass[11pt,a4paper]{scrartcl}
[PACKAGES]
[DEFAULT-PACKAGES]
[EXTRA]
\\usepackage{fontspec}
\\usepackage{geometry}
\\usepackage[svgnames]{xcolor}
\\usepackage{hyperref}
\\setmainfont{Source Serif Pro}
\\setmonofont{JetBrains Mono}[Scale=MatchLowercase]
\\usepackage{microtype}
\\geometry{margin=1in}
\\hypersetup{colorlinks=true, linkcolor=NavyBlue, citecolor=ForestGreen, urlcolor=SteelBlue}
% Support for admonition boxes
\\newtcolorbox{warning}{colback=yellow!10,colframe=yellow!70!black,title=Warning}
\\newtcolorbox{info}{colback=blue!10,colframe=blue!70!black,title=Info}
\\newtcolorbox{success}{colback=green!10,colframe=green!70!black,title=Success}
\\newtcolorbox{error}{colback=red!10,colframe=red!70!black,title=Error}"
                 ("\\section{%s}" . "\\section*{%s}"))))

(map! :leader
      :map latex-mode-map
      :prefix ("m" . "latex")
      "c" '(:ignore t :wk "Compile")
      "cc" '(my/tex-compile-and-clean :wk "Compile & Clean")
      "cv" '(TeX-view :wk "View Output")
      "ce" '(TeX-error-overview :wk "Error Overview")
      "ck" '(TeX-clean :wk "Clean Aux Files")
      "i" '(:ignore t :wk "Insert")
      "in" '(orb-note-actions :wk "Create/Open Literature Note (ORB)")
      "ic" '(citar-insert-citation :wk "Insert Citation")
      "ip" '(my/latex-insert-packages-tex :wk "Insert Project Packages")
      "il" '(LaTeX-insert-label :wk "Insert Label")
      "ir" '(LaTeX-insert-ref :wk "Insert Reference")
      "e" '(:ignore t :wk "Environment")
      "ee" '(LaTeX-environment :wk "Insert Environment")
      "s" '(:ignore t :wk "Section")
      "ss" '(LaTeX-section :wk "Insert Section"))

(map! :leader
      :map org-roam-mode-map
      :prefix ("n" . "roam") ; 'n' for node
      "l" '(orb-note-actions :wk "Literature Note Actions (ORB)"))

(map! :leader
      :map org-mode-map
      :prefix ("m" . "org")
      "p" '(my/latex-insert-packages-org :wk "Insert Project Packages"))

(setq forge-owned-accounts '(("aahsnr")))

(after! python
  ;; Set your preferred Python interpreter. For project-specific environments,
  ;; it is highly recommended to use the `:tools direnv` module, which will
  ;; handle this automatically.
  (setq python-shell-interpreter "python3")

  ;; Set the standard indentation to 4 spaces, per PEP 8.
  (setq python-indent-offset 4)
  ;; This ensures indentation is consistent for `python-mode` and `python-ts-mode`.
  (setq-default tab-width 4
                python-indent-offset 4))

(after! apheleia
  (setf (alist-get 'python-mode apheleia-formatters) '(black))
  (setf (alist-get 'python-ts-mode apheleia-formatters) '(black)))

(after! flycheck
  ;; Set `pylint` as the default checker. While Doom often defaults to this,
  ;; being explicit guarantees the desired behavior.
  (flycheck-add-next-checker 'python-pylint 'python-flake8 :append))

  ;; You can customize pylint arguments here. For example, to load a specific
  ;; configuration file or disable certain checks globally.
  ;; (setq flycheck-pylintrc ".pylintrc")
  ;; Example: Disable common "missing docstring" warnings
  ;; (setq flycheck-pylint-args '("--disable=C0114,C0115,C0116")))


(after! dap-python
  ;; Set the debugger to `debugpy`. This is the default in the latest `dap-mode`
  ;; but is set here for clarity and to override any older defaults.
  (setq dap-python-debugger 'debugpy)

  ;; Define a debug template to easily launch the current Python file.
  ;; This will appear in the `dap-hydra` menu (SPC d r) or when running `dap-debug`.
  (dap-register-debug-template
   "Python :: Launch Current File"
   (list :type "python"
         :request "launch"
         :name "Launch File"
         :program "${file}"
         :console "integratedTerminal"
         :justMyCode t)) ; Set to nil to step into library code

  ;; If you use a different terminal emulator with vterm, you can specify it.
  (setq dap-python-terminal-kind "kitty"))


(map! :leader
      :map python-mode-map
      :prefix ("d" . "debug")
      "d" '(dap-debug :wk "Debug")
      "t" '(dap-debug-last :wk "Debug Last")
      "q" '(dap-disconnect :wk "Disconnect")
      "b" '(dap-toggle-breakpoint :wk "Toggle Breakpoint")
      "B" '(dap-condition-breakpoint :wk "Conditional Breakpoint")
      "c" '(dap-continue :wk "Continue")
      "n" '(dap-next :wk "Next")
      "i" '(dap-step-in :wk "Step In")
      "o" '(dap-step-out :wk "Step Out")
      "r" '(dap-hydra :wk "Hydra Menu"))

(map! :leader
      :map python-mode-map
      :prefix ("c" . "code")
      "f" '(+format/buffer :wk "Format Buffer")
      "r" '(lsp-rename :wk "Rename Symbol")
      "a" '(lsp-execute-code-action :wk "Code Actions")
      "d" '(lsp-find-definition :wk "Go to Definition")
      "D" '(lsp-find-declaration :wk "Go to Declaration")
      "I" '(lsp-find-implementation :wk "Go to Implementation")
      "R" '(lsp-find-references :wk "Find References")
      "h" '(lsp-describe-thing-at-point :wk "Describe at Point"))

(use-package! feature-mode
  :mode "\\.feature$")

(use-package! systemd
  :mode "\\.service$")

;; (use-package! rpm-spec-mode
;;   :mode "\\.spec\\(\\.in\\)?$")

(map! :leader
      :desc "Open like spacemacs" "SPC" #'execute-extended-command)
