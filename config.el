;;; -*- lexical-binding: t; -*-

(setq user-full-name "Ahsanur Rahman"
      user-mail-address "ahsanur041@proton.me")

;; Prefer y-or-n over yes-or-no
(defalias 'yes-or-no-p 'y-or-n-p)

(setq doom-theme 'catppuccin)

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14.5 :weight 'medium)
      doom-variable-pitch-font (font-spec :family "JetBrainsMono Nerd Font" :size 14.5)
      doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 26.0))
(setq doom-unicode-font (font-spec :family "JetBrainsMono Nerd Font" :size 14.5))

(add-hook! 'doom-after-init-hook
  (defun +my/setup-font-faces ()
    "Configure font faces after Doom initialization."
    (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
    (set-face-attribute 'font-lock-keyword-face nil :slant 'italic)))

;; Line Spacing
(setq-default line-spacing 0.002)

;; Line Numbers
(add-hook! '(org-mode-hook term-mode-hook shell-mode-hook eshell-mode-hook)
           #'(lambda () (display-line-numbers-mode -1)))

(setq +doom-dashboard-banner-padding '(0 . 2))
(setq +doom-dashboard-banner-file "~/.config/doom/banner.png")
;;(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
;; (remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)

(after! which-key
  (setq which-key-popup-type 'minibuffer))

(after! doom-modeline
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-display-buffer-encoding nil
        doom-modeline-display-buffer-encoding nil))

(use-package! rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(setq evil-split-window-below t
      evil-vsplit-window-right t
      evil-want-fine-undo t)

(setq evil-normal-state-cursor '(box "#fab387")  ;; Catppuccin Mocha Peach
      evil-insert-state-cursor '(bar "#f38ba8")  ;; Catppuccin Mocha Red
      evil-visual-state-cursor '(hollow "#fab387")) ;; Catppuccin Mocha Peach

(use-package! evil-escape
  :hook (evil-mode . evil-escape-mode)
  :custom
  (evil-escape-key-sequence "jk")
  (evil-escape-delay 0.2)
  (evil-escape-excluded-modes '(dired-mode)))

(use-package! evil-goggles
  :hook (evil-mode . evil-goggles-mode)
  :custom (evil-goggles-duration 0.1))

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
    (set-face-attribute (car face) nil :font "JetBrainsMono Nerd Font" :weight 'bold :height (cdr face) :slant 'unspecified))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-tag nil :foreground nil :inherit '(shadow fixed-pitch) :weight 'bold)
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(setq org-directory "~/org"
      org-ellipsis " "
      org-startup-with-inline-images t
      org-image-actual-width 600
      org-archive-location "archive/Archive_%s::"
      org-auto-align-tags nil) ; org-modern handles this better

(defun ar/org-setup-hook ()
  "Modes to enable on org-mode start"
  (org-indent-mode)
  (visual-line-mode)
  (+org-pretty-mode)
  (ar/org-font-setup))

(add-hook! org-mode #'ar/org-setup-hook)

;; (after! evil-org
;;   (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h))

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
        '(("☛ TODO" . (:foreground "#f38ba8" :weight bold))
          ("⚡ NEXT" . (:foreground "#fab387" :weight bold))
          ("🔄 PROG" . (:foreground "#89b4fa" :weight bold))
          ("⏳ WAIT" . (:foreground "#f9e2af" :weight bold))
          ("✅ DONE" . (:foreground "#a6e3a1" :weight bold))
          ("❌ CANCELLED" . (:foreground "#cdd6f4" :weight bold))
          ("🎯 GOAL" . (:foreground "#b4befe" :weight bold))
          ("🚀 ACTIVE" . (:foreground "#74c7ce" :weight bold))
          ("⏸ PAUSED" . (:foreground "#9399b2" :weight bold))
          ("🏆 ACHIEVED" . (:foreground "#a6e3a1" :weight bold))
          ("🚫 DROPPED" . (:foreground "#585b70" :weight bold)))))

;; Visual enhancements for Org
(after! org-modern
  (setq org-modern-star '("◉" "○" "◈" "◇" "◆" "▷")
        org-modern-hide-stars "· "
        org-modern-list '((43 . "➤") (45 . "–") (42 . "•"))
        org-modern-table-vertical 1
        org-modern-table-horizontal 0.1
        org-modern-block-name '(("src" "»" "«") ("example" "»" "«") ("quote" "❝" "❞"))
        ;; Style tags with a subtle box, inspired by Doom Emacs.
        org-modern-tag-faces `((:foreground ,(face-attribute 'default :foreground) :weight bold :box (:line-width (1 . -1) :color "#45475a")))))

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
        org-roam-ui-open-on-start nil)) ; Set to `t` to open UI on startup

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

;; Disable flyspell spell checking for org headlines
(after! org
  (defun +my/org-mode-flyspell-verify ()
    "Custom org-mode flyspell verification function.
    Calls the original org-mode-flyspell-verify but additionally
    skips spell checking on org headlines."
    (and (org-mode-flyspell-verify)  ; Call original function first
         (not (org-at-heading-p))))   ; Skip if we're on a headline

  ;; Override the flyspell mode predicate for org-mode
  (put 'org-mode 'flyspell-mode-predicate '+my/org-mode-flyspell-verify))

(setq +zen-mixed-pitch-modes '(org-mode LaTeX-mode markdown-mode gfm-mode Info-mode rst-mode adoc-mode))

(dolist (hook +zen-mixed-pitch-modes)
  (add-hook (intern (concat (symbol-name hook) "-hook")) #'mixed-pitch-mode))

(use-package! org-super-agenda
  :commands org-super-agenda-mode)

(after! org-agenda
  (let ((inhibit-message t))
    (org-super-agenda-mode)))

(setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-block-separator nil
      org-agenda-tags-column 100 ;; from testing this seems to be a good value
      org-agenda-compact-blocks t)

(setq org-agenda-custom-commands
      '(("o" "Overview"
         ((agenda "" ((org-agenda-span 'day)
                      (org-super-agenda-groups
                       '((:name "Today"
                          :time-grid t
                          :date today
                          :todo "TODAY"
                          :scheduled today
                          :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '((:name "Next to do"
                           :todo "NEXT"
                           :order 1)
                          (:name "Important"
                           :tag "Important"
                           :priority "A"
                           :order 6)
                          (:name "Due Today"
                           :deadline today
                           :order 2)
                          (:name "Due Soon"
                           :deadline future
                           :order 8)
                          (:name "Overdue"
                           :deadline past
                           :face error
                           :order 7)
                          (:name "Assignments"
                           :tag "Assignment"
                           :order 10)
                          (:name "Issues"
                           :tag "Issue"
                           :order 12)
                          (:name "Emacs"
                           :tag "Emacs"
                           :order 13)
                          (:name "Projects"
                           :tag "Project"
                           :order 14)
                          (:name "Research"
                           :tag "Research"
                           :order 15)
                          (:name "To read"
                           :tag "Read"
                           :order 30)
                          (:name "Waiting"
                           :todo "WAITING"
                           :order 20)
                          (:name "University"
                           :tag "uni"
                           :order 32)
                          (:name "Trivial"
                           :priority<= "E"
                           :tag ("Trivial" "Unimportant")
                           :todo ("SOMEDAY" )
                           :order 90)
                          (:discard (:tag ("Chore" "Routine" "Daily")))))))))))

(setq forge-owned-accounts '(("aahsnr")))

(use-package! feature-mode
  :mode "\\.feature$")

(use-package! systemd
  :mode "\\.service$")

(with-eval-after-load 'ob-jupyter
  (org-babel-jupyter-aliases-from-kernelspecs))

(map! :leader
      :desc "Open like spacemacs" "SPC" #'execute-extended-command)

(after! smartparens
  ;; Enable show-pair mode for bracket highlighting
  (show-smartparens-global-mode 1)

  ;; Explicitly define smartparens highlighting faces for Catppuccin Mocha
  (custom-set-faces
   '(sp-show-pair-face ((t (:background "#6e6a86" :foreground "#cdd6f4" :underline nil))))
   '(sp-show-pair-match-face ((t (:background "#6e6a86" :foreground "#cdd6f4" :weight bold))))
   '(sp-show-pair-mismatch-face ((t (:background "#f38ba8" :foreground "#1e1e2e" :weight bold))))))
