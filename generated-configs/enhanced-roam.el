;; Enhanced Org-Roam Configuration for Doom Emacs
;; Add this to your config.el file

(use-package! org-roam
  :init
  ;; Essential directory setup
  (setq org-roam-directory "~/org-roam/")
  (setq org-roam-db-location (concat org-roam-directory ".org-roam.db"))
  
  ;; Performance optimizations
  (setq org-roam-db-gc-threshold gc-cons-threshold)
  
  :config
  ;; Enable org-roam-db-autosync-mode for automatic database updates
  ;; Initialize after org-roam is loaded to prevent startup issues
  (org-roam-db-autosync-enable)
  
  ;; Node display configuration
  (setq org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:10}" 'face 'org-tag)))
  
  ;; Enhanced capture templates with multiple types
  (setq org-roam-capture-templates
        '(("d" "default" plain
           "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                             "#+title: ${title}\n#+created: %U\n#+last_modified: %U\n\n")
           :unnarrowed t)
          
          ("l" "literature" plain
           "* Source\n\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Summary\n\n%?\n\n* Key Points\n\n* Personal Thoughts\n\n"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                             "#+title: ${title}\n#+filetags: :literature:\n#+created: %U\n#+last_modified: %U\n\n")
           :unnarrowed t)
          
          ("p" "project" plain
           "* Project Overview\n\n%?\n\n* Goals\n\n* Tasks\n\n* Resources\n\n* Notes\n\n"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                             "#+title: ${title}\n#+filetags: :project:\n#+created: %U\n#+last_modified: %U\n\n")
           :unnarrowed t)
          
          ("m" "meeting" plain
           "* Attendees\n\n%^{Attendees}\n\n* Agenda\n\n%?\n\n* Action Items\n\n* Follow-up\n\n"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                             "#+title: ${title}\n#+filetags: :meeting:\n#+created: %U\n#+last_modified: %U\n\n")
           :unnarrowed t)
          
          ("c" "concept" plain
           "* Definition\n\n%?\n\n* Examples\n\n* Related Concepts\n\n* References\n\n"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                             "#+title: ${title}\n#+filetags: :concept:\n#+created: %U\n#+last_modified: %U\n\n")
           :unnarrowed t)))
  
  ;; Daily notes configuration
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %<%H:%M> %?"
           :target (file+head "%<%Y-%m-%d>.org"
                             "#+title: %<%Y-%m-%d>\n#+filetags: :daily:\n\n"))
          
          ("j" "journal" entry
           "* %<%H:%M> Journal :journal:\n%?"
           :target (file+head "%<%Y-%m-%d>.org"
                             "#+title: %<%Y-%m-%d>\n#+filetags: :daily:\n\n"))
          
          ("m" "meeting" entry
           "* %<%H:%M> Meeting: %^{Meeting Title} :meeting:\n%?"
           :target (file+head "%<%Y-%m-%d>.org"
                             "#+title: %<%Y-%m-%d>\n#+filetags: :daily:\n\n"))
          
          ("t" "task" entry
           "* TODO %^{Task} :task:\nSCHEDULED: %t\n%?"
           :target (file+head "%<%Y-%m-%d>.org"
                             "#+title: %<%Y-%m-%d>\n#+filetags: :daily:\n\n"))))
  
  ;; Completion system configuration
  (setq org-roam-completion-everywhere t)
  
  ;; Graph configuration with better defaults
  (setq org-roam-graph-executable "dot")
  (setq org-roam-graph-extra-config 
        '(("overlap" . "false")
          ("splines" . "true")
          ("rankdir" . "LR")
          ("bgcolor" . "transparent")))
  
  ;; Auto-update file modification time with error handling
  (add-hook 'org-roam-find-file-hook
            (lambda ()
              (when (and (org-roam-file-p) (buffer-file-name))
                (add-hook 'before-save-hook 'org-roam-update-last-modified nil t)))))

;; Function to update last modified timestamp with error handling
(defun org-roam-update-last-modified ()
  "Update the #+last_modified timestamp in the current buffer."
  (when (and (buffer-file-name)
             (org-roam-file-p))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^#\\+last_modified:" nil t)
        (let ((inhibit-read-only t))
          (delete-region (line-beginning-position) (line-end-position))
          (insert (format "#+last_modified: %s"
                         (format-time-string "[%Y-%m-%d %a %H:%M]"))))))))

;; Enhanced node filtering functions
(defun org-roam-node-find-by-tag (tag)
  "Find org-roam nodes filtered by TAG."
  (interactive "sTag: ")
  (org-roam-node-find
   nil nil
   (lambda (node)
     (member tag (org-roam-node-tags node)))))

(defun org-roam-literature-notes ()
  "Browse literature notes."
  (interactive)
  (org-roam-node-find-by-tag "literature"))

(defun org-roam-project-notes ()
  "Browse project notes."
  (interactive)
  (org-roam-node-find-by-tag "project"))

(defun org-roam-meeting-notes ()
  "Browse meeting notes."
  (interactive)
  (org-roam-node-find-by-tag "meeting"))

;; Org-roam UI setup (optional - only if using org-roam-ui)
(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil)) ; Changed to nil to prevent auto-opening

;; Integration with org-agenda
(after! org-agenda
  (setq org-agenda-files (append org-agenda-files (list org-roam-directory))))

;; Additional helper functions
(defun org-roam-backlinks-count (node)
  "Return the number of backlinks for NODE."
  (length (org-roam-backlinks-get node)))

(defun org-roam-orphaned-nodes ()
  "Find nodes without any backlinks."
  (interactive)
  (let ((nodes (org-roam-node-list)))
    (seq-filter (lambda (node)
                  (= 0 (org-roam-backlinks-count node)))
                nodes)))

;; Custom agenda view for org-roam
(defun org-roam-agenda-custom ()
  "Custom agenda view for org-roam files."
  (interactive)
  (let ((org-agenda-files (list org-roam-directory)))
    (org-agenda nil "a")))

;; Performance optimization for large databases
(setq org-roam-db-node-include-function
      (lambda ()
        (not (member "archive" (org-get-tags)))))

;; Database maintenance function
(defun org-roam-db-rebuild ()
  "Rebuild the org-roam database."
  (interactive)
  (org-roam-db-clear-all)
  (org-roam-db-sync))

;; Safe database initialization
(defun org-roam-ensure-db ()
  "Ensure org-roam database is properly initialized."
  (unless (file-exists-p org-roam-db-location)
    (org-roam-db-sync)))

;; Initialize database safely
(add-hook 'org-roam-mode-hook #'org-roam-ensure-db)

;; Doom Emacs keybinding configuration using map! macro
(map! :leader
      :prefix "n"
      :desc "Find node" "f" #'org-roam-node-find
      :desc "Insert node" "i" #'org-roam-node-insert
      :desc "Capture" "c" #'org-roam-capture
      :desc "Random node" "r" #'org-roam-node-random
      :desc "Show graph" "g" #'org-roam-graph
      :desc "Literature notes" "l" #'org-roam-literature-notes
      :desc "Project notes" "p" #'org-roam-project-notes
      :desc "Meeting notes" "m" #'org-roam-meeting-notes
      :desc "Custom agenda" "a" #'org-roam-agenda-custom
      :desc "Rebuild database" "R" #'org-roam-db-rebuild)

;; Daily notes keybindings
(map! :leader
      :prefix "n d"
      :desc "Today" "t" #'org-roam-dailies-goto-today
      :desc "Yesterday" "y" #'org-roam-dailies-goto-yesterday
      :desc "Tomorrow" "T" #'org-roam-dailies-goto-tomorrow
      :desc "Capture today" "c" #'org-roam-dailies-capture-today
      :desc "Find date" "d" #'org-roam-dailies-goto-date
      :desc "Capture date" "C" #'org-roam-dailies-capture-date)

;; Alternative keybindings that work with vanilla Emacs style
(after! org-roam
  (define-key org-roam-mode-map (kbd "C-c n f") #'org-roam-node-find)
  (define-key org-roam-mode-map (kbd "C-c n i") #'org-roam-node-insert)
  (define-key org-roam-mode-map (kbd "C-c n c") #'org-roam-capture)
  (define-key org-roam-mode-map (kbd "C-c n g") #'org-roam-graph))

;; Optional: Enable org-roam-ui mode toggle
(when (modulep! :lang org +roam2)
  (map! :leader
        :prefix "n"
        :desc "Toggle UI" "u" #'org-roam-ui-mode))

;; Export enhancement
(after! ox
  (setq org-export-with-broken-links 'mark))

;; Ensure org-roam directory exists
(unless (file-directory-p org-roam-directory)
  (make-directory org-roam-directory t))

;; Hook to ensure database is synced after major operations
(add-hook 'kill-emacs-hook #'org-roam-db-sync)
