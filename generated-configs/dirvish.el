(after! dired
  ;; Omit files like in the vanilla config
  (setq dired-omit-files "^\\.[^.]\\|^#\\|^\\.$\\|^\\.\\.$\\|\\.pyc$\\|\\.o$")
  (setq dired-listing-switches "-agho --group-directories-first"))

(after! dirvish
  ;; --- Yazi File Manager Inspired Setup ---

  ;; 1. Customize attributes for a cleaner, yazi-like column view.
  ;; We display icons, file size, modification time, and git status.
  (setq dirvish-attributes '(nerd-icons file-size file-time vc-state))

  ;; 2. Configure a custom header line for more information at a glance.
  ;; Format: <File Path> <Permissions> [<Total Files>]
  (setq dirvish-header-line-format " %p %m [%N] ")

  ;; 3. Enable automatic file previews on the right, mimicking yazi's layout.
  ;; This automatically opens a preview pane for the selected file.
  (add-hook 'dirvish-mode-hook #'dirvish-peek-mode)
  (setq dirvish-preview-width 0.4) ; Preview window takes 40% of the frame width
  (setq dirvish-peek-show-on 'right)

  ;; 4. Define keybindings for yazi-style navigation.
  (map! :map dirvish-mode-map
        :n "h" #'dirvish-up-dir          ; Go to parent directory
        :n "l" #'dirvish-open-dwim       ; Open file or enter directory
        :n " " #'dirvish-toggle-preview) ; Manually toggle the preview pane

  ;; 5. Retain your original quick access directories.
  (setq dirvish-quick-access-entries
        '(("h" "~/" "Home")
          ("d" "~/Downloads/" "Downloads")
          ("D" "~/Documents/" "Documents")
          ("p" "~/Projects/" "Projects")
          ("/" "/" "Root"))))

