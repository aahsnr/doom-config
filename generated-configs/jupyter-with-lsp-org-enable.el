;;; jupyter.el --- Enhanced Jupyter environment for Org Mode -*- lexical-binding: t; -*-

;;;--------------------------------------------------------------------------
;;; Enhanced Jupyter Programming Environment in Org Mode
;;;
;;; This configuration turns Org Mode into a powerful, interactive
;;; notebook for Python, powered by the `emacs-jupyter` package. It
;;; integrates with Doom's existing LSP, completion, debugging, and
;;; formatting frameworks to provide a seamless experience directly
;;; within .org files.
;;;--------------------------------------------------------------------------

;; --- Part 1: Core Jupyter & Org Babel Setup ---
;; Configures the fundamental behavior of jupyter kernels and how Org Babel
;; interacts with them.
(after! jupyter
  ;; Set the Python interpreter for the server. This should match the
  ;; interpreter where your jupyter and data science packages are installed.
  (setq python-shell-interpreter "python3")

  ;; Configure default header arguments for all jupyter-python blocks.
  ;; This ensures consistent behavior and rich output formatting.
  (setq org-babel-default-header-args:jupyter-python
        '((:session . "doom-py")            ; Use a consistent session name to maintain state.
          (:kernel . "python3")               ; Specify the default kernel.
          (:results . "output raw drawer")   ; Format results neatly in a drawer.
          (:async . "yes")                   ; Execute asynchronously to prevent UI freezing.
          (:iopub-channel-timeout . 60)      ; Increase timeout for long-running cells.
          (:cache . "yes")                   ; Cache results to speed up re-exporting.
          (:noweb . "yes")))                 ; Allow referencing other code blocks.

  ;; Use the jupyter-repl for interactive execution with `C-c C-c`.
  ;; This sends code to a persistent kernel, maintaining state across blocks.
  (setq jupyter-C-c-C-c-function #'jupyter-repl-execute-buffer-or-block)

  ;; Add support for displaying pandas DataFrames and other rich outputs as Org tables.
  (add-to-list 'org-babel-mime-types '("text/html" . (lambda (data) (jupyter-org-babel-render-html data 'table))) t)
  (add-to-list 'org-babel-mime-types '("text/plain" . jupyter-org-babel-render-plain-text) t))


;; --- Part 2: LSP Integration for Code Intelligence ---
;; Uses `lsp-org`, the modern, built-in LSP support for Org Mode. It
;; activates the `pyright` language server inside source blocks, providing
;; definitions, references, and on-the-fly diagnostics.
(add-hook 'org-mode-hook #'lsp-org-enable)


;; --- Part 3: Enhanced Code Completion ---
;; This function adds completions from the live Jupyter kernel (e.g., variables,
;; dataframe columns) to the standard Corfu completion-at-point functions (CAPFs).
;; This merges kernel-aware completions with LSP suggestions.
(defun +my/jupyter-completion-setup ()
  "Add Jupyter kernel completions to `completion-at-point-functions`."
  (when (and (derived-mode-p 'org-mode)
             (string-match-p "jupyter" (or (cdr (assq :language (org-babel-get-src-block-info))) "")))
    ;; Add the jupyter completion function to the *local* hook, so it only
    ;; applies to the current buffer and not globally.
    (add-hook 'completion-at-point-functions #'jupyter-completion-at-point nil t)))

;; This hook ensures that completions are available as soon as you
;; start editing a source block.
(add-hook 'org-src-mode-hook #'+my/jupyter-completion-setup)


;; --- Part 4: Linting with Flycheck ---
;; This ensures that when you edit a source block with `C-c '`, Flycheck is
;; automatically enabled. It will use your existing Python linter configuration
;; (pylint, flake8) to provide on-the-fly feedback.
(add-hook 'org-src-mode-hook #'flycheck-mode)


;; --- Part 5: Code Formatting in Org Blocks ---
;; A helper command to format the current Python source block using your
;; configured default formatter (black, via apheleia).
(defun +my/jupyter-format-src-block ()
  "Format the current Python org source block using `+format/buffer`."
  (interactive)
  (when (string-equal (cdr (assq :language (org-babel-get-src-block-info))) "python")
    (let ((info (org-babel-get-src-block-info)))
      ;; Open the block in a dedicated python-mode buffer
      (org-edit-special)
      ;; Call Doom's universal formatting command
      (call-interactively #'+format/buffer)
      ;; Exit the special editor, applying the changes
      (org-edit-src-exit))))

;; Bind formatting to `SPC c f` inside the source block editor (`org-edit-special`).
(map! :map org-src-mode-map
      :localleader
      :prefix ("c" . "code")
      "f" #'+my/jupyter-format-src-block)


;; --- Part 6: Interactive Debugging with DAP ---
;; Configures `dap-mode` to attach to a running Jupyter kernel for a full
;; debugging experience, including breakpoints, stepping, and inspection.
(after! dap-python
  (dap-register-debug-template
   "Python :: Attach to Jupyter Kernel"
   (list :type "python"
         :request "attach"
         :name "Python :: Attach to Jupyter Kernel"
         ;; This magic line tells dap-python to get connection info from the jupyter package.
         :connect (list :host "127.0.0.1"
                        :port (lambda () (jupyter-get-connection-info :type 'shell))))))

(defun +my/dap-jupyter-debug-block ()
  "Start a DAP session to debug the current Org source block."
  (interactive)
  ;; Ensure a kernel is running before attempting to attach.
  (jupyter-repl-ensure-kernel)
  ;; Launch the debug template defined above.
  (dap-debug-by-name "Python :: Attach to Jupyter Kernel"))


;; --- Part 7: Custom Keybindings ---
;; A dedicated leader menu for Jupyter operations in Org mode for quick access
;; to common commands, following Doom's conventions.
(map! :map org-mode-map
      :leader
      :prefix ("o" . "org")
      :prefix ("j" . "jupyter")
      "r" #'jupyter-repl-associate-buffer    ;; (r)epl associate buffer with kernel
      "s" #'jupyter-server-list-kernels      ;; (s)erver list running kernels
      "x" #'jupyter-org-execute-block-and-next ;; e(x)ecute and advance to next block
      "c" #'jupyter-repl-clear-all-results   ;; (c)lear all results in buffer
      "i" #'jupyter-inspect-at-point         ;; (i)nspect object at point
      "d" #'+my/dap-jupyter-debug-block      ;; (d)ebug the current block
      "k" #'jupyter-shutdown-kernel)         ;; (k)ill the kernel
