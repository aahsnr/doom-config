;;; config.el
;; This file contains your personal Doom Emacs configuration.
;; It is loaded after all modules and packages have been initialized.

;; --- Core Jupyter & Org Babel Setup ---
(after! jupyter
  ;; Configure default header arguments for all jupyter-python blocks.
  ;; This ensures consistent behavior and nicer output formatting.
  (setq org-babel-default-header-args:jupyter-python
        '((:session . "doom-py")           ; Use a consistent session name
          (:kernel . "python3")            ; Specify the default kernel
          (:results . "output raw drawer") ; Format results neatly in a drawer
          (:async . "yes")                ; Execute asynchronously
          (:iopub-channel-timeout . 60)))  ; Increase timeout for long-running cells

  ;; Use the jupyter-repl for interactive execution with `C-c C-c`.
  ;; This sends code to a persistent kernel, maintaining state.
  (setq jupyter-C-c-C-c-function #'jupyter-repl-execute-buffer-or-block))

  ;; Add support for displaying pandas DataFrames as Org tables.
  ;; Doom's org module often handles many mime types automatically.
  ;; Uncomment the following lines ONLY if you encounter issues with HTML/plain text output:
  ;; (add-to-list! 'org-babel-mime-types '("text/html" . (lambda (data) (jupyter-org-babel-render-html data 'table))) t)
  ;; (add-to-list! 'org-babel-mime-types '("text/plain" . jupyter-org-babel-render-plain-text) t)
  

;; Ensure Jupyter kernels are aliased correctly after ob-jupyter loads.
;; This is a known fix for Doom Emacs's lazy loading to ensure kernels are recognized
;; when you first interact with Jupyter Org blocks.
(with-eval-after-load 'ob-jupyter
  (org-babel-jupyter-aliases-from-kernelspecs))

;; --- LSP Integration for Code Intelligence ---
;; (assuming :lang python with +lsp +pyright flags are enabled in init.el)
;; Doom's LSP module (with +peek) should automatically handle LSP in Org source blocks
;; via `lsp-mode` and `lsp-bridge` (if enabled). No explicit `lsp-babel` is needed.
(after! lsp-mode
  ;; Set LSP priority for pyright if you have multiple Python LSP servers.
  ;; This is already present in your config.el, so ensuring it's here for context.
  (set-lsp-priority! 'pyright 2))

  ;; If you use `direnv` module, LSP will often pick up the virtual environment automatically.
  ;; If not, and you need explicit virtual environment activation for LSP in Jupyter buffers,
  ;; you might add a hook like this:
  ;; (add-hook 'jupyter-python-mode-hook #'lsp-pyright-activate-venv)
  

;; --- Enhanced Code Completion (assuming :completion corfu is enabled in init.el) ---
(after! corfu
  ;; Ensure Corfu is active in Org source blocks.
  ;; `jupyter-python-mode` typically inherits from `python-mode`, which is often
  ;; covered by Doom's default `prog-mode-hook` for Corfu. However, this is explicit.
  (add-hook 'org-src-mode-hook #'corfu-mode)

  ;; Fix `RET` (Enter) key behavior in Org mode when Corfu is active.
  ;; This ensures that pressing Enter inserts the selected completion candidate
  ;; instead of a newline, which is a common desire for completion systems.
  (defadvice! +corfu--org-return (orig)
    :around '+org/return
    (if (and (featurep! :completion corfu) ; Check if Corfu module is enabled
             corfu-mode                     ; Check if Corfu is active in the current buffer
             (>= corfu--index 0))           ; Check if there are active completion candidates
        (corfu-insert)                      ; Insert the selected candidate
      (funcall orig))))                     ; Otherwise, call the original `+org/return` function
  

;; --- Code Formatting (using apheleia with black) ---
;; (assuming :editor format with +onsave +lsp flags are enabled in init.el)
;; Your `config.el` already sets up `apheleia` to use `black` for `python-mode`
;; and `python-ts-mode`. `jupyter-python-mode` typically inherits from `python-mode`,
;; so `apheleia` should automatically apply `black` when `+format/buffer` is called.

;; Custom function to format the current Org source block.
;; This function will now leverage Doom's `+format/buffer`, which in turn uses `apheleia`.
(defun +my/jupyter-format-src-block ()
  "Format the current Org source block using `apheleia` with `black` if it's a jupyter-python block."
  (interactive)
  (when (string-equal (org-src-get-lang) "jupyter-python")
    ;; `+format/buffer` is Doom's standard formatting function that respects `apheleia` setup.
    (+format/buffer)))

;; Bind manual formatting to `SPC o c f` in `org-mode-map`.
;; This provides a dedicated keybinding for formatting Jupyter blocks.
(map! :map org-mode-map
      :leader
      :prefix ("o" . "org")
      :prefix ("c" . "code")
      "f" #'+my/jupyter-format-src-block)

;; --- Interactive Debugging with DAP ---
;; (assuming :tools debugger with +lsp flag is enabled in init.el)
;; Your `config.el` already sets `dap-python-debugger` to `debugpy` and
;; `dap-python-terminal-kind` to `kitty`, ensuring consistency.
(after! dap-python
  ;; Register a debug template to attach to a running Jupyter kernel.
  ;; This template will appear when you use `dap-debug-by-name`.
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
  ;; Launch the predefined debug template.
  (dap-debug-by-name "Python :: Attach to Jupyter Kernel"))

;; --- Custom Keybindings for Jupyter in Org Mode ---
;; A dedicated leader menu for Jupyter operations within Org mode.
(map! :map org-mode-map
      :leader
      :prefix ("o" . "org")  ; Define 'o' as prefix for org-related commands
      :prefix ("j" . "jupyter") ; Define 'j' under 'o' as prefix for jupyter-related commands
      "r" #'jupyter-repl-associate-buffer    ;; (r)epl associate
      "s" #'jupyter-server-list-kernels      ;; (s)erver list kernels
      "x" #'jupyter-org-execute-block-and-next ;; e(x)ecute and advance
      "c" #'jupyter-repl-clear-all-results   ;; (c)lear all results (from original jupyter.el)
      "i" #'jupyter-inspect-at-point         ;; (i)nspect object (from original jupyter.el)
      "d" #'+my/dap-jupyter-debug-block      ;; (d)ebug block
      "b" #'dap-breakpoint-toggle)            ;; (b)reakpoint toggle (from original jupyter.el)
