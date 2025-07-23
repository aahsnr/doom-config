You are absolutely right to point that out. My apologies for the oversight. The Doom Emacs `direnv` module indeed uses the `envrc` package, not `direnv.el`, which operates differently. This distinction is crucial for a correct configuration.

Thank you for the correction. Let's rewrite the entire guide with the proper integration for `envrc`. This version is more accurate to your Doom Emacs setup and achieves the same powerful, project-based workflow.

-----

## 1\. Overview

This definitive guide provides an enhanced Jupyter setup for your Doom Emacs configuration. It is tailored to how Doom's modules, specifically `:tools direnv` (which uses `envrc.el`), actually work.

The goal remains a "zero-friction" environment where:

  - **Project-specific environments** are loaded automatically and transparently by `envrc`.
  - **LSP features** (completion, documentation) connect to the correct project-specific Python server.
  - **Plots and results** are handled gracefully and displayed inline.
  - **Session management** is available via simple, intuitive keybindings.

## 2\. Prerequisites

Ensure the following are set up correctly before configuring Emacs.

### Doom Emacs Modules (`init.el`)

Your `init.el` is correctly configured. This setup relies on these specific Doom modules:

```el
;;; in init.el
       (org +jupyter)
       (python +conda +poetry +pyright +tree-sitter)
       :tools
       direnv            ; Uses envrc.el for project-specific environments
       (lsp +eglot)      ; You use eglot for LSP
```

### System Environment

1.  **Jupyter & Kernels:** In each Python environment you plan to use (e.g., a specific poetry or conda environment), install the necessary packages.

    ```bash
    # Using pip
    pip install jupyterlab ipykernel

    # Using poetry
    poetry add jupyterlab ipykernel
    ```

2.  **Direnv (`.envrc`) Setup:** This is the core of the automatic environment switching. In your project's root directory, create a `.envrc` file. This file will export the necessary environment variables (like `PATH`) for any shell or application (including Emacs subprocesses) running in that directory.

      * For a **Poetry** project, your `.envrc` should contain:
        ```bash
        layout python3
        source $(poetry env info --path)/bin/activate
        ```
      * For a **Conda** environment, it might look like:
        ```bash
        layout anaconda
        conda activate my-env-name
        ```
      * After creating or editing the file, you must authorize it by running `direnv allow` in your terminal from that directory.

## 3\. Full Emacs Configuration

Place the following configuration in your `config.el` file. It's simpler than the previous version because we are correctly relying on the built-in behavior of Doom's `direnv` module.

```el
;;; in config.el

;; ## ----------------------------------------------------------------------
;; ## Part A: Core Jupyter & Org Babel Integration
;; ## ----------------------------------------------------------------------
;; No separate direnv configuration is needed. Doom's `:tools direnv` module
;; activates `envrc-mode` globally. When you open a file in a project,
;; `envrc` automatically sets the environment for any subprocesses
;; Emacs launches, such as Jupyter kernels or LSP servers.
(use-package! ob-jupyter
  :after org
  :config
  ;; Automatically discover and create friendly names for available kernels.
  ;; When run, this command inherits the environment from `envrc`, so it
  ;; finds your project-specific kernels.
  (org-babel-jupyter-aliases-from-kernelspecs)

  ;; Default headers for jupyter blocks. Results will be output as files
  ;; in a drawer, which is great for plots and rich output.
  (setq org-babel-default-header-args:jupyter
        '((:session . "jupyter-python") ; Use a consistent session name
          (:results . "file drawer replace")
          (:async . "yes") ; Run blocks asynchronously to not freeze Emacs
          (:cache . "no")))

  ;; Ensure inline images are displayed automatically after execution.
  (add-hook 'org-babel-after-execute-hook #'org-display-inline-images 'append))

;; ## ----------------------------------------------------------------------
;; ## Part B: Enhanced Eglot LSP for Python in Org Blocks
;; ## ----------------------------------------------------------------------
;; This hook ensures that when you edit a Python source block, Eglot connects.
;; Because `eglot-ensure` launches its server as a subprocess, it correctly
;; inherits the `PATH` from `envrc`. This means it finds and starts the
-;; `pyright` language server from your project's virtual environment without
+;; `pyright` language server from your project's virtual environment without
 ;; any extra configuration.
(add-hook! 'org-src-mode-hook
  (defun +my/org-src-eglot-ensure ()
    ;; Only activate in files that are part of a project recognized by projectile
    (when (and (string-equal-ignore-case (get-char-property (point) 'babel-lang) "python")
               (project-current))
      (require 'eglot)
      (eglot-ensure)))
  ;; The hook is buffer-local, so it's safe to add directly.
  (add-hook 'hack-local-variables-hook #'+my/org-src-eglot-ensure nil t))


;; ## ----------------------------------------------------------------------
;; ## Part C: Custom Functions & Keybindings for Jupyter Workflow
;; ## ----------------------------------------------------------------------
(defun my/org-jupyter-restart-kernel ()
  "Restart the Jupyter kernel for the current Org buffer's session."
  (interactive)
  (if-let ((session (org-babel-get-session-info 'jupyter)))
      (progn
        (jupyter-kernel-restart (jupyter-session-kernel session))
        (message "Jupyter kernel restarted."))
    (message "No active Jupyter session for this buffer.")))

(defun my/org-jupyter-clear-results ()
  "Clear all Jupyter results in the current Org buffer."
  (interactive)
  (org-babel-map-src-blocks nil
    (when (string-equal-ignore-case (org-babel-get-src-block-info '("language")) "jupyter")
      (org-babel-remove-result)))
  (message "All Jupyter results cleared."))

(map! :leader
      :mode 'org-mode
      :prefix ("j" . "jupyter")
      :desc "Execute block"          "e" #'org-babel-execute-src-block
      :desc "Execute block (sync)"   "E" #'org-babel-execute-src-block-maybe
      :desc "Restart kernel"         "r" #'my/org-jupyter-restart-kernel
      :desc "Clear all results"      "c" #'my/org-jupyter-clear-results
      :desc "Go to kernel buffer"    "b" (lambda () (interactive) (jupyter-show-session-buffer)))
```

## 4\. Advanced Usage Workflow

The workflow is now even more transparent.

1.  **Open a Project File**: Navigate to any file within a `direnv`-enabled project. `envrc-mode` (indicated in your modeline) will load the `.envrc` file and configure the environment for subprocesses.
2.  **Write Code**: Create a `jupyter` source block in an Org file.
    ```el
    #+begin_src jupyter
      import sys
      import pandas as pd

      # This will print the path to the python executable in your
      # project's virtual environment, not the system python!
      print(sys.executable)
      print(f"Pandas version: {pd.__version__}")
    #+end_src
    ```
3.  **Edit and Execute**:
      * As you edit inside the block, Eglot will start the correct `pyright` server from your virtual environment, giving you accurate completions and diagnostics.
      * Press **`SPC j e`** to execute. The `ob-jupyter` process inherits the correct environment from `envrc`, finds the right `jupyter` command, and connects to the kernel from your project's virtual environment.
4.  **Manage Your Workflow**:
      * Use **`SPC j r`** to restart a stuck kernel.
      * Use **`SPC j c`** to clean the buffer of all generated output.
      * Any plots you create will appear inline automatically.
