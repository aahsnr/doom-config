# Example Doom Emacs Configuration Examples
- [ ] https://github.com/ArtemSmaznov/dotfiles-doom-emacs.git 
- [ ] https://github.com/tecosaur/emacs-config/blob/master/config.org 
- [ ] https://github.com/hieutkt/dotfiles/tree/main/emacs/.doom.d
- [x] https://github.com/elken/doom
- [x] https://joshblais.com/posts/my-literate-doom-emacs-config/
- [ ] https://github.com/michaelneuper/doom


# All tasks must be done in the given order

- **Next Tasks**:
- [ ] **Add necessary LaTeX export stuff from elken-doom**
- [ ] **Org Modern TODO items has weird graphics issues. Use hl-todo instead**
- [ ] **Check DT's doom config for color-coded todo keywords**
- [ ] **Buffer Management with ibuffer and bufler**
- [ ] **Treemacs**
- [ ] **Git with treemacs**


-   [ ] **Make sure the following packages are working org source code
    blocks, first by manually checking, then adding if neccessary**

    -   Completion System
    -   Eglot, eldoc, dape, flymake
    -   Alphaelia

-   [ ] **Setup word-wrap in specific modes**

-   [ ] **Adapt elken's org-capture configuration into mine**


-   [ ] **Add toggle for dirvish, not dirvish side**

-   [x] **Evil**

-   [ ] **Keybindings**

-   [x] **Delimiters with smartparens and delimiters**

-   [x] **PDF Tools**

-   [x] **Winner-Mode**

-   [x] **Which-Key**

-   [x] **Solaire Mode**

-   [x] **Savehist**-

- [ ] **Add org habit to org config**

-   [ ] **Window Management**

-   [x] **Avy: Complete the configuration from avy.md**

-   [x] **Snippets: yasnippet + yasnippet-capf + consult-yasnippet**

-   [x] **Move-Text**

-   [ ] **Aggressive Indent**: \[Make sure enabled in org source code
    blocks. Does it provide extra functionality than evil indent plus?
    Maybe integrate with indent plus\]

-   [ ] **Helpful**

-   [x] **Pulsar**

-   [ ] **Casual**

-   [ ] **AutoSave**

-   [ ] **Indent Bars**

-   [ ] **Org**

    -   [ ] **flyspell**
    -   [ ] **cdlatex**
    -   [ ] **fill column**
    -   [ ] **display no. of words**
    -   [x] **integrate org-roam with org-roam-ql, org-roam-ql-ql,
        embark-org-roam, org-roam-timestamps**
    -   [ ] **setup padding from gemini (located in generated-configs)**
    -   [ ] \*setup org-roam general keybindings from incomplete
        claude.ai configuration in email unbent-crib-onyx@duck.com. Also
        complete org-roam from org-roam.md and compared it with the
        incomplete claude.ai configuration\*

-   [ ] **LaTeX**:

-   [ ] **Imenu**

-   [ ] **Crux**

-   [X] **Rainbow-Delimiters**

-   [X] **Smartparens**

-   [ ] **EasySession**

-   [ ] **tldr**

-   [ ] **add transient config after everything** Set a separate
    transient menu for magit

-   [ ] Integrate ripgrep and fd throughout the whole configuration

-   [ ] Improve existing vertico by adding the extensions from github

-   [x] Line Numbers from emacs-config.org

-   [ ] Org-mode export support or bibliography note management with
    Org-roam later.

-   [ ] Get jupyter config from python-dev-env.md

-   [ ] **Setup calendar, diary-lib, appt (appointments) later**

## The following packages must be working in org source code blocks

-   [ ] **Aggressive Indent**
-   [ ] **Move Text**
-   [ ] **Pulsar**
-   [ ] **Indent Bars**
-   [ ] **Origami**
-   [ ] **Rainbow Delimiters**
-   [ ] **Smartparens**
-   [ ] **Aggressive Indent**


______
# Using Built-in and Minimal Packages

- [ ] **Replace lsp-mode with eglot**

- [ ] **Replace dap-mode with dape**

- [ ] **Replace flycheck with flymake**

- [ ] **Add consult-flymake**
- [ ] **Replace tree-sitter with treesit**

- [ ] **Replace projectile entirely with project.el**
- [ ] **Replace persp-mode completely with perspective.el**


------------------------------------------------------------------------

# EMACS/DOOM EMACS

-   [ ] Use crafted-emacs and use deepseek to create emacs config files
-   [ ] Decide whether to use personal config instead of doom emacs
-   [x] Install hyprlang-ts-mode for emacs
-   [ ] Add color support to doom emacs
-   [ ] add scripts directory to path
-   [ ] add lisp code to path for use in configuration
-   [ ] highlight matching parenthesis and use rainbow brackets
-   [ ] integrate better defaults from emacs-config.org into
    fresh-emacs.org
-   [ ] line numbering support inside org-src-code blocks
-   [ ] borrow line numbers setting and minibuffer escape from
    emacs-config.org
-   [ ] borrow zooming config from emacs-config.org
-   [ ] org-capture binary from the doom emacs project
-   [ ] setup a doom doctor-like setup and binary from the doom emacs
    project
-   [ ] jupyter and latex integration inside org-babel
-   [ ] Use emacs to setup systemd files
-   [ ] after finishing emacs-config.org add features from from doom
    emacs init.el into the deepseek command
-   [ ] Setup Org-mode from tmp-org5.el and org-roam from deepseek to
    setup. Turn on org-modern todo check
-   [ ] Optimize org-mode scrolling using the deepseek setup
-   [ ] Add origami, drag-stuff support
-   [ ] Test tecosaur doom eamcs setup \[TODO\]
-   [ ] Add solaire-mode
-   [ ] Use evil keybindings wherever possible even in general
-   [ ] Look for overlapping keybindings using gemini
-   [ ] Train how to use all evil modules
-   [ ] Chat for Gemini/Deepseek: Optimize the emacs 30 configuration
    using the attached file. Make sure all the respective modules are
    loaded in the correct order. Make sure all the components work well
    with each other. Defer any component if necessary. Optimize the
    whole configuration as well. Then rewrite the whole configuration.

------------------------------------------------------------------------

# Integrate the following comments into doom emacs

1.  For the attached init.el file containing the base emacs 30
    configuration, write a comprehensive org-mode configuration using
    the built-in org-mode with org-mode optimizations and the following
    features and integration:
    -   org file directory is in \$home/org along with all other
        org-mode related files;
    -   extensive org headlines configuration with variable font size
        with each level of header, fonts using jetbrainsmono nerd font
        bold fonts for all headlines;
    -   prettify source code blocks with ligatures, icons and
        prettify-symbols-mode;
    -   all org files start in the overview mode;
    -   comprehesive org-agenda setup including integrations with
        org-super-agenda;
    -   comprehensive org-modern configuration with optimizations and
        integrations throughout the whole configuration and also include
        integrations with org-super-agenda, org-fragtog, org-download,
        org headlines and have custom org stars; use org-modern-table
        instead of a custom configuration;
    -   org-fancy-priorities configuration with integration into
        org-modern
    -   comprehensive org-todo configuration with ligatures and unicode
        integrated into org todo keywords
    -   make sure org-ellipses integrates well with org-modern stars
    -   have support for pretty tables in org files
    -   seamless integrations writing in bold, italic and underline
        texts
    -   have gruvbox dark theme integration throughtout the org
        configuration
    -   org-roam v2 configuration with the following features and
        integrations: features that are inspired by The Brain in
        <https://thebrain.com/>; keybindings that follow doom-emacs-like
        bindings; quality-of-life features and improvements; advance
        visualizations with org-roam-ui; obsidian-like features; don\'t
        follow obsidian keybindings;; org-roam v2 related files are
        inside the org directory; additional quality-of-life features
        and improvements
    -   comprehensive org-noter configuration that integrates well with
        org-roam v2 that integrates with pdf-tools
    -   integrate any other missing features for quality-of-life
        improvements for org-mode configuration

------------------------------------------------------------------------

-   enhance the above configuration
-   find and fix any errors and issues in the above configuration
    including syntax errors, brackets mismatch, etc. Remove any
    redundant configuration options. Then rewrite the whole
    configuration
-   further enhance the above configuration
-   find and fix any further errors and issues in the above
    configuration. Then rewrite the whole configuration


