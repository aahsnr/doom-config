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

[!Note](All%20steps%20must%20contain%20the%20following%20replies:) Add
org-mode after seeing crafted-emacs config, write part about org
configuration with org-appear, org-modern, denote, org-fragtog Write a
state-of-the-art emacs 30 configuration in org-mode that will be tangled
to init.el with the following features, properties and integrations:

-   divide the whole org file into sensible titles and respectives
    emacs-lisp org source code blocks with integration between the
    source code blocks for their respective configurations

-   optimize emacs startup time and optimize the whole configuration
    where possible. all packages must be lazy loaded like neovim
    wherever possible.\

-   use both elpaca and straight.el as package manager but only
    use-package format to install packages using elpaca package manager.
    straight.el is used to manage packages from git repos

-   aggressive emacs optimizations to the configuration wherever
    possible

-   comprehensive doom tokyo-night theme integration throughout the
    configuration and wherever possible

-   setup automatic package update

-   comprehensive keybindings configuration with doom emacs-like and
    spacemacs-like bindings and vim bindings integration using the
    general emacs package. Vim keybindings must not clash with the doom
    emacs-like or spacemacs-like keybindings

-   minimal ui along with zen mode integration

-   relace yes/no prompts for y/n

-   disable automatically starting the splash screen, startup message,
    scratch message on startup

-   comprehensive lsp-mode configuration for all the major programming
    mode\

-   comprehensive tree-sitter support with treesitter integration for
    any part of the emacs 30 config that needs it.

-   comprehensive editorconfig configuration to have cross-editor/ide
    like features

-   comprehensive ibuffer configuration following keybindings from doom
    emacs project and integrating the ibuffer-project emacs package

-   color and emojis support for emacs 30 as well as rainbow-mode
    integration

-   comprehensive evil configuration following the evil configuration
    from the doom emacs project and including setups for the following
    packages: emacs-collection, evil-nerd-commenter and evil-goggles

-   comprehensive completion system using extensive configurations for
    cape, consult, corfu, corfu-terminal, embark embark-consult,
    marginalia, orderless, and vertico. Have nerd-icons and tokyonight
    night theme integration wherever possible

-   comprehensive lisp configuration for lisp modes including
    emacs-lisp, sly, clojure and guile. All lisp modes must have
    aggressive indent integration. The following emacs packages will be
    setup: package-lint, package-lint-flymake, sly, sly-asdf,
    sly-quicklisp, sly-repl-ansi-color, cider, clj-refactor,
    clojure-mode, flycheck-clojure, geiser, geiser-guile and
    geiser-racket.

-   comprehensive speedbar configuration. speedbar is built into
    emacs 30. Don\'t pull from package manager sources or git sources.

-   keep folders clean by no littering emacs package and disable
    nativecomp warnings

-   set default fonts as JetBrainsMono Nerd Font and Ubuntu Nerd Font
    for variable pitch fonts

-   comprehensive and state-of-the-art dired configuration with the
    following features and integrations: ranger integration; keybindings
    must follow the keybindings from the doom emacs project; file
    preview for various types of files; files and folders must only show
    the icon and title of the respective file and/or folder in that
    particular order; nerd icons integration; tokyonight night theme
    integration; hidden files must be shown with distinction from
    regular files; folders must be shown first then files are shown;
    respective files and/or folders for hidden files must be shown first
    before their regular counterpars

-   comprehensive setup for looking up documentation for all common
    programming languages

-   all-the-icons and nerd-icons integration throughout the
    configuration where needed. Don\'t use both. Mainly have nerd icons
    integration for the whole configuration and all the icons where nerd
    icons use is not available

-   extensive dabbrev integration throughout the configuration

-   add the ability to drag stuff (words, region, lines) around in Emacs
    using drag-stuff emacs package

-   add the ability to format the a file on save using format-all
    package for the available file types

-   setup comprehensive flycheck configuration using the flycheck
    package

-   setup comprehensive magit configuration that includes
    git-timemachine package

-   extensive helpful configuration for the package helpful from
    <https://github.com/Wilfred/helpful> instead of the built-in emacs
    30 one

-   extensive indent guides highlighting setup using
    highlight-indent-guides package

-   comprehensive dap-mode setup integrated with lsp-mode

-   comprehensive ligature configuration using the ligature emacs
    package

-   comprehensive emacs modeline configuration using doom-modeline
    package inspired by the doom emacs project

-   comprehensive treemacs configuration following keybindings from doom
    emacs project, integration with tokyo-night night theme and nerd
    icons theming

-   support for editing nix files in emacs

-   comprehensive prettiy-symbols configuration with integration for all
    programming modes

-   support for re-opening all open buffers and files if emacs crashes
    for any reason

-   add quality of life features for delimites including highlighting
    for matching parenthesis and extensive rainbow-delimiters
    integration

-   comprehensive liguratures configuration

-   comprehensive centaur-tabs configuration including features and
    keybindings from the doom emacs project

-   comprehensive vterm configuration with optimizations and vterm
    toggle integration. Disable the use of eshell in emacs

-   comprehensive which-key configuration with which-key being at the
    bottom and having idle delay of 0.1 and using → as the separator

-   comprehensive snippets configuration using yasnippet and the
    snippets are integrated throughout the whole configuration

-   comprehensive dashboard configuration using the emacs-dashboard
    packages with quality of life improvements emacs and following the
    theming and features from the doom emacs dashboard


