;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

;; idea from https://github.com/syl20bnr/spacemacs/issues/7257
(defun my/load-customization (name)
  "Run Lisp file `~/.spacemacs.d/<name>.el' if it exists, otherwise do nothing."
  (let ((filename (expand-file-name (concat "~/.spacemacs.d/" name ".el"))))
    (when (file-readable-p filename)
      (load filename)
      t)))

;; --------------------------------------------------------------------------------

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(javascript
     html
     csv
     yaml
     graphviz
     racket
     python
     julia
     c-c++
     cscope
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     helm
     (auto-completion
      :variables
      ;auto-completion-enable-help-tooltip t
      auto-completion-enable-snippets-in-popup t
      auto-completion-return-key-behavior 'complete
      auto-completion-tab-key-behavior 'cycle
      auto-completion-complete-with-key-sequence nil
      auto-completion-complete-with-key-sequence-delay 0.1
      auto-completion-private-snippets-directory nil)
     ;; better-defaults
     emacs-lisp
     multiple-cursors
     treemacs
     git
     latex
     markdown
     ;; org
     (shell :variables
            shell-default-width 40
            shell-default-position 'right)
     ;; spell-checking may crash Emacs on the first load, because apt needs the
     ;; sudo password to install the dependencies, but Emacs doesn't prompt for it.
     ;; If this happens, look at the command Emacs is trying to run (at the status line),
     ;; run it manually (so that the installation completes), and kill and restart Emacs.
     ;; https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Bcheckers/spell-checking
     spell-checking
     syntax-checking
     ;; version-control
     themes-megapack
     pdf)

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(
                                      ;; helm-swoop-edit is broken, see: https://github.com/ShingoFukuyama/helm-swoop/issues/133
                                      (helm-swoop :location (recipe :fetcher github :repo "ashiklom/helm-swoop"))
                                      ;; http://pragmaticemacs.com/emacs/pop-up-a-quick-shell-with-shell-pop/
                                      ;; https://github.com/kyagi/shell-pop-el
                                      ;;shell-pop
                                      fireplace
                                      ;; The minimap in ELPA is old; even though both that and the one on GitHub
                                      ;; advertise themselves as version 1.2, only the one on GitHub has an option
                                      ;; to change the color of the highlighted line.
                                      ;; https://github.com/dengste/minimap
                                      ;;(minimap :location (recipe :fetcher github :repo "dengste/minimap"))
                                      sublimity
                                      yafolding
                                      beacon
                                      rainbow-mode
                                      mwim
                                      (bookmark+ :location (recipe :fetcher github :repo "emacsmirror/bookmark-plus"))
                                      ;; https://github.com/tlikonen/suomalainen-kalenteri
                                      suomalainen-kalenteri
                                      ;; https://github.com/zk-phi/phi-search
                                      (phi-search :location (recipe :fetcher github :repo "zk-phi/phi-search"))
                                      ;; https://github.com/knu/mc-extras.el
                                      mc-extras
                                      synosaurus)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

;; --------------------------------------------------------------------------------

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  (my/load-customization "platform-setup")

  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; File path pointing to emacs 27.1 executable compiled with support
   ;; for the portable dumper (this is currently the branch pdumper).
   ;; (default "emacs-27.0.50")
   dotspacemacs-emacs-pdumper-executable-file "emacs-27.0.50"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'emacs

   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(zenburn
                         spacemacs-dark
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator slant :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   ;; https://github.com/adobe-fonts/source-code-pro
   ;; doesn't work; https://github.com/syl20bnr/spacemacs/issues/3477
   dotspacemacs-default-font `(,my-default-font
                               :size 13
                               :weight normal
                               :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 16

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers t

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis t

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'changed

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

;; --------------------------------------------------------------------------------

;; Useful custom functions
(my/load-customization "functions")

;; --------------------------------------------------------------------------------
;; global keymap customizations

;; pdf-tools doesn't support helm-swoop or phi-search in PDF text, only the builtin isearch.
;; http://pragmaticemacs.com/emacs/view-and-annotate-pdfs-in-emacs-with-pdf-tools/
;; TODO: would be much cleaner to switch a "phi-search-mode" on/off depending on major mode. Investigate.
(defun my-isearch-forward (&rest args)
  "phi-search or isearch as appropriate."
  (interactive "P")
  (if (eq major-mode 'pdf-view-mode)
    (apply 'isearch-forward args)
    (if phi-search--active  ;; FIXME: accessing implementation detail
      (phi-search-next)
      (apply 'phi-search args))))

(defun my-isearch-backward (&rest args)
  "phi-search-backward or isearch-backward as appropriate."
  (interactive "P")
  (if (eq major-mode 'pdf-view-mode)
    (apply 'isearch-backward args)
    (if phi-search--active  ;; FIXME: accessing implementation detail
      (phi-search-previous)
      (apply 'phi-search-backward args))))

;; https://clojureverse.org/t/share-your-spacemacs-tweaks/1496/9
(defvar custom-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    ;; phi-search for a multiple-cursors compatible isearch replacement
    (define-key map (kbd "C-s") 'my-isearch-forward)
    (define-key map (kbd "C-r") 'my-isearch-backward)
    ;; Ctrl+Z undo shadows helm's action list viewer; let's place that on Alt+Z
    (define-key map (kbd "M-z") 'helm-select-action)  ; this seems to be the "C-z Actions"?
    (define-key map (kbd "C-z") 'undo-tree-undo)
    (define-key map (kbd "C-S-z") 'undo-tree-redo)
    (define-key map (kbd "C-S-c") 'copy-fullpath-of-current-buffer)
    (define-key map (kbd "C-S-d") 'copy-fullpath-of-current-directory)
    (define-key map (kbd "C-t") 'spacemacs/shell-pop-inferior-shell)  ; much more useful than transpose-chars
    (define-key map (kbd "C-S-t") 'open-dedicated-terminal)
    (define-key map (kbd "C-S-e") 'browse-file-directory)
    ;; (define-key map (kbd "C-e") 'move-end-of-line)  ; FIXME: unshadowing a default
    (define-key map (kbd "C-c e") 'eval-and-replace-sexp)
    (define-key map (kbd "C-<next>") 'switch-to-next-file-repeatable)
    (define-key map (kbd "C-<prior>") 'switch-to-previous-file-repeatable)
    (define-key map (kbd "M-S-q") 'unfill-paragraph)
    (define-key map (kbd "M-Q") 'unfill-paragraph)
    ;; (define-key map (kbd "M-q") 'fill-paragraph)  ; FIXME: unshadowing a default
    (define-key map (kbd "<f6>") 'iedit-mode)
    (define-key map (kbd "<f7>") 'org-agenda)
    (define-key map (kbd "S-<f8>") 'my-flyspell-correct-unlucky)
    (define-key map (kbd "<f8>") 'my-flyspell-correct-lucky)
    (define-key map (kbd "<f9>") 'toggle-minimap)
    (define-key map (kbd "S-<f12>") 'yafolding-go-parent-element)
    (define-key map (kbd "<f12>") 'yafolding-toggle-element)
    (define-key map (kbd "S-<backspace>") 'delete-indentation)
    (define-key map (kbd "S-<return>") 'newline-and-indent-relative)
    (define-key map (kbd "C-x 8 RET") 'counsel-unicode-char)
    ;; Replace EMACS's default sexp navigation keybindings with smartparens-enabled ones
    ;; https://www.emacswiki.org/emacs/NavigatingParentheses#toc7
    ;; https://github.com/Fuco1/smartparens  (works also with Python!)
    (define-key map (kbd "C-M-f") 'sp-forward-sexp)
    (define-key map (kbd "C-M-b") 'sp-backward-sexp)
    (define-key map (kbd "C-M-d") 'sp-down-sexp)
    (define-key map (kbd "C-M-a") 'sp-backward-down-sexp)
    (define-key map (kbd "C-M-e") 'sp-up-sexp)  ;; mnemonic: "Exit sexp"
    (define-key map (kbd "C-M-u") 'sp-backward-up-sexp)
    (define-key map (kbd "C-M-n") 'sp-next-sexp)
    (define-key map (kbd "C-M-p") 'sp-previous-sexp)
    (define-key map (kbd "C-M-k") 'sp-kill-sexp)
    (define-key map (kbd "C-M-SPC") 'sp-mark-sexp)
    map)
  "Minor mode for custom keymap.")

(define-minor-mode custom-keys-minor-mode
  "Minor mode for global keymap overrides."
  :init-value t
  :lighter nil
  :keymap custom-keys-minor-mode-map
  :global t
  (if custom-keys-minor-mode
      (progn  ; on
        (define-key evil-emacs-state-map (kbd "C-z") nil)  ; prevent shadowing "C-z" undo-tree-undo in custom map
        (spacemacs/set-leader-keys "s M" 'helm-multi-swoop-this-mode)  ; M-m s M, because M-m s m is taken by multiple-cursors
        (spacemacs/set-leader-keys "s C-p" 'helm-multi-swoop-projectile)  ; M-m s C-p (NOTE: searches open project buffers only)
        (spacemacs/set-leader-keys-for-major-mode 'latex-mode "O" 'reftex-toc)  ; M-m m O (same as in Spacemacs pdf layer)
        (spacemacs/set-leader-keys "M-j" 'evil-avy-goto-char-timer)  ; M-m M-j, less repetitive than default M-m j j
        )
      (progn  ; off
        (define-key evil-emacs-state-map (kbd "C-z") 'evil-exit-emacs-state)
        (spacemacs/set-leader-keys "s M" nil)
        (spacemacs/set-leader-keys "s C-p" nil)
        (spacemacs/set-leader-keys-for-major-mode 'latex-mode "O" nil)
        (spacemacs/set-leader-keys "M-j" nil)
        )))

;; --------------------------------------------------------------------------------

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env)
  ;; On Windows/MSYS2, by default, PATH in process-environment seems to be set incorrectly
  ;; (missing MSYS2 directories), but Emacs's exec-path sees it correctly.
  (when my-on-winnt
    (setenv "PATH" (mapconcat #'identity exec-path path-separator)))
  ;; Doesn't seem to use the LANG from the Linux environment on WSL (Windows Subsystem for Linux).
  (when my-on-wsl
    (setenv "LANG" "en_US.utf8"))
  )

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  ;; treemacs-dir is detected incorrectly, icons fail to load
  ;; https://github.com/Alexander-Miller/treemacs/issues/319
  ;; This doesn't seem to help, it's set at compile time. Instead, "M-m h d v treemacs-dir",
  ;; jump to the dir where the help says it's defined, and then "C-0 M-x byte-recompile-directory"
  ;; (the prefix argument zero forces a recompile of all .el files).
  ;;(set 'treemacs-dir "~/.emacs.d/elpa/27.0/develop/treemacs-20190513.1513/")
  (setq multiple-cursors-backend 'mc)  ; see ~/.emacs.d/layers/+misc/multiple-cursors/packages.el
  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."
  ;; Our custom isearch functions (that work around phi-search's lack of pdf-tools support)
  ;; need the var phi-search--active to be defined, which it isn't until phi-search loads.
  (require 'phi-search)
  ;; my-flyspell-correct-lucky needs flyspell-correct to be loaded so that
  ;; flyspell-correct-interface is defvar'd (otherwise the let, in
  ;; effect, does nothing).
  (require 'flyspell-correct)

  ;; https://emacs.stackexchange.com/questions/19472/how-to-let-auctex-open-pdf-with-pdf-tools
  ;; https://tex.stackexchange.com/questions/359924/rebinding-key-in-auctex-which-takes-other-argument
  (with-eval-after-load "latex"
    (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
          TeX-source-correlate-start-server t)
    ;; Update PDF buffers after successful LaTeX runs
    (add-hook 'TeX-after-compilation-finished-functions
              #'TeX-revert-document-buffer)
    (define-key LaTeX-mode-map (kbd "<f5>") 'my-default-TeX-command))

  ;; http://babbagefiles.blogspot.com/2017/11/more-pdf-tools-tricks.html
  (with-eval-after-load "pdf-tools"
    (add-hook 'pdf-view-mode-hook (lambda ()
                                     (pdf-view-midnight-minor-mode))))

  ;; fix default font
  ;; https://github.com/syl20bnr/spacemacs/issues/3477
  (set-face-attribute 'default nil :family my-default-font)
  (set-face-attribute 'default nil :height 100)
  ;; fix tab completion in Helm when Treemacs is open (workaround by Simon Bourne)
  ;; https://github.com/syl20bnr/spacemacs/issues/7446#issuecomment-417334718
  (with-eval-after-load "helm"
    (defun helm-persistent-action-display-window (&optional split-onewindow)
      "Return the window that will be used for persistent action.
If SPLIT-ONEWINDOW is non-`nil' window is split in persistent action."
      (with-helm-window
        (setq helm-persistent-action-display-window (get-mru-window)))))
  ;; http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html
  (setq ispell-local-dictionary "english")
  (setq ispell-local-dictionary-alist
        '(("english" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)  ;; default to en_US
          ("american" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)
          ("british" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_UK") nil utf-8)
          ("finnish" "[[:alpha:]]" "[^[:alpha:]]" "['-]" t ("-d" "fi") nil utf-8)))
  ;; TODO fix enchant on Windows/MSYS2 (builds fine with aspell and voikko, but fails to find any dictionaries)
  (setq ispell-program-name (if my-on-winnt "c:/msys64/usr/bin/aspell" "~/.local/bin/enchant"))
  ;; https://orgmode.org/worg/org-tutorials/orgtutorial_dto.html
  (if (or my-on-winnt my-on-wsl)
      (setq org-agenda-files (list "~/org/todo.org"))
      (setq org-agenda-files (list "~/org/personal.org" "~/org/home.org" "~/org/work.org")))
  ;;https://www.emacswiki.org/emacs/CalendarLocalization
  (setq calendar-week-start-day 1)
  (setq calendar-mark-holidays-flag t)
  (setq calendar-latitude +61.5)
  (setq calendar-longitude +23.8)
  (setq calendar-location-name "Tampere, Finland")
  (with-eval-after-load "calendar"
    (define-key calendar-mode-map (kbd "RET") 'calendar-insert-date))
  ;; synosaurus eats `C-c C-s', so let's rebind org entry scheduling to `C-c s'
  (with-eval-after-load "org"
    (define-key org-mode-map (kbd "C-c s") 'org-schedule))
  ;; make <f5> in python-mode run buffer like in racket-mode
  (with-eval-after-load "python"
    (define-key python-mode-map (kbd "<f5>") 'run-buffer-in-python))
  ;; helm-top is really nice. Hint from here: https://ambrevar.xyz/emacs-eshell/
  (with-eval-after-load "helm"
    (helm-top-poll-mode))
  (require 'suomalainen-kalenteri)

  ;; make M-. (look up definition) work in C/C++ projects
  ;; TODO: should maybe update the tags file when it goes out of date?
  ;; https://www.emacswiki.org/emacs/TagsFile
  (defadvice xref-find-definitions (before c-tag-file activate)
    "Automatically create tags file."
    (when (eq major-mode 'c-mode)
      (let ((tag-file (concat default-directory "TAGS")))
      (unless (file-exists-p tag-file)
        (shell-command "etags *.[ch] *.cpp -o TAGS 2>/dev/null"))
      (visit-tags-table tag-file))))

  ;; https://github.com/zk-phi/phi-search/issues/53
  (require 'mc-extras)
  (with-eval-after-load "phi-search"
    (defadvice phi-search-complete (after phi-search-auto-dedup last activate protect)
      (mc/remove-duplicated-cursors)))
  ;; https://www.reddit.com/r/emacs/comments/90si1c/why_cant_i_have_truly_persistent_undo_on_emacs/
  ;; default undo-limit is 80k, which is way too small in a large file if I accidentally `C-x C-p RET'
  ;; (mark page, replace all with an empty line) instead of `C-x p RET' (set BookmarkPlus bookmark).
  ;; (Happens with source code way too often, for any project that happens to have large modules.)
  (setq undo-limit 78643200)
  (setq undo-outer-limit 104857600)
  (setq undo-strong-limit 157286400)
  ;; TODO: bad Spacemacs style to require modules in init.el; how to set up lazy autoload for helm-swoop like "SPC s s" does?
  (require 'helm-swoop)  ; for helm-multi-swoop-this-mode
  (require 'bookmark+)
  (setq bmkp-autoname-format "^%B:[0-9]+,[0-9]+.*")  ; see my-auto-l+c-name
  (spacemacs|diminish beacon-mode)
  ;; TODO: Seems either Source Code Pro is missing some symbols Source Code Variable has, or the Linux fallback font for missing symbols is different.
  ;; (spacemacs|diminish beacon-mode (if (or my-on-winnt my-on-wsl) "*" "⛯") "*")
  (spacemacs|diminish flyspell-mode (if (or my-on-winnt my-on-wsl) "Sp" "📜") "Sp")
  (spacemacs|diminish pdf-view-midnight-minor-mode "☾" "Mid")
  (spacemacs|diminish git-timemachine-mode (if (or my-on-winnt my-on-wsl) "GTM" "🔃") "GTM")  ; no flux capacitor symbol in Unicode...
  ;; (spacemacs|diminish flycheck-mode "✔" "Stx")
  ;; (spacemacs|diminish visual-line-mode "⏎" "Vl")
  (spacemacs|diminish visual-line-mode)
  (spacemacs|diminish reftex-mode (if (or my-on-winnt my-on-wsl) "Ref" "🖹") "Ref")
  ;; (spacemacs|diminish synosaurus-mode "＝" "Syn")
  (spacemacs|diminish synosaurus-mode)
  ;; (spacemacs|diminish which-key-mode "？" "K?")  ; "⌘"
  (spacemacs|diminish which-key-mode)
  ;; TODO: only takes effect after a config reload (M-m f e R), why?
  ;; Wrapping it in a (with-eval-after-load "magit" ...) doesn't help.
  (spacemacs|diminish magit-gitflow-mode (if (or my-on-winnt my-on-wsl) "Fl" "🌊") "Flow")
  (spacemacs|diminish holy-mode)
  ;;(spacemacs|diminish auto-fill-mode "□" "Fl")
  ;; custom hotkeys
  (define-key 'iso-transl-ctl-x-8-map "l" [?λ])  ; automatically gives sensible display label for which-key
  ;; but can be done manually like this
  ;; (global-set-key (kbd "C-x 8 l") [?λ])
  ;; (which-key-add-key-based-replacements "C-x 8 l" "λ")
  (cua-mode)  ; standard cut, copy, paste hotkeys, also delete region on highlight & insert
  (custom-keys-minor-mode 1)
  ;; see commands kmacro-name-last-macro (name-last-kbd-macro) and insert-kbd-macro
  (fset 'sort-lines-and-save
        (kmacro-lambda-form [?í ?x ?l ?s ?í ?f ?s] 0 "%d"))
  (fset 'unparenthesize-python-return-stmt
        [?\C-s ?r ?e ?t ?u ?r ?n ?\( return left ?  ?\M-x ?s ?p ?- ?u ?n tab return])
  ;; minimap
  ;; https://github.com/zk-phi/sublimity
  (require 'sublimity)
  ;;(require 'sublimity-scroll)
  (require 'sublimity-map)
  (sublimity-map-set-delay nil)
  ;; (setq sublimity-scroll-weight 3
  ;;       sublimity-scroll-drift-length 3)
  ;; (require 'sublimity-attractive)
  ;; Python support
  (eval-after-load "company"
    '(add-to-list 'company-backends 'company-anaconda))
  (add-hook 'python-mode-hook 'anaconda-mode)
  ;; add thesaurus to text-mode
  (add-hook 'text-mode-hook 'synosaurus-mode)
  (setq synosaurus-choose-method 'popup)
  ;; enable smartparens also when writing text files (TODO: maybe just use the Spacemacs global toggle for this?)
  (add-hook 'text-mode-hook 'smartparens-mode)
  (add-hook 'web-mode-hook 'smartparens-mode)
  ;; add company-dabbrev to company-backends in text mode to enable completion from text already in buffer
  (eval-after-load "company"
    '(add-hook 'text-mode-hook 'my-company-text-mode-hook))
  (defun my-company-text-mode-hook ()
    "Enable some completers for Company in Text mode, and enable company-mode."
    (add-to-list 'company-backends 'company-dabbrev)
    (company-mode))
  ;;(add-hook 'latex-mode-hook #'outline-minor-mode)  ; maybe not needed, we have reftex
  ;; default modes
  (defun setup-mwim-keys ()
    "Set up smart home/end using the MWIM package."
    (define-key visual-line-mode-map (kbd "<home>") 'mwim-beginning-of-code-or-line)
    (define-key visual-line-mode-map (kbd "C-a") 'mwim-beginning-of-code-or-line)
    (define-key visual-line-mode-map (kbd "<end>") 'mwim-end-of-code-or-line)
    (define-key visual-line-mode-map (kbd "C-e") 'mwim-end-of-code-or-line)
    )
  (add-hook 'visual-line-mode-hook 'setup-mwim-keys)
  (global-visual-line-mode t)
  (spacemacs/toggle-zero-based-column-indexing-off)  ; one of the few things that is nice to index 1-based.
  (add-hook 'prog-mode-hook 'yafolding-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  ;; TODO: disable which-function-mode for LaTeX, not so reliable for random LaTeX documents.
  (add-hook 'prog-mode-hook 'which-function-mode)
  ;(add-hook 'prog-mode-hook 'rainbow-mode)  ; display color specifications (e.g. #A0A080, green) using the color they describe
  ;(add-hook 'text-mode-hook 'rainbow-mode)
  ;; TODO: how to add multiple modes in the same call?
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'smartparens-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'show-smartparens-mode)
  ;; (add-hook 'eval-expression-minibuffer-setup-hook #'rainbow-delimiters-mode)  ; does nothing in minibuffer
  ;; (add-hook 'eval-expression-minibuffer-setup-hook #'highlight-parentheses-mode)  ; does nothing in minibuffer
  ;; (fancy-battery-mode)
  ;; https://www.emacswiki.org/emacs/DisplayTime
  ;; (display-time-mode)
  ;; (setq display-time-24hr-format t)
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Fill-Commands.html
  (setq sentence-end-double-space nil)
  (spacemacs/toggle-automatic-symbol-highlight-on)  ; in addition should configure colors for ahs (see customs below)
  ;; WORKAROUND: Invoking evil-avy-goto-char-timer crashes
  ;; auto-highlight-symbol-mode when cursor is on a symbol,
  ;; so switch it off and back on.
  (defvar my-had-ahs nil "Internal variable for avy/ahs workaround.")
  (defadvice evil-avy-goto-char-timer (before avy-disable-ahs last activate)
    (setq my-had-ahs auto-highlight-symbol-mode)
    (when my-had-ahs
      (auto-highlight-symbol-mode 0)))
  (defadvice evil-avy-goto-char-timer (after avy-reenable-ahs first activate)
    (when my-had-ahs
      (auto-highlight-symbol-mode 1)))
  (beacon-mode t)  ; highlight cursor after scroll
  ;; "vim scrolloff", gradual scrolling https://wolfecub.github.io/dotfiles/
  (setq scroll-margin 10
        scroll-step 1
        scroll-conservatively 10000
        scroll-preserve-screen-position 1)
  ;; Fix scrolloff when scrolling up from bottom of buffer
  ;; (courtesy of @sollidsnake on GitHub)
  ;; https://github.com/syl20bnr/spacemacs/issues/8224
  (defvar sm-use-visual-line t)
  (defun sm-fix-enable()
    (advice-add 'previous-line :around 'sm-fix))
  (defun sm-fix-disable()
    (advice-remove 'previous-line 'sm-fix))
  (defun sm-get-lines-from-top ()
    (if sm-use-visual-line
        (save-excursion
          (beginning-of-line)
          (count-screen-lines (point) (window-start)))
      (- (line-number-at-pos (point)) (line-number-at-pos (window-start)) )))
  (defun sm-fix(old-function &rest args)
    (apply old-function args)
    (let ((diff (- scroll-margin (sm-get-lines-from-top))))
      (when (> diff 0)
        (scroll-down diff))))
  (sm-fix-enable)

  ;; prettify symbols
  (defun my/prettify-symbols-setup ()
    "Set up symbol prettification (base settings for all programming languages)."
    ;; see counsel-unicode-char and C-x 8 RET
    (let ((xs '(;("lambda" . ?λ)  ; lambda is already there by default
                ("<=" . ?≤)
                (">=" . ?≥)
                ("!=" . ?≠)
                ("=>" . ?⇒)
                ("->" . ?→)
                ("<-" . ?←)  ; Haskell do-notation
                )))
      (dolist (x xs nil)
        (push x prettify-symbols-alist)))
    (prettify-symbols-mode))
  (defun my/prettify-python-setup ()  ; suggestions from https://wolfecub.github.io/dotfiles/
     "Set up symbol prettification (additional settings for Python)."
     (let ((xs '(("**2" . ?²)
                 ("**3" . ?³)
                 ("**4" . ?⁴)
                 ("**5" . ?⁵)
                 ("**6" . ?⁶)
                 ("**7" . ?⁷)
                 ("**8" . ?⁸)
                 ("**9" . ?⁹)
                 ;; https://emacs.stackexchange.com/questions/34808/using-prettify-symbols-with-strings-instead-of-characters
                 ("**-1" . (?⁻ (Br . Bl) ?¹))  ; ⁻¹
                 ("**-2" . (?⁻ (Br . Bl) ?²))  ; ⁻²
                 ("**-3" . (?⁻ (Br . Bl) ?³))  ; ⁻³
                 ("**-4" . (?⁻ (Br . Bl) ?⁴))  ; ⁻⁴
                 ("**-5" . (?⁻ (Br . Bl) ?⁵))  ; ⁻⁵
                 ("**-6" . (?⁻ (Br . Bl) ?⁶))  ; ⁻⁶
                 ("**-7" . (?⁻ (Br . Bl) ?⁷))  ; ⁻⁷
                 ("**-8" . (?⁻ (Br . Bl) ?⁸))  ; ⁻⁸
                 ("**-9" . (?⁻ (Br . Bl) ?⁹))  ; ⁻⁹
                 ("sum" . ?∑)
                 ("prod" . ?∏)  ; numpy.prod; unpythonic.fold.prod  https://github.com/Technologicat/unpythonic
                 ("product" . ?∏)  ; pandas; also alternative name for prod in numpy
                 ("and" . ?∩)
                 ("or" . ?∪)
                 ("not" . ?¬)
                 ;; Sometimes "in" means "∈", but sometimes not:
                 ;;   x in A               set membership, ok
                 ;;   for x in range(10)   iteration, not ok
                 ;;   let[(x, 21) in 2*x]  marker for let-expression body (unpythonic.syntax.let), not ok
                 ;; prettify-symbols-mode isn't smart enough to tell these apart, so we leave "in" as-is.
                 ;("in" . ?∈)
                 ("not in" . ?∉)  ; "not in" is only used for testing the absence of membership.
                 ("is" . ?≡)
                 ("is not" . ?≢)
                 ("==" . ?＝)
                 ("=" . ?←)
                 ("all" . ?∀)
                 ("any" . ?∃)
                 ("None" . ?∅)
                 ("return" . ?➡)
                 ;("inf" . ?∞)  ; TODO: not a symbol. Usually seen in a string, float("+inf").
                 ;("def" . ?ƒ)  ; looks silly; literal "def" easier to spot.
                 )))
       (dolist (x xs nil)
         (push x prettify-symbols-alist)))
     (prettify-symbols-mode))
  (add-hook 'prog-mode-hook 'my/prettify-symbols-setup)
  (add-hook 'python-mode-hook 'my/prettify-python-setup)
  (global-prettify-symbols-mode)
  ;; TODO: takes effect from the **second** Python file opened. Figure out why. (Maybe font-lock for python mode scans the file before the hooks run?)
  (defun my/unpythonic-syntax-highlight-setup ()
    "Set up additional syntax highlighting for `unpythonic.syntax' in python mode."
    ;; adapted from code in dash.el
    (let ((new-keywords '("let" "dlet" "blet"
                          "letseq" "dletseq" "bletseq"
                          "letrec" "dletrec" "bletrec"
                          "let_syntax" "abbrev"
                          "where"
                          "do" "local" "delete"
                          "continuations" "call_cc"
                          "curry" "lazify" "envify" "tco" "prefix" "autoreturn" "forall"
                          "multilambda" "namedlambda" "quicklambda"
                          "cond" "aif" "autoref" "dbg" "nb"
                          "macros" "q" "u" "hq" "ast_literal")) ; macropy
          (special-variables '("it"
                               "dyn"
                               "dbgprint_expr")))
      (font-lock-add-keywords 'python-mode `((,(concat "\\_<" (regexp-opt special-variables 'paren) "\\_>")
                                              1 font-lock-variable-name-face)) 'append)
      ;; "(\\s-*" maybe somewhere?
      (font-lock-add-keywords 'python-mode `((,(concat "\\_<" (regexp-opt new-keywords 'paren) "\\_>")
                                              1 font-lock-keyword-face)) 'append)
  ))
  (add-hook 'python-mode-hook 'my/unpythonic-syntax-highlight-setup)
  (setq inhibit-compacting-font-caches t)  ; https://wolfecub.github.io/dotfiles/
  ;; this comboes with the .bashrc "alias em='emacsclient -c'"; use `C-x #' (M-x server-edit) to close a file.
  (when my-on-winnt
    (server-start))
  ;; https://www.reddit.com/r/bashonubuntuonwindows/comments/70i8aa/making_emacs_on_wsl_open_links_in_windows_web/
  (when my-on-wsl
    (setq
     cmdExeBin"/mnt/c/Windows/System32/cmd.exe"
     cmdExeArgs '("/c" "start" "") )
    (setq
     browse-url-generic-program  cmdExeBin
     browse-url-generic-args     cmdExeArgs
     browse-url-browser-function 'browse-url-generic))
  ;; ;; no need for hook, this whole function runs after init is done
  ;; (add-hook 'after-init-hook #'fancy-battery-mode)
  ;; (add-hook 'after-init-hook #'display-time)
)

;; In custom settings, use tmispell instead of classic ispell to check also Finnish; part of Voikko.
;; EDIT: in 2019, deprecated. Use enchant instead, Voikko has a provider also for that.
;; https://voikko.puimula.org/

;; major-mode text-mode takes effect for `C-x b'; `M-m b N n' uses dotspacemacs-new-empty-buffer-major-mode.

;; Note matplotlib in inferior-python requires the python3-tk package in Debian/Ubuntu.

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(beacon-color "goldenrod")
 '(bmkp-auto-light-when-jump 'any-bookmark)
 '(bmkp-auto-light-when-set 'any-bookmark)
 '(bmkp-autoname-bookmark-function 'my-auto-l+c-name)
 '(bmkp-autotemp-bookmark-predicates nil)
 '(bmkp-bookmark-map-prefix-keys '("p" [134217837 66]))
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/.cache/bookmarks")
 '(evil-want-Y-yank-to-eol nil)
 '(flycheck-flake8rc "~/.config/flake8")
 '(flycheck-python-flake8-executable "python3")
 '(flycheck-python-mypy-executable "python3")
 '(flycheck-python-pycompile-executable "python3")
 '(flycheck-python-pylint-executable "python3")
 '(importmagic-python-interpreter "python3")
 '(major-mode 'text-mode)
 '(minimap-update-delay 0)
 '(minimap-window-location 'right)
 '(mwim-beginning-of-line-function
   '((t . beginning-of-visual-line)
     (message-mode . message-beginning-of-line)
     (org-mode . org-beginning-of-line)))
 '(mwim-end-of-line-function '((t . end-of-visual-line) (org-mode . org-end-of-line)))
 '(nil nil t)
 '(package-selected-packages
   '(csv-mode yaml-mode synosaurus graphviz-dot-mode mc-extras phi-search helm-cscope xcscope helm-rtags google-c-style flycheck-rtags disaster cpp-auto-include company-rtags rtags company-c-headers clang-format multiple-cursors suomalainen-kalenteri lsp-julia lsp-mode julia-repl julia-mode fireplace shell-pop company-quickhelp yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode dash-functional helm-pydoc cython-mode company-anaconda anaconda-mode pythonic flycheck-pos-tip pos-tip flycheck company-auctex auctex zenburn-theme zen-and-art-theme white-sand-theme underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme seti-theme reverse-theme rebecca-theme railscasts-theme purple-haze-theme professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme organic-green-theme omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme naquadah-theme mustang-theme monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme majapahit-theme madhat2r-theme lush-theme light-soap-theme jbeans-theme jazz-theme ir-black-theme inkpot-theme heroku-theme hemisu-theme hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gandalf-theme flatui-theme flatland-theme farmhouse-theme exotica-theme espresso-theme dracula-theme django-theme darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme smeargle orgit mmm-mode markdown-toc markdown-mode magit-gitflow magit-popup helm-gitignore helm-company helm-c-yasnippet gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md fuzzy evil-magit magit transient git-commit with-editor company-statistics company auto-yasnippet yasnippet ac-ispell auto-complete ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hydra lv hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-projectile projectile pkg-info epl helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist highlight evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async))
 '(paradox-github-token t)
 '(prettify-symbols-unprettify-at-point 'right-edge)
 '(python-shell-interpreter "ipython3")
 '(sort-fold-case t)
 '(python-shell-interpreter-args "--simple-prompt --matplotlib=tk -i")
 '(sublimity-map-active-region 'secondary-selection))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-face ((t (:background "olive drab" :foreground "GhostWhite"))))
 '(ahs-plugin-whole-buffer-face ((t (:background "dark olive green" :foreground "ghost white"))))
 '(bmkp-light-autonamed ((t (:background "SteelBlue4"))))
 '(bmkp-light-autonamed-region ((t (:background "SteelBlue4"))))
 '(bmkp-light-fringe-autonamed ((t (:background "SteelBlue4"))))
 '(bmkp-light-fringe-non-autonamed ((t (:background "SteelBlue4"))))
 '(bmkp-light-non-autonamed ((t (:background "SteelBlue4"))))
 '(bmkp-light-non-autonamed-region ((t (:background "SteelBlue4"))))
 '(minimap-active-region-background ((t (:background "dark olive green"))))
 '(minimap-current-line-face ((t (:background "olive drab" :foreground "olive drab"))))
 '(minimap-font-face ((t (:height 20 :family my-default-font))))
 '(mouse ((t (:background "gainsboro"))))
 '(spacemacs-emacs-face ((t (:inherit 'mode-line :background "SkyBlue4")))))
)
