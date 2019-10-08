;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;
;; Custom configuration, refactored into its own file as it's a bit long.

(defun my/user-config ()
  "Load custom settings. Loaded from `dotspacemacs/user-config' in `init.el'."

  ;; Default undo-limit is 80k, which is way too small in a large file if I accidentally `C-x C-p RET'
  ;; (mark page, replace all with an empty line) instead of `C-x p RET' (set BookmarkPlus bookmark).
  ;; Happens with source code way too often, for any project that happens to have large modules.
  ;; https://www.reddit.com/r/emacs/comments/90si1c/why_cant_i_have_truly_persistent_undo_on_emacs/
  (setq undo-limit 78643200)
  (setq undo-outer-limit 104857600)
  (setq undo-strong-limit 157286400)

  ;; Custom hotkeys that make no sense to define in keymap.el.
  ;; Make C-x 8 l insert λ (useful with Racket lambda)
  (define-key 'iso-transl-ctl-x-8-map "l" [?λ])  ; automatically gives sensible display label for which-key
  ;; but can be done manually like this
  ;; (global-set-key (kbd "C-x 8 l") [?λ])
  ;; (which-key-add-key-based-replacements "C-x 8 l" "λ")
  (cua-mode)  ; standard cut, copy, paste hotkeys, also delete region on highlight & insert

  ;; This hooks into spacemacs/comment-or-uncomment-lines.
  (evilnc-toggle-comment-empty-lines)  ; this should load evil-nerd-commenter if not loaded already
  (setq comment-empty-lines t)         ; and then make sure it's on

  ;; Use pdf-tools as default viewer for pdflatex PDF output.
  ;; Bind <f5> in latex-mode to recompile the document and autorefresh the view.
  ;; https://emacs.stackexchange.com/questions/19472/how-to-let-auctex-open-pdf-with-pdf-tools
  ;; https://tex.stackexchange.com/questions/359924/rebinding-key-in-auctex-which-takes-other-argument
  (with-eval-after-load "latex"
    (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
          TeX-source-correlate-start-server t)
    ;; Update PDF buffers after successful LaTeX runs
    (add-hook 'TeX-after-compilation-finished-functions
              #'TeX-revert-document-buffer)
    (define-key LaTeX-mode-map (kbd "<f5>") 'my-default-TeX-command))

  ;;(add-hook 'latex-mode-hook #'outline-minor-mode)  ; maybe not needed, we have reftex

  ;; Use dark view mode in pdf-tools by default.
  ;; http://babbagefiles.blogspot.com/2017/11/more-pdf-tools-tricks.html
  (with-eval-after-load "pdf-tools"
    (add-hook 'pdf-view-mode-hook (lambda ()
                                    (pdf-view-midnight-minor-mode))))

  ;; Fix keymap conflict.
  ;; Synosaurus eats `C-c C-s', so rebind the org entry scheduler to `C-c s'.
  ;; TODO: Switch these, org-schedule is needed more often than synosaurus.
  ;; TODO: The problem is, synosaurus makes C-c C-s into a prefix key, so we should rebind that
  ;; TODO: first and "un-make" C-c C-s being a prefix key so we can then rebind it as a regular key.
  (with-eval-after-load "org"
    (define-key org-mode-map (kbd "C-c s") 'org-schedule))

  ;; Bind <f5> in python-mode to run buffer, like in racket-mode.
  (with-eval-after-load "python"
    (define-key python-mode-map (kbd "<f5>") 'run-buffer-in-python))

  ;; Make helm-top auto-refresh its view.
  ;; https://ambrevar.xyz/emacs-eshell/
  (with-eval-after-load "helm"
    (helm-top-poll-mode))

  ;; Spell checking for English and Finnish.
  ;; http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html
  (setq ispell-local-dictionary "english")
  (setq ispell-local-dictionary-alist
        '(("english" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)  ;; default to en_US
          ("american" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)
          ("british" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_UK") nil utf-8)
          ("finnish" "[[:alpha:]]" "[^[:alpha:]]" "['-]" t ("-d" "fi") nil utf-8)))
  ;; TODO fix enchant on Windows/MSYS2 (builds fine with aspell and voikko, but fails to find any dictionaries)
  (setq ispell-program-name (if my-on-winnt "c:/msys64/usr/bin/aspell" "~/.local/bin/enchant"))

  ;; Set up org global TODO locations.
  ;; https://orgmode.org/worg/org-tutorials/orgtutorial_dto.html
  (if (or my-on-winnt my-on-wsl)
      (setq org-agenda-files (list "~/org/todo.org"))
      (setq org-agenda-files (list "~/org/personal.org" "~/org/home.org" "~/org/work.org")))

  ;; Set up calendar locale and coordinates to compute sunrise/sunset.
  ;; Bind RET to insert date under point.
  ;;https://www.emacswiki.org/emacs/CalendarLocalization
  (setq calendar-week-start-day 1)
  (setq calendar-mark-holidays-flag t)
  (setq calendar-latitude +61.5)
  (setq calendar-longitude +23.8)
  (setq calendar-location-name "Tampere, Finland")
  (with-eval-after-load "calendar"
    (define-key calendar-mode-map (kbd "RET") 'calendar-insert-date))
  (require 'suomalainen-kalenteri)  ; Finnish holidays

  ;; Set up BookmarkPlus
  (require 'bookmark+)
  (setq bmkp-autoname-format "^%B:[0-9]+,[0-9]+.*")  ; see my-auto-l+c-name in funcs.el

  ;; Set up minor mode diminishizers
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

  ;; minimap
  ;; https://github.com/zk-phi/sublimity
  (require 'sublimity)
  ;;(require 'sublimity-scroll)
  (require 'sublimity-map)
  (sublimity-map-set-delay nil)
  ;; (setq sublimity-scroll-weight 3
  ;;       sublimity-scroll-drift-length 3)
  ;; (require 'sublimity-attractive)

  ;; Set up Python support for company autocompletion.
  (eval-after-load "company"
    '(add-to-list 'company-backends 'company-anaconda))
  (add-hook 'python-mode-hook 'anaconda-mode)

  ;; Add thesaurus to all text-modes.
  (add-hook 'text-mode-hook 'synosaurus-mode)
  (setq synosaurus-choose-method 'popup)

  ;; Enable smartparens also when writing text files. (TODO: maybe just use the Spacemacs global toggle for this?)
  (add-hook 'text-mode-hook 'smartparens-mode)
  (add-hook 'web-mode-hook 'smartparens-mode)

  ;; Set up completion from text already in open buffers in all text-modes.
  ;; We do this by adding company-dabbrev to company-backends in text-mode.
  (eval-after-load "company"
    '(add-hook 'text-mode-hook 'my-company-text-mode-hook))
  (defun my-company-text-mode-hook ()
    "Enable some completers for Company in Text mode, and enable company-mode."
    (add-to-list 'company-backends 'company-dabbrev)
    (company-mode))

  ;; Use the MWIM package for smart home/end in visual-line-mode.
  (defun setup-mwim-keys ()
    "Set up smart home/end using the MWIM package."
    (define-key visual-line-mode-map (kbd "<home>") 'mwim-beginning-of-code-or-line)
    (define-key visual-line-mode-map (kbd "C-a") 'mwim-beginning-of-code-or-line)
    (define-key visual-line-mode-map (kbd "<end>") 'mwim-end-of-code-or-line)
    (define-key visual-line-mode-map (kbd "C-e") 'mwim-end-of-code-or-line)
    )
  (add-hook 'visual-line-mode-hook 'setup-mwim-keys)

  ;; Always use visual-line-mode.
  (global-visual-line-mode t)

  ;; Columns are one of the few things that are nice to index 1-based.
  (spacemacs/toggle-zero-based-column-indexing-off)

  ;; Improve the UX for programming.
  (add-hook 'prog-mode-hook 'yafolding-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)  ; spellcheck comments and strings.
  ;; TODO: disable which-function-mode for LaTeX, not so reliable for random LaTeX documents.
  (add-hook 'prog-mode-hook 'which-function-mode)

  ;(add-hook 'prog-mode-hook 'rainbow-mode)  ; display color specifications (e.g. #A0A080, green) using the color they describe
  ;(add-hook 'text-mode-hook 'rainbow-mode)

  ;; Improve Org mode math UX.
  (add-hook 'org-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'org-mode-hook #'highlight-parentheses-mode)

  ;; Improve the UX of the Lisp minibuffer.
  ;; TODO: how to add multiple modes in the same call?
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'smartparens-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'show-smartparens-mode)
  ;; (add-hook 'eval-expression-minibuffer-setup-hook #'rainbow-delimiters-mode)  ; does nothing in minibuffer
  ;; (add-hook 'eval-expression-minibuffer-setup-hook #'highlight-parentheses-mode)  ; does nothing in minibuffer

  ;; These would be mainly useful in fullscreen mode.
  ;; (fancy-battery-mode)
  ;; https://www.emacswiki.org/emacs/DisplayTime
  ;; (display-time-mode)
  ;; (setq display-time-24hr-format t)
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Fill-Commands.html

  ;; For fill-paragraph. Use European style (single space after full stop).
  (setq sentence-end-double-space nil)

  ;; Use AHS.
  (spacemacs/toggle-automatic-symbol-highlight-on)  ; in addition must configure colors for ahs (see customs)

  ;; Highlight cursor after scroll, so we never lose it.
  (beacon-mode t)

  ;; Configure "vim-style scrolloff" i.e. gradual scrolling.
  ;; https://wolfecub.github.io/dotfiles/
  (setq scroll-margin 10
        scroll-step 1
        scroll-conservatively 10000
        scroll-preserve-screen-position 1)

  ;; https://wolfecub.github.io/dotfiles/
  (setq inhibit-compacting-font-caches t)

 ;; Combo with the .bashrc "alias em='emacsclient -c'"; use `C-x #' (M-x server-edit) to close a file.
  (when my-on-winnt
    (server-start))

  ;; Open web links in default browser in Windows when running on WSL.
  ;; https://www.reddit.com/r/bashonubuntuonwindows/comments/70i8aa/making_emacs_on_wsl_open_links_in_windows_web/
  (when my-on-wsl
    (setq
     cmdExeBin"/mnt/c/Windows/System32/cmd.exe"
     cmdExeArgs '("/c" "start" "") )
    (setq
     browse-url-generic-program  cmdExeBin
     browse-url-generic-args     cmdExeArgs
     browse-url-browser-function 'browse-url-generic))
)

(my/user-config)
