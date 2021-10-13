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
    (define-key org-mode-map (kbd "C-c s") 'org-schedule)
    (define-key org-mode-map (kbd "<f5>") 'org-latex-export-to-pdf))

  ;; Bind <f5> in python-mode to run buffer, like in racket-mode.
  (with-eval-after-load "python"
    (define-key python-mode-map (kbd "<f5>") 'run-buffer-in-python))

  ;; ;; https://emacs.stackexchange.com/questions/328/how-to-override-keybindings-for-term
  ;; ;; https://stackoverflow.com/questions/4187117/emacs-how-add-custom-key-bindings-to-be-used-with-char-mode-of-ansi-term
  ;;(with-eval-after-load "term"
  ;; These don't help, because custom-keys-minor-mode-map has higher precedence.
  ;;  (define-key term-raw-map (kbd "<home>") 'term-send-home)
  ;;  (define-key term-raw-map (kbd "<end>") 'term-send-end)
  ;;  (define-key term-raw-map (kbd "C-r") 'my-term-send-ctrl-r)
  ;;  (define-key term-raw-map (kbd "C-s") 'my-term-send-ctrl-s))

  ;; Make helm-top auto-refresh its view.
  ;; https://ambrevar.xyz/emacs-eshell/
  (with-eval-after-load "helm"
    (helm-top-poll-mode))

  ;; Spell checking for English and Finnish.
  ;; http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html
  ;; See also Emacs help on `ispell-dictionary-alist', which explains the format.
  (setq ispell-local-dictionary "english")
  (setq ispell-local-dictionary-alist
        '(("english" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)  ;; default to en_US
          ("american" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)
          ("british" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_UK") nil utf-8)
          ("finnish" "[[:alpha:]]" "[^[:alpha:]]" "[':]" t ("-d" "fi") nil utf-8)))  ; otherchars: e.g. "raa'at", "IT:ssä"
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
  (setq calendar-intermonth-text  ; show week numbers; from help for `calendar-intermonth-text'
        '(propertize
          (format "%2d"
                  (car
                   (calendar-iso-from-absolute
                    (calendar-absolute-from-gregorian (list month day year)))))
          'font-lock-face 'font-lock-function-name-face))
  (with-eval-after-load "calendar"
    (define-key calendar-mode-map (kbd "RET") 'calendar-insert-date))
  (require 'suomalainen-kalenteri)  ; Finnish holidays

  ;; Set up BookmarkPlus
  (require 'bookmark+)
  (setq bmkp-autoname-format "^%B:[0-9]+,[0-9]+.*")  ; see my-auto-l+c-name in funcs.el
  (setq bookmark-save-flag 10)  ; https://help-gnu-emacs.gnu.narkive.com/qR72biV2/bookmark-veeery-slow

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

  (require 'py-autopep8)
  (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
  ; isort breaks macropythonic code, so disable it for now.
  ;(add-hook 'before-save-hook 'py-isort-before-save)  ; it checks we're in `python-mode' before doing anything.
  ;; Work around crash in py-autopep8.el
  ;; https://github.com/emacs-lsp/lsp-mode/issues/711
  (setq lsp-json-use-lists t)

  ;; Add thesaurus to all text-modes.
  (add-hook 'text-mode-hook 'synosaurus-mode)
  (setq synosaurus-choose-method 'popup)

  ;; Enable smartparens also when writing text files. (TODO: maybe just use the Spacemacs global toggle for this?)
  (add-hook 'text-mode-hook 'smartparens-mode)
  ;; web-mode-hook includes spacemacs/toggle-smartparens-off by default. Try to push it after that.
  (add-hook 'web-mode-hook 'smartparens-mode 50)

  (add-hook 'term-mode-hook 'highlight-parentheses-mode)
  (add-hook 'term-mode-hook 'rainbow-delimiters-mode)

  ;; Set up completion from text already in open buffers in all text-modes.
  ;; We do this by adding company-dabbrev to company-backends in text-mode.
  (eval-after-load "company"
    '(add-hook 'text-mode-hook 'my-company-text-mode-hook))
  (defun my-company-text-mode-hook ()
    "Enable some completers for Company in Text mode, and enable company-mode."
    (add-to-list 'company-backends 'company-dabbrev)
    (company-mode))

  ;; Use the MWIM package for smart home/end in visual-line-mode.
  (with-eval-after-load "mwim"
    (defun my/where-is-beginning-of-line ()
      "Smart HOME for MWIM.

If point is on a continuation line of a visual line, then return
`mwim-beginning-of-code' for the first line of that visual line.

If point is on the first line of a visual line, return NIL.

This is useful as a `mwim-beginning-position-function' for the MWIM package."
      (let* ((continued-line-beg (mwim-point-at (beginning-of-line)))
             (visual-line-beg (mwim-point-at (beginning-of-visual-line)))
             (on-first-line-of-visual-line (eq continued-line-beg visual-line-beg)))
        (if (not on-first-line-of-visual-line)
            (mwim-point-at (goto-char continued-line-beg) (mwim-beginning-of-code))
          nil)))
    (defun my/where-is-end-of-line ()
      "Return POSITION at the end of the current logical line."
      (mwim-point-at (end-of-line))))
  (defun my/smart-beginning (&rest args)
    "Call mwim-beginning, except in term char mode; then term-send-home."
    ;; Functions like home/end that need shift-translation must use "^P", not just "P".
    ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Interactive-Codes.html
    (interactive "^P")
    (if (and (eq major-mode 'term-mode) (term-in-char-mode))
        ;; Doesn't take args
        ;; https://github.com/emacs-mirror/emacs/blob/master/lisp/term.el
        (term-send-home)
      (apply #'mwim-beginning args)))
  (defun my/smart-end (&rest args)
    "Call mwim-end, except in term char mode; then term-send-end."
    (interactive "^P")
    (if (and (eq major-mode 'term-mode) (term-in-char-mode))
        (term-send-end)
      (apply #'mwim-end args)))
  (defun setup-mwim-keys ()
    "Set up smart home/end using the MWIM package."
    (define-key visual-line-mode-map (kbd "<home>") 'my/smart-beginning)
    (define-key visual-line-mode-map (kbd "C-a") 'my/smart-beginning)
    (define-key visual-line-mode-map (kbd "<end>") 'my/smart-end)
    (define-key visual-line-mode-map (kbd "C-e") 'my/smart-end)
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

  (with-eval-after-load "js2-mode"
    (setq js2-mode-show-parse-errors nil
          js2-mode-show-strict-warnings nil)
    ;; https://github.com/bbatsov/prelude/blob/master/modules/prelude-js.el
    ;; electric-layout-mode doesn't play nice with smartparens
    (defun my-js-hook ()
      (setq-local electric-layout-rules '((?\; . after))))
    (add-hook 'js2-mode-hook #'my-js-hook))

  ;; Improve org-mode and inferior-python-mode math UX.
  (add-hook 'org-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'org-mode-hook #'highlight-parentheses-mode)
  (add-hook 'org-mode-hook #'org-cdlatex-mode)
  ;; Disable <> as parens in org-mode, because otherwise writing inequalities in
  ;; math mode confuses the parentheses highlighting minor modes.
  ;; https://sites.google.com/site/xiangyangsite/home/technical-tips/linux-unix/emacs/parenthesis-matching-in-emacs
  ;; https://www.emacswiki.org/emacs/EmacsSyntaxTable
  ;; https://stackoverflow.com/questions/41681167/modify-syntax-entry-is-not-loaded-in-emacs-init
  (add-hook 'org-mode-hook (lambda ()
                             (modify-syntax-entry ?< "w" org-mode-syntax-table)
                             (modify-syntax-entry ?> "w" org-mode-syntax-table)))
  (add-hook 'inferior-python-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'inferior-python-mode-hook #'highlight-parentheses-mode)

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
     cmdExeBin "/mnt/c/Windows/System32/cmd.exe"
     cmdExeArgs '("/c" "start" "") )
    (setq
     browse-url-generic-program  cmdExeBin
     browse-url-generic-args     cmdExeArgs
     browse-url-browser-function 'browse-url-generic))
)

(my/user-config)
