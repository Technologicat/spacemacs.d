;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;
;; Global keymap customizations: custom-keys-minor-mode. Loaded from init.el.
;;
;; !!! Load funcs.el first, it will define some functions that get keybindings here. !!!

(defun my/keymap-init ()
  "Global keymap customizations: custom-keys-minor-mode. Loaded from init.el."
  ;; Our custom isearch functions (that work around phi-search's lack of pdf-tools support)
  ;; need the var phi-search--active to be defined, which it isn't until phi-search loads.
  (require 'phi-search)
  (custom-keys-minor-mode 1))

;; pdf-tools doesn't support helm-swoop or phi-search in PDF text, only the builtin isearch.
;; http://pragmaticemacs.com/emacs/view-and-annotate-pdfs-in-emacs-with-pdf-tools/
;; TODO: would be much cleaner to switch a "phi-search-mode" on/off depending on major mode. Investigate.
(defun my-isearch-forward (&rest args)
  "phi-search or isearch as appropriate."
  (interactive "P")
  (cond ((eq major-mode 'pdf-view-mode) (apply 'isearch-forward args))
        ((and (eq major-mode 'term-mode) (term-in-char-mode)) (term-send-raw-string ""))
        (t (if phi-search--active  ;; FIXME: accessing implementation detail
               (phi-search-next)
               (apply 'phi-search args)))))

(defun my-isearch-backward (&rest args)
  "phi-search-backward or isearch-backward as appropriate."
  (interactive "P")
  (cond ((eq major-mode 'pdf-view-mode) (apply 'isearch-backward args))
        ((and (eq major-mode 'term-mode) (term-in-char-mode)) (term-send-raw-string ""))
        (t (if phi-search--active  ;; FIXME: accessing implementation detail
               (phi-search-previous)
               (apply 'phi-search-backward args)))))

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
    (define-key map (kbd "C-c e") 'eval-and-replace-sexp)
    (define-key map (kbd "C-<next>") 'switch-to-next-file-repeatable)
    (define-key map (kbd "C-<prior>") 'switch-to-previous-file-repeatable)
    (define-key map (kbd "M-S-q") 'unfill-paragraph)
    (define-key map (kbd "M-Q") 'unfill-paragraph)
    (define-key map (kbd "s-q") 'fill-paragraph-at-this-width)
    (define-key map (kbd "C-M-S-q") 'fill-paragraph-at-this-width)
    (define-key map (kbd "<f6>") 'iedit-mode)
    (define-key map (kbd "<f7>") 'org-agenda)
    (define-key map (kbd "<f8>") 'my-flyspell-correct)
    (define-key map (kbd "S-<f8>") 'my-flyspell-correct-unlucky)
    (define-key map (kbd "C-<f8>") 'my-flyspell-correct-lucky)
    (define-key map (kbd "<f9>") 'toggle-minimap)
    (define-key map (kbd "S-<f12>") 'yafolding-go-parent-element)
    (define-key map (kbd "<f12>") 'yafolding-toggle-element)
    (define-key map (kbd "S-<backspace>") 'delete-indentation)
    (define-key map (kbd "S-<return>") 'newline-and-indent-relative)
    (define-key map (kbd "C-x 8 RET") 'counsel-unicode-char)  ; with previews!
    ;; Replace EMACS's default sexp navigation keybindings with smartparens-enabled ones
    ;; https://www.emacswiki.org/emacs/NavigatingParentheses#toc7
    ;; https://github.com/Fuco1/smartparens  (works also with Python!)
    (define-key map (kbd "C-M-S-u") 'sp-unwrap-sexp)
    (define-key map (kbd "s-u") 'sp-unwrap-sexp)  ; some Linuxen (Mint 19) reserve C-M-S-u for "insert unicode char by number"
    (define-key map (kbd "C-M-S-r") 'sp-rewrap-sexp)
    (define-key map (kbd "s-r") 'sp-rewrap-sexp)  ; some Linuxen (Mint 19 w/ Cinnamon) reserve C-M-S-r for "toggle desktop recording"
    (define-key map (kbd "C-M-S-s") 'sp-slurp-hybrid-sexp)
    (define-key map (kbd "C-M-S-b") 'sp-forward-barf-sexp)
    (define-key map (kbd "C-M-S-a") 'sp-backward-slurp-sexp)  ; one key back from slurp
    (define-key map (kbd "C-M-S-v") 'sp-backward-barf-sexp)  ; one key back from barf
    (define-key map (kbd "C-M-f") 'sp-forward-sexp)
    (define-key map (kbd "C-M-b") 'sp-backward-sexp)
    (define-key map (kbd "C-M-d") 'sp-down-sexp)
    (define-key map (kbd "C-M-a") 'sp-backward-down-sexp)  ; "ackward" down
    (define-key map (kbd "C-M-e") 'sp-up-sexp)  ; "exit"
    (define-key map (kbd "C-M-u") 'sp-backward-up-sexp)
    (define-key map (kbd "C-M-n") 'sp-next-sexp)
    (define-key map (kbd "C-M-p") 'sp-previous-sexp)
    (define-key map (kbd "C-M-k") 'sp-kill-sexp)
    (define-key map (kbd "C-M-SPC") 'sp-mark-sexp)
    (define-key map (kbd "<menu>") 'helm-M-x)  ; shadow the binding to default execute-extended-command
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
        (define-key evil-emacs-state-map (kbd "C-z") nil)                       ; prevent shadowing "C-z" undo-tree-undo in custom map
        (spacemacs/set-leader-keys "s M" 'helm-multi-swoop-this-mode)           ; M-m s M, because M-m s m is taken by multiple-cursors
        (spacemacs/set-leader-keys "s C-p" 'helm-multi-swoop-projectile)        ; M-m s C-p (NOTE: searches open project buffers only)
        (spacemacs/set-leader-keys-for-major-mode 'latex-mode "O" 'reftex-toc)  ; M-m m O, M-RET O (same as in Spacemacs pdf layer)
        (spacemacs/set-leader-keys "M-j" 'evil-avy-goto-char-timer)             ; M-m M-j, less repetitive than default M-m j j
        (spacemacs/set-leader-keys "j g" 'dumb-jump-go)                         ; M-m j g
        )
      (progn  ; off
        (define-key evil-emacs-state-map (kbd "C-z") 'evil-exit-emacs-state)
        (spacemacs/set-leader-keys "s M" nil)
        (spacemacs/set-leader-keys "s C-p" nil)
        (spacemacs/set-leader-keys-for-major-mode 'latex-mode "O" nil)
        (spacemacs/set-leader-keys "M-j" nil)
        (spacemacs/set-leader-keys "j g" nil)
        )))

(my/keymap-init)
