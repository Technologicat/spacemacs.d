;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;
;; Custom compatibility hacks.

(defun my/workarounds ()
  "Custom compatibility hacks. Loaded from `dotspacemacs/user-config' in `init.el'."

  ;; Fix default font.
  ;; https://github.com/syl20bnr/spacemacs/issues/3477
  (set-face-attribute 'default nil :family my-default-font)
  (set-face-attribute 'default nil :height 100)

  ;; Fix tab completion in Helm when Treemacs is open (workaround by Simon Bourne).
  ;; https://github.com/syl20bnr/spacemacs/issues/7446#issuecomment-417334718
  ;;   (with-eval-after-load "helm"
  ;;     (defun helm-persistent-action-display-window (&optional split-onewindow)
  ;;       "Return the window that will be used for persistent action.
  ;; If SPLIT-ONEWINDOW is non-`nil' window is split in persistent action."
  ;;       (with-helm-window
  ;;         (setq helm-persistent-action-display-window (get-mru-window)))))

  ;; Fix compatibility issue between phi-search and multiple-cursors.
  ;; https://github.com/zk-phi/phi-search/issues/53
  (require 'mc-extras)
  (with-eval-after-load "phi-search"
    (defadvice phi-search-complete (after phi-search-auto-dedup last activate protect)
      (mc/remove-duplicated-cursors)))

  ;; Make M-. (look up definition) work in C/C++ projects
  ;; TODO: should maybe update the tags file when it goes out of date?
  ;; https://www.emacswiki.org/emacs/TagsFile
  (defadvice xref-find-definitions (before c-tag-file activate)
    "Automatically create tags file."
    (when (eq major-mode 'c-mode)
      (let ((tag-file (concat default-directory "TAGS")))
        (unless (file-exists-p tag-file)
          (shell-command "etags *.[ch] *.cpp -o TAGS 2>/dev/null"))
        (visit-tags-table tag-file))))

  ;; Invoking evil-avy-goto-char-timer crashes auto-highlight-symbol-mode when
  ;; cursor is on a symbol, so switch it off and back on when we do that.
  (defvar my-had-ahs nil "Internal variable for avy/ahs workaround.")
  (defadvice evil-avy-goto-char-timer (before avy-disable-ahs last activate)
    (setq my-had-ahs auto-highlight-symbol-mode)
    (when my-had-ahs
      (auto-highlight-symbol-mode 0)))
  (defadvice evil-avy-goto-char-timer (after avy-reenable-ahs first activate)
    (when my-had-ahs
      (auto-highlight-symbol-mode 1)))

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
)

(my/workarounds)
