;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;
;; Top-level storage location for recorded keyboard macros.

(defun my/recorded-keyboard-macros ()
  "Set up recorded keyboard macros. Loaded from `dotspacemacs/user-config' in `init.el'."
  ;; Recorded keyboard macros
  ;; see commands kmacro-name-last-macro (name-last-kbd-macro) and insert-kbd-macro
  (fset 'sort-lines-and-save
        (kmacro-lambda-form [?í ?x ?l ?s ?í ?f ?s] 0 "%d"))
  (fset 'unparenthesize-python-return-stmt
        [?\C-s ?r ?e ?t ?u ?r ?n ?\( return left ?  ?\M-x ?s ?p ?- ?u ?n tab return])
  (fset 'fill-paragraph-at-this-width
        (kmacro-lambda-form [?\C-x ?f return ?\M-q] 0 "%d"))
)

(my/recorded-keyboard-macros)
