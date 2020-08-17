;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;
;; Set up prettify-symbols-mode.

(defun my/prettifier-init ()
  "Custom settings for prettify-symbols-mode. Loaded from `dotspacemacs/user-config' in `init.el'."

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

  (defun my/prettify-js-setup ()
     "Set up symbol prettification (additional settings for JavaScript)."
     (let ((xs '(("function" . ?λ)
                 ;; ES7  https://mariusschulz.com/blog/the-exponentiation-operator-in-javascript
                 ("**2" . ?²)
                 ("**3" . ?³)
                 ("**4" . ?⁴)
                 ("**5" . ?⁵)
                 ("**6" . ?⁶)
                 ("**7" . ?⁷)
                 ("**8" . ?⁸)
                 ("**9" . ?⁹)
                 ("**-1" . (?⁻ (Br . Bl) ?¹))  ; ⁻¹
                 ("**-2" . (?⁻ (Br . Bl) ?²))  ; ⁻²
                 ("**-3" . (?⁻ (Br . Bl) ?³))  ; ⁻³
                 ("**-4" . (?⁻ (Br . Bl) ?⁴))  ; ⁻⁴
                 ("**-5" . (?⁻ (Br . Bl) ?⁵))  ; ⁻⁵
                 ("**-6" . (?⁻ (Br . Bl) ?⁶))  ; ⁻⁶
                 ("**-7" . (?⁻ (Br . Bl) ?⁷))  ; ⁻⁷
                 ("**-8" . (?⁻ (Br . Bl) ?⁸))  ; ⁻⁸
                 ("**-9" . (?⁻ (Br . Bl) ?⁹))  ; ⁻⁹
                 ("&&" . ?∩)
                 ("||" . ?∪)
                 ("!" . ?¬)
                 ("===" . ?≡)
                 ("!==" . ?≢)
                 ("==" . ?＝)
                 ("=" . ?←)
                 ("all" . ?∀)  ; custom
                 ("any" . ?∃)  ; custom
                 ("undefined" . ?∅)
                 ("return" . ?➡)
                 ("Infinity" . ?∞)
                 ;("def" . ?ƒ)  ; looks silly; literal "def" easier to spot.
                 )))
       (dolist (x xs nil)
         (push x prettify-symbols-alist)))
     (prettify-symbols-mode))

  (defun my/prettify-julia-setup ()
     "Set up symbol prettification (additional settings for Julia)."
     (let ((xs '(("function" . ?λ)
                 ("begin". ?⌜)
                 ("end" . ?⌟)
                 ("^2" . ?²)
                 ("^3" . ?³)
                 ("^4" . ?⁴)
                 ("^5" . ?⁵)
                 ("^6" . ?⁶)
                 ("^7" . ?⁷)
                 ("^8" . ?⁸)
                 ("^9" . ?⁹)
                 ("^-1" . (?⁻ (Br . Bl) ?¹))  ; ⁻¹
                 ("^-2" . (?⁻ (Br . Bl) ?²))  ; ⁻²
                 ("^-3" . (?⁻ (Br . Bl) ?³))  ; ⁻³
                 ("^-4" . (?⁻ (Br . Bl) ?⁴))  ; ⁻⁴
                 ("^-5" . (?⁻ (Br . Bl) ?⁵))  ; ⁻⁵
                 ("^-6" . (?⁻ (Br . Bl) ?⁶))  ; ⁻⁶
                 ("^-7" . (?⁻ (Br . Bl) ?⁷))  ; ⁻⁷
                 ("^-8" . (?⁻ (Br . Bl) ?⁸))  ; ⁻⁸
                 ("^-9" . (?⁻ (Br . Bl) ?⁹))  ; ⁻⁹
                 ("sum" . ?∑)
                 ("prod" . ?∏)
                 ("&&" . ?∩)
                 ("||" . ?∪)
                 ("!" . ?¬)
                 ("==" . ?＝)
                 ("===" . ?≡)  ; egal operator
                 ("!==" . ?≢)  ; not-egal operator
                 ("in" . ?∈)
                 ;; Julia's "not in" has no bindings other than unicode ∉ (\notin) and ∌ (\nni)
                 ("=" . ?←)
                 ("nothing" . ?∅)
                 ("return" . ?➡)
                 ("Inf" . ?∞)
                 ("sqrt" . ?√)
                 ("all" . ?∀)
                 ("any" . ?∃)
                 ;; Some builtin types
                 ("Integer" . ?ℤ)
                 ("Rational" . ?ℚ)
                 ("Real" . ?ℝ)
                 ("Complex" . ?ℂ)
                 ("Bool" . (?ℤ (Br . Bl) ?₂))  ; ℤ₂ ...maybe bad taste?
                 ("Function" . ?λ)
                 ("Nothing" . ?∅)  ; singleton type containing the value `nothing'
                 ("Any" . ?⊤)  ; top type, down tack symbol
                 ; in Julia, the bottom type is `Union{}', which is not a lone symbol, so we can't convert that.
                 ;; Greek symbols for mathematical usage
                 ("alpha" . ?α)
                 ("beta" . ?β)
                 ("gamma" . ?γ)
                 ("delta" . ?δ)
                 ("epsilon" . ?ε)
                 ("zeta" . ?ζ)
                 ("eta" . ?η)
                 ("theta" . ?θ)
                 ("iota" . ?ι)
                 ("kappa" . ?κ)
                 ("lambda" . ?λ)
                 ("mu" . ?μ)
                 ("nu" . ?ν)
                 ("xi" . ?ξ)
                 ("pi" . ?π)
                 ("rho" . ?ρ)
                 ("sigma" . ?σ)
                 ("tau" . ?τ)
                 ("upsilon" . ?υ)
                 ("phi" . ?φ)
                 ("chi" . ?χ)
                 ("psi" . ?ψ)
                 ("omega" . ?ω)
                 ("Gamma" . ?Γ)
                 ("Delta" . ?Δ)
                 ("Theta" . ?Θ)
                 ("Lambda" . ?Λ)
                 ("Xi" . ?Ξ)
                 ("Pi" . ?Π)
                 ("Sigma" . ?Σ)
                 ("Phi" . ?Φ)
                 ("Psi" . ?Ψ)
                 ("Omega" . ?Ω)
                 )))
       (dolist (x xs nil)
         (push x prettify-symbols-alist)))
     (prettify-symbols-mode))

  (add-hook 'prog-mode-hook 'my/prettify-symbols-setup)
  (add-hook 'python-mode-hook 'my/prettify-python-setup)
  (add-hook 'js-mode-hook 'my/prettify-js-setup)
  (add-hook 'julia-mode-hook 'my/prettify-julia-setup)
  (global-prettify-symbols-mode)

  ;; TODO: takes effect from the **second** Python file opened. Figure out why. (Maybe font-lock for python mode scans the file before the hooks run?)
  (defun my/unpythonic-syntax-highlight-setup ()
    "Set up additional syntax highlighting for `unpythonic.syntax' and `macropy3' in python mode."
    ;; adapted from code in dash.el
    (let ((new-keywords '("test" "test_raises" "test_signals" "fail" "error"
                          "signal" "handlers" "restarts" ; not macros, but in a role similar to exception handling constructs in the conditions/restarts system.
                          "let" "dlet" "blet"
                          "letseq" "dletseq" "bletseq"
                          "letrec" "dletrec" "bletrec"
                          "let_syntax" "abbrev"
                          "where"
                          "do" "local" "delete"
                          "continuations" "call_cc"
                          "curry" "lazify" "envify" "tco" "prefix" "autoreturn" "forall"
                          "multilambda" "namedlambda" "quicklambda"
                          "cond" "aif" "autoref" "dbg" "nb"
                          "macros" "q" "u" "hq" "ast_literal")) ; macropy3
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
)

(my/prettifier-init)
