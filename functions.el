;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;
;; Top-level file for custom elisp functions. Loaded from init.el.

;; https://www.masteringemacs.org/article/searching-buffers-occur-mode
;;
;; with-current-buffer is slow; use buffer-local-value instead; see:
;; https://alphapapa.github.io/emacs-package-dev-handbook/
;;
;; helm-swoop.el already defines this and automatically invokes buffer-name on each result.
;; (defun get-buffers-matching-mode (mode)
;;   "Returns a list of buffers where their major-mode is equal to MODE"
;;   (let ((buffer-mode-matches '()))
;;     (dolist (buf (buffer-list))
;;       (with-current-buffer buf
;;         (if (eq mode major-mode)
;;             (add-to-list 'buffer-mode-matches buf))))
;;     buffer-mode-matches))
;;
;; (defun multi-occur-this-mode ()
;;   "Show all lines matching REGEXP in buffers with this major mode."
;;   (interactive)
;;   (multi-occur
;;    (get-buffers-matching-mode major-mode)
;;    (car (occur-read-primary-args))))

;; For an example how to do things like this with helm-swoop, see helm-multi-swoop-projectile
;; TODO: make a dynamic-let macro for temporarily changing values of special variables (or try harder to find the standard approach on the internet)
;; TODO: just use a regular let here, Elisp should notice helm-swoop-prompt is a special variable and dynamically let-assign it.
(defun helm-multi-swoop-this-mode (&optional $query)
  "Helm-swoop in buffers matching current major mode."
  (interactive)
  (let (($p helm-swoop-prompt))
    (unwind-protect
        (progn
          ;; change the **dynamic** binding with (set 'var val) so that helm-swoop sees it, if we (setq var val) it changes the lexical binding
          ;; https://emacs.stackexchange.com/questions/27581/why-do-setq-and-set-quote-act-differently-on-let-bound-variables-with-lexical-sc
          (set 'helm-swoop-prompt "Swoop (in current major mode):")
          (setq helm-multi-swoop-query (helm-multi-swoop--get-query $query))
          (helm-multi-swoop--exec nil
                                  :$query helm-multi-swoop-query
                                  :$buflist (get-buffers-matching-mode major-mode)))
        (set 'helm-swoop-prompt $p))))

;; https://emacs.stackexchange.com/questions/7742/what-is-the-easiest-way-to-open-the-folder-containing-the-current-file-by-the-de
;; need xsel under Linux; xclip has some problem when copying under Linux
(defun copy-yank-str (msg &optional clipboard-only)
  (unless clipboard-only (kill-new msg))
  (cond
   ;; display-graphic-p need windows 23.3.1
   ((and (display-graphic-p) select-enable-clipboard)
    (gui-set-selection 'CLIPBOARD msg))
   (t (with-temp-buffer
        (insert msg)
        (shell-command-on-region (point-min) (point-max)
                                 (cond
                                  ((eq system-type 'cygwin) "putclip")
                                  ((eq system-type 'darwin) "pbcopy")
                                  (t "xsel -ib")))))))

(defun copy-fullpath-of-current-buffer ()
  "Copy full path of current file into the kill ring and OS clipboard."
  (interactive)
  (when buffer-file-name
    (copy-yank-str (file-truename buffer-file-name))
    (message "file full path => kill ring & OS clipboard")))

;; https://www.emacswiki.org/emacs/UnfillParagraph
;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph.
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(defvar dedicated-terminal-command-template "gnome-terminal --working-directory='%s'" "Command used by `dedicated-terminal-f'. Must contain exactly one `\%s', for the path to open the terminal in.")
(defun dedicated-terminal-f (&optional directory)
  "Open a terminal window, setting cwd to directory (default: directory of current buffer)."
  (let (($dir (expand-file-name (if directory directory default-directory))))
    (if (and $dir (file-directory-p $dir))
        (progn
          (message (format "terminal in `%s'" $dir))
          (start-process-shell-command "emacs-dedicated-terminal" nil (format dedicated-terminal-command-template $dir)))
      (message "no such directory `%s'" $dir))))

(defvar dedicated-file-browser-template "/usr/bin/nemo '%s'" "Command used by `browse-file-f'. Must contain exactly one `\%s', for the path to open the file browser in.")
(defun browse-file-f (&optional directory)
  "Open a file browser in directory (default: directory of current buffer)."
  (let (($dir (expand-file-name (if directory directory default-directory))))
    (if (and $dir (file-directory-p $dir))
        (progn
          (message (format "file browser in `%s'" $dir))
          (start-process-shell-command "emacs-dedicated-file-browser" nil (format dedicated-file-browser-template $dir)))
      (message "no such directory `%s'" $dir))))

(defun pyan-f (&optional directory)
  "Run Pyan, the Python static call graphing tool. Visualize results in xdot."
  (interactive)
  (let (($dir (expand-file-name (if directory directory default-directory))))
    (if (and $dir (file-directory-p $dir))
        (progn
          (message (format "Python call graph analysis in `%s'" $dir))
          ;; (start-process-shell-command "pyan" nil (format "cd \"%s\" && shopt -s globstar && pyan3 -nuca --dot **/*.py | xdot" $dir))
          (start-process-shell-command "pyan" nil (format "cd \"%s\" && find . -not -iwholename '*/test*/*' -name '*.py' | xargs pyan3 -nuca --dot | xdot" $dir))
          )
      (message "no such directory `%s'" $dir))))

;; TODO: detect whether projectile is available (using condition-case)
(defun call-in-current-or-project-directory (func in-project-root)
  "Call `func' with one argument, set to the directory of the current buffer or the project root from Projectile.

If `in-project-root' is `nil', use the currect directory. If non-nil, use the project root."
  (if in-project-root
    (let (($dir (projectile-project-root)))
      (if $dir
        (funcall func (expand-file-name $dir))
        (message "not in a project")))
    (if default-directory
        (funcall func (expand-file-name default-directory))
      (message "current buffer not associated with a directory"))))

(defun copy-fullpath-of-current-directory (&optional in-project-root)
  "Show the path of current buffer and copy the path to the kill ring and OS clipboard.

If `in-project-root' is non-nil (can be set interactively by setting the
universal argument), show and copy root path of project (queried from Projectile)."
  (interactive "P")
  (defun $copy-and-msg (directory)
    (copy-yank-str directory)
    (message "`%s'" directory))
  (call-in-current-or-project-directory '$copy-and-msg in-project-root))

(defun browse-file-directory (&optional in-project-root)
  "Open the directory of the current buffer however the OS would.

If `in-project-root' is non-nil (can be set interactively by
setting the universal argument), instead open root directory of
project (queried from Projectile).

On WSL, instead of opening the directory as the OS would, this
calls `browse-file-f', because the Linux filesystem is not
visible to Windows, and so the directories in it cannot be
browsed in Windows Explorer.
"
  (interactive "P")
  (call-in-current-or-project-directory
   (if my-on-wsl 'browse-file-f 'browse-url-of-file)
   in-project-root))

(defun open-dedicated-terminal (&optional in-project-root)
  "Open a system terminal window.

With no argument or `nil', set terminal cwd to directory of current buffer.

If `in-project-root' is non-nil (can be set interactively by setting the
universal argument), set terminal cwd to root directory of project (queried
from Projectile)."
  (interactive "P")
  (call-in-current-or-project-directory 'dedicated-terminal-f in-project-root))

(defun run-pyan (&optional in-project-root)
  "Run Pyan, the Python static call graphing tool. Visualize results in xdot.

With no argument or `nil', set terminal cwd to directory of current buffer.

If `in-project-root' is non-nil (can be set interactively by setting the
universal argument), set terminal cwd to root directory of project (queried
from Projectile)."
  (interactive "P")
  (call-in-current-or-project-directory 'pyan-f in-project-root))

(defun run-buffer-in-python ()
  "Start an inferior-python and send the current buffer to it.

Only has an effect if the current buffer is in the 'python-mode major mode.

Reuses existing python shell if already running.
"
  (interactive)
  (when (eq major-mode 'python-mode)
    (let (($b (current-buffer)))
      (message "Running buffer contents in python shell...")
      (run-python)
      (with-current-buffer $b
        (python-shell-send-buffer 'send-main)))))

;; (defun ansi-term-bash ()
;;   "Run /bin/bash in ansi-term."
;;   (interactive)
;;   (message "bash in ansi-term")
;;   (ansi-term "/bin/bash")
;; )

;; based on https://emacs.wordpress.com/2007/01/17/eval-and-replace-anywhere/
(defun eval-and-replace-sexp ()
  "Replace the preceding sexp with its value.

To support modes that automatically match parens, if point is on a closing
paren, it is first moved forward until it isn't, to eliminate the need
to type (and hence skip over) the closing paren before invoking
eval-and-replace-sexp.

Useful as an inline calculator.
"
  (interactive)
  (while (and (char-after) (char-equal (char-after) ?\)))  ; HACK for convenience in Lisp mode (no need to type closing paren first)
    (forward-char))
  (backward-kill-sexp)
  (condition-case nil
      (progn
        (message "eval & replace sexp, original => kill ring")
        (prin1 (eval (read (current-kill 0)))
               (current-buffer)))
    (error (message "eval-and-replace-sexp: invalid expression")
           (insert (current-kill 0)))))

;; TODO: see if someone has already implemented this
(defun my-make-repeatable-interactive-command (command &optional new-name)
  "Make a repeatable version of an existing interactive command.

COMMAND is the command name as a symbol.

If NEW-NAME (a symbol) is given, globally bind that function
name (at the Lisp level) to the new function, as if the new
function was DEFUN'd with that name.

Return the new command, which behaves exactly as the original,
except that it accepts a numeric prefix argument to specify how
many times to repeat.

Prefix argument semantics for the new command:

    With no prefix argument, run the original command once.

    With the universal prefix argument, run the original command
    once, passing the universal prefix argument to it.
    NOTE: currently more than one `C-u' is not supported.

    If the prefix argument is numeric and positive, repeat the
    original command that many times.

    If the prefix argument is numeric and negative, repeat the
    original command as many times as the absolute value of the
    argument. Each time, pass the universal argument to the
    original command.

    If the prefix argument is numeric and zero, do nothing.
"
  (let (($newcmd (lambda (&optional num)
                   ;; https://emacs.stackexchange.com/questions/44407/how-to-implement-a-numeric-prefix-argument-in-emacs
                   ;; TODO: support several C-u prefixes
                   (interactive (list (if current-prefix-arg
                                          (if (equal current-prefix-arg '(4))  ;; C-u
                                              -1
                                            (prefix-numeric-value current-prefix-arg))
                                        1)))
                   (let (($parg (if (< num 0) '(4) nil))  ;; simulate C-u
                         ($n (abs num)))
                     (dotimes ($k $n nil)  ;; TODO: return the return value from the final repeat
                       (funcall command $parg))))))  ;; TODO: should probably use (call-interactively command), but how to pass the prefix arg then?
    (when new-name
      ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Function-Cells.html#Function-Cells
      (fset new-name $newcmd))
    $newcmd))

(defun switch-to-next-file (&optional backwards)
  "Switch current window to next file.

If `backwards' is non-nil (can be set interactively by setting the
universal argument), switch to previous file.

A buffer is skipped as not representing a file, if:
  - its name begins and ends with `*'
  - its purpose is \"general\", \"dired\" or \"minibuf\"
  - it matches a special case, e.g. a Dired view or the file
    \".custom-settings\", which Spacemacs uses as a staging area
    to update `dotspacemacs/emacs-custom-settings' in init.el
    (so it occasionally automatically opens in the background).
"
  (interactive "P")
  (catch 'stnf-exit
    (let (($seen (make-hash-table))
          ($orig (current-buffer)))
      (defun $next ()
        (if backwards (previous-buffer) (next-buffer))
        (when (gethash (buffer-name) $seen)  ; looped around?
          (message "no files open")
          (switch-to-buffer $orig)
          (throw 'stnf-exit nil))
        (puthash (buffer-name) 1 $seen))
      (defun $rejectp ()
        (let* (($n (buffer-name))
               ($p (purpose-buffer-purpose $n)))
          (and
           (not (s-starts-with? "untitled" $n))
           (or
            (and (s-starts-with? "*" $n) (s-ends-with? "*" $n))
            (s-starts-with? "Dired " $n)
            (s-starts-with? ".custom-settings" $n)
            (equal $p 'general)
            (equal $p 'dired)
            (equal $p 'minibuf)))
          ))
      ($next)
      (while ($rejectp)
        ($next))
    (if (eq (current-buffer) $orig)
      (message "no other files open")
      (message "%s file" (if backwards "previous" "next"))))))

;; this version is not as useful, since (buffer-list) gives the buffers in most-recently-used order.
;; (defun switch-to-next-file (&optional backwards)
;;   "Switch current window to next file.
;;
;; If `backwards' is non-nil (can be set interactively by setting the
;; universal argument), switch to previous file.
;;
;; A buffer is skipped as not representing a file, if:
;;   - its name begins and ends with `*'
;;   - its purpose is `general', `dired' or `minibuf'
;; "
;;   (interactive "P")
;;   (catch 'stnf-exit
;;     (defun $rejectp (buf)
;;       (let (($n (string-trim (buffer-name buf)))
;;             ($p (purpose-buffer-purpose buf)))
;;         (and
;;          (not (s-starts-with? "untitled" $n))
;;          (or
;;           (and (s-starts-with? "*" $n) (s-ends-with? "*" $n))
;;           (s-starts-with? "Dired " $n)
;;           (equal $p 'general)
;;           (equal $p 'dired)
;;           (equal $p 'minibuf)))))
;;     (let (($bufs (cdr (buffer-list (selected-frame)))))  ; current buffer is first, skip it
;;       (when backwards (setq $bufs (reverse $bufs)))
;;       (dolist ($buf $bufs nil)
;;         (when (not ($rejectp $buf))
;;           (message "%s file" (if backwards "previous" "next"))
;;           (switch-to-buffer $buf)
;;           (throw 'stnf-exit nil)))
;;       (message (if ($rejectp (current-buffer)) "no files open" "no other files open"))
;;       )))

(defun switch-to-previous-file (&optional forwards)
  "Switch current window to previous file.

Shorthand for `(switch-to-next-file (not forwards))',
i.e. `(switch-to-next-file t)' when no arg given.

The optional argument is there to support negative numeric args
for `switch-to-previous-file-repeatable'.
"
  (interactive)
  (switch-to-next-file (not forwards)))

(my-make-repeatable-interactive-command 'switch-to-next-file 'switch-to-next-file-repeatable)
(my-make-repeatable-interactive-command 'switch-to-previous-file 'switch-to-previous-file-repeatable)

;; based on https://www.emacswiki.org/emacs/BookmarkPlus#toc63
(defun my-auto-l+c-name (position)
  "Return a name for POSITION that uses line & column numbers."
  (let* ((line  (line-number-at-pos position))
         (col0   (save-excursion
                  (goto-char position) (current-column)))
         (col (if (bound-and-true-p column-number-indicator-zero-based) col0 (1+ col0)))
         (fn0 (condition-case nil
                  (which-function)
               (error nil)))
         (fn (if fn0 (format ":%s" fn0) "")))
    (format "%s:%d,%d%s" (buffer-name) line col fn)))

;; https://emacs.stackexchange.com/questions/42529/insert-date-using-a-calendar
(defun calendar-insert-date ()
  "Capture the date at point, exit the Calendar, insert the date."
  (interactive)
  (seq-let (month day year) (save-match-data (calendar-cursor-to-date))
    (calendar-exit)
    (insert (format "%d-%02d-%02d" year month day))))

(defun toggle-minimap ()
  "Toggle the minimap."
  (interactive)
  ;; (minimap-mode 'toggle)
  (sublimity-mode 'toggle))

(defun select-all (&optional leave-point-at-end)
  "Mark all text in the current buffer.

With no args, leave point at start of buffer. With the universal arg,
leave point at end of buffer."
  (interactive "P")
  (if leave-point-at-end
    (progn
      (goto-char (point-min))
      (push-mark (point) 'nomsg 'activate)
      (goto-char (point-max)))
    (progn
      (goto-char (point-max))
      (push-mark (point) 'nomsg 'activate)
      (goto-char (point-min))))
  (message "Whole buffer marked"))

;; https://stackoverflow.com/questions/8606954/path-and-exec-path-set-but-emacs-does-not-find-executable
;; see also https://github.com/purcell/exec-path-from-shell
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(defun my-flyspell-correct-lucky (&rest args)
  "`I'm feeling lucky' mode for flyspell-correct-previous.

Accept the first suggestion without prompting.
"
  (interactive "P")
  (let ((flyspell-correct-interface (lambda (candidates misspelled-word)
                                      (car candidates))))
      (call-interactively 'flyspell-correct-previous)))

(defun my-flyspell-correct-unlucky (&rest args)
  "Wrapper for flyspell-correct-previous.

If invoked right after a `my-flyspell-correct-lucky', undo first.

This allows first hitting \\[my-flyspell-correct-lucky], which is
often sufficient, but if the default suggestion was not correct,
it is then possible to immediately hit \\[my-flyspell-correct-unlucky]
to correct interactively with minimum keypresses."
  (interactive "P")
  (when (eq last-command 'my-flyspell-correct-lucky)
    (undo-tree-undo)
    ;; Undoing the edit does not restore the misspelled-word status.
    ;; Force flyspell to update its overlay.
    ;; TODO: we assume the word that used to be misspelled is now on screen.
    (flyspell-region (window-start) (window-end)))
  (call-interactively 'flyspell-correct-previous))

(defun unparenthesize-python-return-stmts ()
  "Unparenthesize all return values in Python return statements from point forward."
  (interactive)
  (while (search-forward "return(" nil t 1)
    (backward-char)
    (insert-char #x20 1 t)
    (sp-unwrap-sexp)))

(defun newline-and-indent-relative ()
  "Start a new line and call indent-relative.

Useful when writing Python docstrings, where anaconda-mode
insists on indenting to the level of the def, even in the
parameter list in the docstring."
  (interactive)
  (if electric-indent-mode
    (electric-indent-just-newline nil)
    (newline))
  (indent-relative))

(defun insert-key-description ()
  "Insert a `(kbd ...)' compatible description string for a key sequence.

Prompts for the key sequence."
  (interactive)
  (let* (($raw (read-key-sequence "Key sequence to insert:" nil t nil nil))
         ($k (key-description $raw)))
    (insert (concat "\"" $k "\""))
    (message "")))

;; TODO: Where does the second "View command" prompt come from, it's not in the AUCTeX sources?
;; TODO: Call the original if called with a prefix argument.
(defun my-default-TeX-command (&optional override-confirm)
  "Like TeX-command-master, but use the default command."
  (interactive "P")
  (TeX-command (TeX-command-default (TeX-master-file nil nil t))
	             'TeX-master-file override-confirm))
