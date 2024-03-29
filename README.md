# spacemacs.d

My [Spacemacs](http://spacemacs.org/) configuration. Contains some useful custom functions (this being Emacs, just like everyone else's).

Any contributions from the internet are attributed in source code comments (in `init.el`), as appropriate. All original content in this repository is published under [The Unlicense](LICENSE.md).

Potentially useful notes below.


## Customizations

 - **Usability, general**:
   - Use the Zenburn color theme, easy on the eyes with good highlight colors. See the [theme gallery](https://themegallery.robdor.com/) or [its GitHub repo](https://github.com/robmerrell/spacemacs_theme_gallery).
   - Quick file switching in current window with `C-<next>`, `C-<prior>`. Skips any non-file buffers and knows when no files are open, or when the current buffer is the only file open. For the purposes of the switcher, Spacemacs's `untitled` buffers count as files.
     - Mainly more convenient than `M-m b n`, `M-m b p`, by having an easily repeatable key combo, and skipping buffers that are often uninteresting.
   - In `text-mode`, auto-complete from any text in any open buffer. We configure `company` to use `dabbrev` via a mode hook.
   - Spellcheck with [Enchant](https://github.com/AbiWord/enchant), allows proper spellchecking of both English and Finnish. Pre-configured for these two languages. (Requires a suitable Enchant and backends, see below for details.)
   - Bump `undo-limit` to a modern-day useful value ([see here](https://www.reddit.com/r/emacs/comments/90si1c/why_cant_i_have_truly_persistent_undo_on_emacs/)). Helps with *oops, I just deleted the whole document* edits with large files such as source code.
   - Bookmark tweaks. Use [`bookmarkplus`](https://www.emacswiki.org/emacs/BookmarkPlus), and:
     - Adjust autoname format to `file.py:301,12:dostuff`, see `my-auto-l+c-name`. The function name part is omitted when `(which-function)` fails or returns nil.
     - Make autonamed bookmarks persist across sessions.
     - Tweak the visual style of all bookmarks to a non-intrusive blue (`SteelBlue4`). The fringe indicator goes on the **left for autonamed** bookmarks, and on the **right for manually named** bookmarks. 
   - Enable 1-based column indexing. Rows already are, so why not columns too?
   - Diminish some more minor mode lighters to de-clutter the modeline.
   - Minimap mode (bound to `<f9>`), provided by `sublimity`.
   - Several small things such as binding a key to the typo zapper (`<f8>`), and adding an `unfill-paragraph` function (`M-S-q`, [thanks to Stefan Monnier](https://www.emacswiki.org/emacs/UnfillParagraph)).
 - **Cursor handling**:
   - *[Vim scrolloff](https://wolfecub.github.io/dotfiles/)*: cursor always tries to stay at least 10 lines from the upper/lower edge.
   - [`beacon`](https://github.com/Malabarba/beacon) to never visually lose the cursor when scrolling or switching buffers.
   - [`mwim`](https://github.com/alezost/mwim.el) for smart home/end keys.
   - [`multiple-cursors`](https://github.com/magnars/multiple-cursors.el), with [`phi-search`](https://github.com/zk-phi/phi-search) incremental search. Uses the Spacemacs layer, with the `mc` backend.
 - **Usability, programming**:
   - `auto-highlight-symbol-mode` and `which-function-mode` enabled by default.
   - `python-mode`: enable `flycheck`, set up `company` for auto-completion. (Requires at least `flake8` and `jedi` from `pip`.)
   - `python-mode`: syntax highlighting for [`mcpyrate`](https://github.com/Technologicat/mcpyrate) and [`unpythonic`](https://github.com/Technologicat/unpythonic).
   - Lisp eval minibuffer (`M-:`): enable `eldoc-mode`, `paredit-mode` and `smartparens-mode`.
   - Enable `prettify-symbols-mode` for an experimental code-readability improvement, with a generic setup and a Python-specific setup.
 - **Miscellaneous**:
   - Make tab completion work in Helm when Treemacs is open ([thanks to Simon Bourne](https://github.com/syl20bnr/spacemacs/issues/7446#issuecomment-417334718)).
   - New command `M-x select-all` that does exactly as it says on the tin. One-command alternative to `C-<end> C-SPC C-<home>`.
   - New command `M-x insert-key-description` to auto-create args for `(kbd ...)`. Prompts for a key sequence.
   - New command `M-x unparenthesize-python-return-stmts` to strip parentheses around return value from Python return statements, from point forward. Requires `smartparens`.
     - To strip just one, see `M-x unparenthesize-python-return-stmt`, a recorded keyboard macro.
   - New command `M-x my/scale-latex-fragments`, to scale the font size of `org-mode` LaTeX previews of the current buffer to match the (possibly zoomed) font size of the current frame. For use with Spacemacs's `zoom-frm-transient-state` (`M-m z f`). No hook for now; call this `M-x` command manually when needed. May take some time to create the LaTeX previews if the buffer has a lot of content.
   - Utility `my-make-repeatable-interactive-command`, which returns a repeatable (C-digits) version of a non-repeatable interactive command, and optionally `fset`s it to a desired name.
   - Finnish national holidays for the Emacs calendar (via package [`suomalainen-kalenteri`](https://github.com/tlikonen/suomalainen-kalenteri)). Highlight holidays by default.
   - Enable `pdf` layer for viewing **and annotating** PDFs in Emacs.

Note you'll need the *Source Code Variable* font from [here](https://github.com/adobe-fonts/source-code-pro).


## Custom key bindings

Most (but not all) of these are added by a global custom minor mode `custom-keys-minor-mode`, which is enabled by default.

This `init.el` is focused on the `emacs` editing mode (`holy-mode`) of Spacemacs. Hence `M-m` instead of `SPC` as the leader key in the relevant key bindings.

Key | Command
:--- |:---
`C-x` | cut (via `cua-mode`)
`C-c` | copy (via `cua-mode`)
`C-v` | paste (via `cua-mode`)
`C-z` | `undo-tree-undo`
`C-S-z` | `undo-tree-redo`
`s-+` | zoom in (`spacemacs/zoom-frm-in`)
`s--` | zoom out (`spacemacs/zoom-frm-out`)
`s-0` | reset zoom (`spacemacs/zoom-frm-unzoom`)
`S-RET` | sequence: newline, `indent-relative`. Accounts for electric-indent-mode, calling `newline` or `electric-indent-just-newline` as needed. Handy for working with Python docstrings, Org todo lists, and similar.
`S-<backspace>` | `delete-indentation`. New binding since the default `M-^` is not accessible on Finnish keyboards. Also handy for working with Python docstrings.
`M-z` | `helm-select-action`, to unshadow it since `C-z` is taken by undo
`C-s` | `phi-search` (supports `multiple-cursors`)
`C-r` | `phi-search-backward` (supports `multiple-cursors`)
`C-S-c` | show and copy absolute filename of current buffer (if file)
`C-S-d` | show and copy absolute path of current directory (if file buffer)
`C-u C-S-d` | show and copy absolute path of project root (if in project)
`C-S-e` | open file manager in current directory (if file buffer)
`C-u C-S-e` | open file manager in project root (if in project)
`C-S-t` | open dedicated terminal in current directory, see var `dedicated-terminal-command-template`
`C-u C-S-t` | open dedicated terminal in project root
`C-t` | `spacemacs/shell-pop-inferior-shell`
`C-c e` | eval and replace sexp under point (simple Lisp calculator), the sexp goes to the kill ring
`C-<next>` | next file buffer (in current window)
`C-<prior>` | previous file buffer (in current window)
`M-S-q` | unfill paragraph, the counterpart of `fill-paragraph`
`C-M-S-q` (also `s-q`) | set paragraph fill width to point (like `C-x f RET`), then `fill-paragraph`
`<f6>` | `iedit-mode` on/off, to quickly rename all occurrences of symbol under point (or to jump between them: `<TAB>`, `S-<TAB>`)
`<f7>` | `org-agenda`
`<f8>` | zap last typo before point via `flyspell-correct-previous`. When pressed once, auto-accept the first suggestion. If immediately pressed again, undo and choose a correction interactively.
`C-<f8>` | zap last typo before point, always auto-accept first suggestion.
`S-<f8>` | zap last typo before point, always choose a correction interactively. If pressed immediately after zapping a typo with `<f8>` or `C-<f8>`, automatically undo that first.
`<f9>` | toggle minimap (provided by `sublimity`)
`<f12>` | toggle folding of current element (`yafolding`)
`S-<f12>` | go to parent element (`yafolding`)
`C-x 8 l` | insert `λ`
`C-x 8 RET` | `counsel-unicode-char`; like the default `insert-char`, but with preview
`M-m s M` | `helm-multi-swoop` buffers that have the same major mode as the current one
`M-m s C-p` | `helm-multi-swoop-projectile` 
`C-c s` (org-mode) | schedule entry (so it shows up as scheduled in the agenda)
`M-m m O` or `M-RET O` (LaTeX mode) | `reftex-toc` (same binding for TOC as in Spacemacs `pdf` layer)
`RET` (calendar mode) | insert date under point in `YYYY-MM-DD` format
`<f5>` (python-mode) | run buffer contents in python shell (similarly as in racket-mode)
`<f5>` (latex-mode) | run the default LaTeX command (useful for recompiling after a change; cf. `C-c C-c`, which always asks which operation to perform)
`<f5>` (org-mode) | export via LaTeX to PDF
`M-m M-j` | `evil-avy-goto-char-timer` (ergonomics, less repetitive than default `M-m j j`)
`M-m j g` | `dumb-jump-go`, jump to definition of thing under point using [`dumb-jump`](https://github.com/jacktasia/dumb-jump) (works with Julia)

[Smartparens](https://github.com/Fuco1/smartparens) keys:

Key | Command
:--- |:---
`C-M-f` | `sp-forward-sexp`
`C-M-b` | `sp-backward-sexp`
`C-M-d` | `sp-down-sexp`
`C-M-a` | `sp-backward-down-sexp` ("ackward" down)
`C-M-e` | `sp-up-sexp` ("exit")
`C-M-u` | `sp-backward-up-sexp`
`C-M-n` | `sp-next-sexp`
`C-M-p` | `sp-previous-sexp`
`C-M-k` | `sp-kill-sexp`
`C-M-SPC` | `sp-mark-sexp`
`C-M-S-u` (also `s-u`) | `sp-unwrap-sexp` (note the shift)
`C-M-S-r` (also `s-r`) | `sp-rewrap-sexp` (note the shift)
`C-M-S-s` | `sp-slurp-hybrid-sexp`
`C-M-S-b` | `sp-forward-barf-sexp`
`C-M-S-a` | `sp-backward-slurp-sexp` (one key "back" (left) from slurp)
`C-M-S-v` | `sp-backward-barf-sexp` (one key "back" (left) from barf)

Smartparens works also for things such as quoted strings and parenthesized remarks in plain text.

Note as usual in CUA mode, `C-x`, `C-c` still act as prefixes if no region is active - or, when a region is active, if pressed quickly twice in succession (e.g. `C-x C-x`). So when a region is active, you can e.g. `python-indent-shift-left` with `C-c C-c <`.

The `custom-keys-minor-mode` (while enabled) unmaps `C-z` from `evil-emacs-state-map`, because emulation maps precede minor mode maps and we want to have `C-z` for undo.

Note the function keys `<f10>` (menu) and `<f11>` (toggle frame full screen) are already taken.

The `<f10>` menu is missing many useful commands and includes many useless ones. Instead, in Spacemacs, for discovery use `M-m` (Spacemacs menu), `M-x` (Helm M-x), `M-m h d a` (Helm apropos).

We use `sublimity` as the minimap provider, because it works also in LaTeX mode, whereas the `minimap` package does not seem to want to co-operate with it. Upon first activation (globally per session), `sublimity` requires moving the cursor before the minimap actually appears (or alternatively, press `<f9>` three times at the first use during the session to force it to appear immediately). Upon second and further activations, the minimap appears immediately.

If you want to use `minimap` instead, be sure to [get it from GitHub](https://github.com/dengste/minimap); the version on ELPA seems to be old even though it has the same version number. As of this writing, only the version on GitHub has the ability to change the color of the highlighted line.

We use `yafolding` instead of the folding features of `evil`, because it requires no configuration, works perfectly for Python, and importantly, *draws nice-looking folding markers at the fringe*. Spacemacs's `evil` folding (required for languages not based on indentation) of course remains available.

We provide `C-M-S-u` and `C-M-S-r` also as `s-u` and `s-r`, respectively, because the `C-M-S-` variant is reserved in Linux Mint (unicode input and screen recording, respectively), while the `s-` variant is reserved in Windows (accessiblity settings and run command, respectively), which may still be relevant in corporate environments.

**CAUTION**: The undo-and-correct-interactively feature of `<f8> S-<f8>` requires the corrected word still to be on the screen in order to work correctly. This limitation is because the flyspell overlay is deleted when a correction is accepted, and `undo-tree-undo` to remove the correction does not restore the overlay. Thus, as a workaround, when you press `<f8> S-<f8>`, after performing the undo, we run `flyspell-region` for the text currently visible on screen (to restore the overlay) before invoking `flyspell-correct-previous` again. This is known to be slow for buffers with only a screenful of text in them (e.g. git commit messages).


## Some useful standard key bindings

These should be standard-ish Emacs.

Key | Command
:--- |:---
`C-g` | "quit", i.e. cancel the current command or ask Emacs to stop whatever it is currently doing.
`M-x` | the famous meta-x: invoke command by name (the Helm version installed with Spacemacs keeps history, supports incremental fragment search from all available command names, and shows the matching keybind for commands bound to a key combo)
`C-x C-c` | quit Emacs
`C-x C-s` | save
`C-x C-w` | save as
`C-SPC` | (via `cua-mode`) set/unset mark, useful to select stuff without holding down shift
`C-u C-SPC` | (via `cua-mode`) jump to previous location of mark (so `C-SPC C-SPC` to tag a location without leaving mark active, and then later `C-u C-SPC` to jump back to it; consider also bookmarks if you need a more persistent solution) (can also use this to jump back to last edited location if you accidentally hit page-down while typing)
`M-l` | `downcase-word`, make word all-lowercase (starting at point; if at whitespace, apply to next word)
`M-c` | `capitalize-word`, make word have one uppercase letter (at point; if at whitespace, apply to next word)
`M-u` | `upcase-word`, make word all-uppercase (starting at point; if at whitespace, apply to next word)
`<f1>` | `help-command`, same as `C-h`
`<f1> k` | "what does the following key combo do?"
`<f1> f` | "what is the Lisp function under point?"
`<f1> v` | "what is the Lisp variable under point?"
`<f1> l` | `view-lossage` i.e. "what did I just press?"
`C-u C-x =` | `what-cursor-position` i.e. "what is the character under cursor?". Without the `C-u` gives just the number, with `C-u` also the name along with other info. Spacemacs has this as the more mnemonic `M-m h d c` (**m**enu **h**elp **d**escribe **c**haracter).
`<f3>` | start recording keyboard macro
`<f4>` | stop recording; replay last recorded keyboard macro (if not recording)
`C-x` | prefix: global commands
`C-x z` | repeat last command (that was not caused by an input event), hit more `z` to repeat again
`C-x o` | focus other window (e.g. in two-column view, Spacemacs `M-m w v`)
`C-x 2` | `split-window-below`, same as Spacemacs `M-m w s`
`C-x 3` | `split-window-right`, same as Spacemacs `M-m w v`
`C-c` | prefix: major-mode specific commands (Spacemacs has also `M-m m`, which is different)
`ESC` | *meta*; same as holding Alt (e.g. `ESC d` is `M-d`), but in Spacemacs allows discovery of `M-something` key bindings
`C-s` | isearch forward (see also `helm-swoop`)
`C-r` | isearch backward (see also `helm-swoop`)
`C-a` | jump start of line (same as `<home>`)
`C-e` | jump end of line (same as `<end>`)
`M-a` | jump start of sentence (previous full-stop) (in `python-mode`, start of defun)
`M-e` | jump end of sentence (next full-stop) (in `python-mode`, end of defun)
`C-M-<home>` | jump beginning of defun
`C-M-<end>` | jump end of defun
`C-<up>` | jump up paragraph (previous blank line)
`C-<down>` | jump down paragraph (next blank line)
`M-g M-g` | goto line (also `M-g g`, so doesn't matter whether Alt is held down or not)
`C-u M-g M-g` | goto line in most recently used other buffer (sometimes useful with a stack trace in an inferior shell)
`M-s h` | set/remove visual highlighting (persists for the session until manually removed)
`M-s h .` | highlight symbol under point
`M-s h p` | highlight phrase (will ask for phrase and color)
`M-s h u` | remove highlighting (will ask which)
`C-x d` | `dired` current directory
`C-x p` | `bookmarkplus` menu ("pookmarks"? `C-x b` is taken for buffer switching...) Spacemacs has this as `M-m B`.
`C-x p RET` | toggle autonamed bookmark at point
`C-x p n` | jump to next bookmark in current buffer (wraps around)
`C-x p p` | jump to previous bookmark in current buffer (wraps around)
`C-x p e` | edit/view all bookmarks (`RET` to jump, `x` to delete, `s u` to sort by url, `s f n` to sort by file name; see `?` for more key bindings)
`C-x p c m` | create named bookmark at point
`C-x p <delete>` | delete bookmark at point
`C-x p d` | delete bookmark by name (will prompt)
`C-x r` | registers menu (multi-clipboard for pasting snippets often needed during a session)
`C-x r s` | copy selection to register ("save")
`C-x r i` | insert contents of register (in Spacemacs, see also `M-m r r`, a register viewer/selector)
`C-x 8` | input some special chars, `RET` for insert by name (see also `M-x counsel-unicode-char`)
`C-x 0` | kill current window
`C-x k` | kill buffer (will ask which)
`C-x 4 0` | kill current window **and buffer**
`C-x 1` | maximize, i.e. kill other windows
`C-x C-x` | `exchange-point-and-mark` (e.g. jump cursor to other end of selected region, but preserve selection). When using `cua-mode`, if a region is selected, must use a quick `C-x C-x` and then another `C-x`, otherwise the first `C-x` will cut instead!
`C-c C-s i` | `synosaurus-choose-and-insert` ([thesaurus minor mode](https://github.com/hpdeifel/synosaurus) using [WordNet](https://wordnet.princeton.edu/) for English (`sudo apt install wordnet`))
`C-c C-s l` | `synosaurus-lookup`
`C-c C-s r` | `synosaurus-choose-and-replace`
`M-m m O` or `M-RET O` | (pdf-mode) view outline (table of contents)
`M-m m n` or `M-RET n` | (pdf-mode) `pdf-view-midnight-minor-mode`, reverse video grayscale view
`M-RET s s` | (pdf-mode) `pdf-occur` (search for text in PDF, show a list of all matches)
`l` | (help viewer) back to previous viewed help page
`r` | (help viewer) forward (in the sense of opposite of back)
`q` | (help viewer, many others) quit, usually kills the window too
`M-<` | jump to beginning
`M->` | jump to end
`M-.` | (prog-mode) jump to definition of symbol under point, after pushing marker
`M-,` | (prog-mode) jump back by popping marker
`M-?` | (anaconda-mode) show docstring
`S` | (flycheck error list) sort by column under point (if possible)
`h` | (calendar-mode) show holidays for date under point
`a` | (calendar-mode) list holidays for current three months 
`S` | (calendar-mode) show sunrise, sunset, daylight length for date under point (if location configured; see `calendar-latitude`, `calendar-longitude`, `calendar-location-name`)
`M` | (calendar-mode) list moon phases for current three months
`d` | (dired-mode) mark file for deletion
`x` | (dired-mode) actually delete files marked for deletion
`C-<mouse-1>` or `<double-mouse-1>` | (pdf-tools) Jump to clicked position in LaTeX source
`C-c C-v` | (latex-mode) View compiled document / jump to position of point in compiled view (compile first; `C-c C-c`, or in our setup, `<f5>`)

Function keys: by default, `<f2>` is used for 2C two-column mode, which doesn't play well with Spacemacs. Keys `<f5>`...`<f9>` and `<f12>` are unused. (We use some of them, see *Custom key bindings* above.)

Essentially, `<f3>` and `<f4>` together provide the same functionality as vim's `.`.

The `M-<` and `M->` keys are useful in contexts where the `<home>` and `<end>` keys are reserved for other purposes. For example, in a Helm session, `<home>` and `<end>` move the cursor in the text entry field, whereas `M-<` and `M->` jump to the beginning/end of the list of completion suggestions. The `M-<` and `M->` key bindings are also hard-coded into some modes such as the help viewer, and always guaranteed to work.

To debug a frozen Emacs (and potentially save your work), when `C-g` does not help, try `killall -s USR2 emacs` in a terminal. This will essentially `M-x toggle-debug-on-quit`.

Beside short cursor hops by nearby content, isearch is useful also in keyboard macros, e.g. to jump to a delimiter (usually on the same line). For actual search, see `helm-swoop`.


## Some useful Spacemacs key bindings

Key | Command
:--- |:---
`M-m` | Spacemacs menu
`M-m m` | major-mode specific Spacemacs menu (different from prefix `C-c`)
`M-m ?` | show keybindings (searchable, opens a Helm window)
`M-m h p` | `helm-spacemacs-help-packages`, list package status (with Spacemacs layer info)
`M-m a k` | `paradox-list-packages`, package manager (DANGER: bypasses Spacemacs layer system)
`M-m h d a` | `helm-apropos`, search Emacs documentation
`M-m h d f` | ...for Lisp functions only
`M-m h d v` | ...for Lisp variables only
`M-m b N n` | new file (create empty buffer and switch to it)
`M-m b d` | close file (kill current buffer)
`M-m b u` | re-open last closed file (buffer), re-reading it from disk
`M-m b R` | revert current file from disk
`M-m f R` | rename current file on disk
`M-m f s` | save (at first save of an unsaved buffer, will ask for destination) (same as `C-x C-s`)
`M-m q q` | quit Emacs (will ask to save modified buffers; to cancel quitting, use `C-g`)
`M-m q Q` | quit Emacs (just do it)
`M-m q f` | quit Emacs (kill current frame, but leave Emacs open in the background, so that it starts much faster if you need it again)
`M-m f f` | open file
`M-m f r` | open recent file
`M-m b b` | show currently open buffers (jump by fragment search from buffer names)
`M-m b m` | show the messages buffer (useful e.g. if you quickly saw an error message in the minibuffer, but it was already replaced by another message)
`M-m p p` | switch to (another) recent project
`M-m p f` | open file from current project
`M-m f c` | copy current file (will ask for destination)
`M-m w 2` | 2-column window layout (Spacemacs compatible)
`M-m w d` | kill current window (same as `C-x 0`)
`M-m w u` | undo kill window (`M-x winner-undo`)
`M-m w m` | maximize, i.e. kill other windows (same as `C-x 1`)
`M-m l s` | save frame layout (will prompt for a name), including windows and buffers currently open
`M-m l L` | load frame layout
`M-0` | open/focus Treemacs (use `M-0 q` to hide it)
`M-1` | focus window 1 (other numbers available, too)
`M-m j j` | avy timer, jump to a fragment of text on screen (type in a few letters or a word quickly, then wait and pick the occurrence to jump to)
`M-m v` | `expand-region`, select word/sentence/paragraph, sexp, string, ...
`M-m n` | narrowing, restrict view to current function et al. (to operate on it as if it was the whole document)
`M-m n r` | `narrow-to-region`
`M-m n w` | `widen`, i.e. exit from narrowing
`M-m g s` | open magit, the full-featured git control panel (see its `?`)
`M-m a u` | open the undo-tree viewer (see its `?` for key bindings), same as `C-x u`
`M-m e L` | open and jump to `flycheck` error list for current file
`M-m e n` | jump to next `flycheck` error
`M-m e p` | jump to previous `flycheck` error
`M-m s s` | `helm-swoop`, isearch current buffer, with preview of matches (up/down to pick, `RET` to jump, `C-g` to cancel)
`M-m s p` | `spacemacs/helm-project-smart-do-search`, like `M-m s s`, but search the whole project.
`M-m s d` | `spacemacs/helm-dir-smart-do-search`, like `M-m s s`, but search from all files in the current directory.
`M-m s S` | `helm-swoop-region-or-symbol`, like `M-m s s`, but use the highlighted word or symbol under point as the query.
`M-m s P` | `spacemacs/helm-project-smart-do-search-region-or-symbol`, like `M-m s p`, but use the highlighted word or symbol under point as the query.
`M-m s D` | `spacemacs/helm-dir-smart-do-search-region-or-symbol`, like `M-m s d`, but use the highlighted word or symbol under point as the query.
`M-m r l` | resume Helm session (e.g. continue a `helm-swoop` search that was exited via `RET`)
`M-m r y` | show kill ring (`RET` to paste an entry, `C-g` to cancel)
`M-m S` | spellchecking menu
`M-m S .` | spellchecking transient state
`M-m S d` | dictionary, i.e. switch to another language
`M-m S b` | spellcheck whole buffer
`M-m S s` | spellcheck word at point
`M-m z f` | zoom frame transient state (increase/decrease font size)
`M-m s m` | `multiple-cursors` menu (once in `multiple-cursors-mode`, use `RET` to exit it; use `C-m` to insert a literal RET at each cursor)
`M-m s m a` | `mc/mark-all-dwim` (useful with `narrow-to-region`)
`M-m s m r` | `mc/edit-lines` (be careful what exactly is selected and where exactly point is before you invoke this!) (`RET` to exit multiple cursor edit mode)
`M-m s m s n` | insert a running number at cursors (starting from 0)
`C-u 1 M-m s m s n` | insert a running number at cursors, starting from 1 (use a numeric prefix argument)
`M-m b w` | toggle buffer read-only
`M-m b w` (dired-mode) | make Dired listing writable (`C-c C-c` commit changes; `C-c C-k` cancel), e.g. for easy mass rename using the full power of Emacs
`M-m r r` | `helm-register`, i.e. view contents of registers, choose and paste one (see also `C-x r`, the registers menu)

Note in [`projectile`](https://github.com/bbatsov/projectile), a project is [any directory that is under version control](https://jaketrent.com/post/projects-in-spacemacs/), so you don't have to do anything extra to create or manage a project. However, [Treemacs](https://github.com/Alexander-Miller/treemacs) has its own idea of projects, which are essentially directories registered manually; see its `?` for key bindings.

Note also that `helm-swoop` is much better than a standard isearch; it is actually an **incremental fragment search**. For example, to match the text *parachute oranges*, you can use *par ora* or even *ran chu* as the search query. The order of the fragments does not matter. The fragments can be regexes; particularly, use `\b` to mark a word boundary (e.g. `\bora` to match only words beginning with `ora`, and `nges\b` to match only words ending with `nges`).

Also, `helm-swoop` is actually a quick navigation feature in disguise. Even if your document consists of several hundred pages (e.g. a book manuscript), `help-swoop` allows you to jump to any line almost instantly, as long as you remember a couple of words from that line. For example, if the part you want talks about the *Cauchy stress tensor*, a quick query for *cau stre ten* will find it. To continue the example, if the part you want specifically says that the tensor is symmetric, *cau stre ten symm* will narrow down the matches. If you type a lot anyway, this is much faster than other forms of in-document navigation.

(Once you've tried `helm-swoop`, you'll wish everyone implemented search like this.)


## Notes on Org-mode

So far mostly for managing TODO lists. And authoring technical documents.

Key | Command
:--- |:---
`TAB` | cycle folding for current subtree
`S-TAB` | cycle folding for whole buffer (also `C-u TAB`)
`C-c C-n` | jump to next heading (disregard level)
`C-c C-p` | jump to previous heading (disregard level)
`C-c C-f` | jump to next heading at same level
`C-c C-b` | jump to previous heading at same level
`C-c C-u` | jump up one level
`S-<right>`, `S-<left>` | cycle entry state TODO/DONE/none (also `C-c C-t`); when on a timestamp, shift the date
`S-<up>`, `S-<down>` | cycle entry priority #A/#B/#C/none
`M-<up>`, `M-<down>` | move entry up/down
`M-<left>`, `M-<right>` | indent/outdent entry (in empty entry, can also use `TAB` to cycle level)
`M-RET` | insert blank heading at same level
`M-S-RET` | insert TODO heading at same level
`C-c C-s` | schedule entry (so it shows up as scheduled in the agenda) (synosaurus eats this, so we use `C-c s` instead)
`C-u C-c C-s` | unschedule entry
`C-c C-d` | insert deadline for entry
`C-c .` | insert timestamp (see manual sections [8.1](https://orgmode.org/manual/Timestamps.html#Timestamps), [8.2](https://orgmode.org/manual/Creating-Timestamps.html#Creating-Timestamps) and [8.3](https://orgmode.org/manual/Deadlines-and-Scheduling.html#Deadlines-and-Scheduling), and [stackexchange](https://emacs.stackexchange.com/questions/10504/understanding-scheduled-in-org-mode))
`C-c C-l` | `org-insert-link`, insert a hyperlink
`C-c C-o` | `org-open-at-point`, open thing (such as link) in current entry (will prompt if several exist)
`C-c C-x C-l` | `org-toggle-latex-fragment`, preview equation inline as image (if LaTeX installed) (with one `C-u`, toggle all equations in current section; with two `C-u`, toggle all equations in document)
`C-c C-x \` | `org-toggle-pretty-entities`, superscript/subscript/greek prettification on/off

Use `M-x org-sort` with point on a parent entry to move DONE children to end (choose `o` for todo **o**rder).

Syntax for pretty links is `[[target][description]]`. To create or edit a link, `M-x org-insert-link` (`C-c C-l`). Also available: `M-x org-toggle-link-display`, or just move to a link's end, and backspace to delete the last bracket.

See also `org-store-link` to stash a link to file location at point (in whatever file). The stashed link can then be inserted at point, when editing an org file, with `C-c C-l`.

Shift selection not available by default, use `C-SPC` (set mark) and then cursor movement to select a region.

For scheduling, repeater syntax is like `+1h`, `+1d`, `+1w`, `+1m`, `+1y`, such as `<2019-06-24 Mon +1w>`. Also `++` and `.+`. See [here](https://orgmode.org/manual/Repeated-tasks.html).

See [Org tutorial](https://orgmode.org/worg/org-tutorials/orgtutorial_dto.html) and [Org manual](https://orgmode.org/manual/index.html) for more.


### Org template for LaTeX output

Place this at the start of your org-mode document:

```org
#+STARTUP: latexpreview
#+STARTUP: entitiespretty
#+STARTUP: showeverything
#+OPTIONS: num:nil
#+OPTIONS: toc:nil
#+LATEX_HEADER: \usepackage[margin=2.0cm]{geometry}
#+LATEX_HEADER: \usepackage{parskip}
#+LATEX_HEADER_EXTRA: \hypersetup{colorlinks=true,linkcolor=blue}
#+title:
#+author:
```

Then just insert document title and author, and start writing.

This displays LaTeX fragments as images (for equations); pretty-renders entities as unicode (so that e.g. `$\alpha^2$` shows as α² right there in Emacs); expands all sections by default; does not number sections; no table of contents (TOC); a more sensible margin size; paragraphs separated by a vskip, no indent; and no frames around links.

Any `LATEX_HEADER_EXTRA` options only take effect for PDF export, not for fragment preview (such as for equations). If the `hypersetup` option is present in fragment preview, the fragment will erroneously show the `colorlinks=true,linkcolor=blue`.

See [[1]](https://orgmode.org/manual/Comment-lines.html), [[2]](https://orgmode.org/manual/Initial-visibility.html), [[3]](https://www.gnu.org/software/emacs/manual/html_node/org/In_002dbuffer-settings.html) and [[4]](https://orgmode.org/manual/LaTeX-header-and-sectioning.html).


## Some useful M-x commands

 - `count-words`
 - `link-hint-open-link-at-point`
 - `counsel-unicode-char` pick and insert unicode char by name, with preview (we have this bound to `C-x 8 RET`)
 - `git-timemachine` quickly see and/or copy text from an old revision (`p`/`n` previous/next revision; `q` to return to file in working tree; while active, save the buffer to overwrite file in working tree with the historical revision being viewed)
 - `replace-string`, `replace-regexp`
 - `calendar`
 - `grep`, accepts any command that produces output like `grep -nH` (while active, `{`, `}` jump to previous/next file with matches)
 - `diff` compare files
 - `ediff` compare files, [more sophisticated](https://www.gnu.org/software/emacs/manual/html_node/emacs/Comparing-Files.html)
 - `ediff3` three-way compare files
 - `helm-for-files` find a file ([see e.g. this](http://pragmaticemacs.com/emacs/find-and-open-files-from-anywhere-with-helm-for-files/))
 - `list-colors-display` pick and insert color, with preview
 - `name-last-kbd-macro`, `insert-kbd-macro` to save keyboard macros as Lisp (for `init.el`)
 - `artist-mode` draw rectangles and similar
 - `define-word-at-point` (needs internet access, unless you have a local installation of wiktionary) (Spacemacs: `M-m x w d`)
 - `customize`, `customize-group` configure Emacs settings (a.k.a. *Customize*; some packages prefer to use this)
 - `org-agenda` agenda overview
 - `org-todo-list` overview of TODOs, once `org-agenda-files` is configured (`RET` to jump, `t` to cycle TODO/DONE/none, `s` to save all org buffers, `q` to quit)


## Fast Emacs startup

In short: use daemon mode.

Save [this handy script](https://medium.com/@bobbypriambodo/blazingly-fast-spacemacs-with-persistent-server-92260f2118b7) as `~/.local/bin/em` (it's now also provided as [`em.sh`](em.sh) in the repo):

```bash
#!/usr/bin/env bash
# https://medium.com/@bobbypriambodo/blazingly-fast-spacemacs-with-persistent-server-92260f2118b7
# https://superuser.com/questions/142945/bash-command-to-focus-a-specific-window

# Check if there's an Emacs frame open
emacsclient -n -e "(if (> (length (frame-list)) 1) 't)" 2> /dev/null | grep t &> /dev/null

if [ "$?" -eq "1" ]; then
    emacsclient -a '' -nqc "$@" &> /dev/null
else
    emacsclient -nq "$@" &> /dev/null
fi

# Focus the Emacs window
wmctrl -a emacs
```

and then set up your start menu shortcut (if you use one) to invoke that instead of `emacs` directly. (Though the process-spawning features, such as opening file browsers or external terminal sessions, seem to work more reliably if the script is invoked from a terminal.)

When using this, use `M-m q f` (`spacemacs/frame-killer`) to quit Emacs so that it stays running in the background; `em` will connect to the running instance, if there is one.

To shut down Emacs, shutting down also the daemon, use `M-m q q` as usual.


## Configuring flake8 and autopep8

This is used for static analysis of Python (to autodetect and warn about common trivial problems), and automatic [PEP8](https://www.python.org/dev/peps/pep-0008/) code style enforcement.

[The default location](https://flake8.pycqa.org/en/latest/user/configuration.html) (as far as `flake8` itself is concerned) for the configuration is `~/.config/flake8`. Note no `rc` at the end of the name, and that is a filename, not a directory.

However, Emacs thinks the default location is `~/.config/flake8rc` (note the `rc`), so if you want `flycheck-verify-setup` (`M-m e v`) to see it, the value must be customized (via `M-x customize-group flycheck-executables`). (This is already done in the `init.el` provided here.)

This gives [Spyder](https://github.com/spyder-ide/spyder)-like notes in the fringe for statically detected errors and style issues.

For `autopep8`, the default location is `~/.config/pep8`. This can be just a symlink to `~/.config/flake8`, as [it accepts the same format](https://github.com/hhatto/autopep8#configuration), at least for the ignore flags.

The [flake8](flake8) file in this repository contains my actual flake8 config, and `~/.config/flake8` is just a symlink to `~/.spacemacs.d/flake8`. (So `~/.config/pep8` → `~/.config/flake8` → `~/.spacemacs.d/flake8`, which is the actual file.)


## Configuring essential development tools for JavaScript

(Yes, technically it's called [ECMAScript](https://www.ecma-international.org/ecma-262/), though everyone knows it as JS.)

Let's set up linting and autocompletion.

See [Getting started with ESLint](https://eslint.org/docs/user-guide/getting-started).

Roughly:

 - Install `npm` (`sudo apt install npm`) and then `npm init` your home directory.
   - Of course it's not really a package, but the `eslint --init` step below may crash without producing a configuration file if `package.json` does not exist. So let's play along.
   - See also [Node.js - installing packages](https://help.dreamhost.com/hc/en-us/articles/115004415628-Node-js-installing-packages).
 - `npm install eslint --save-dev` should work now without complaining.
 - Then you can `eslint --init` in your home directory to create a default configuration file (the wizard saves it as e.g. `~/.eslintrc.json`).
 - Create a symbolic link somewhere on your `PATH` so that Emacs will find the `eslint` executable: `ln -s ~/node_modules/eslint/bin/eslint.js ~/.local/bin/eslint`
 - [Eslint finds its config automatically](https://github.com/flycheck/flycheck/issues/1195), there's no Emacs-side config for that in flycheck.

Some of this may be nonsense, but that's how I got it working for now.

Then we can disable the very basic internal linter in `js2-mode`; the provided `config.el` does this automatically (so we're really assuming you have `eslint` installed).

To switch a specific ESLint rule off for a particular line, see [Disabling rules with inline comments](https://eslint.org/docs/user-guide/configuring#disabling-rules-with-inline-comments). See [ESLint Rules](https://eslint.org/docs/rules/) for the rule names. Basically, usually you'll want something like `// eslint-disable-line some-rule-name` or `// eslint-disable-next-line some-rule-name`. Yes, that's much more verbose than flake8's `noqa: Fxxx`. Tough luck. That's the JS community for you.

To get autocompletion, `npm install tern` and `ln -s ~/node_modules/tern/bin/tern ~/.local/bin/tern`. Spacemacs's `javascript` layer should take it from there, but in case it doesn't, see [Setting up Emacs for JavaScript](https://emacs.cafe/emacs/javascript/setup/2017/05/09/emacs-setup-javascript-2.html). The relevant minor mode is called `tern-mode`, pretty much as expected.


## Emacs, flyspell, English and Finnish

The only free spellchecker that works properly with Finnish, [Voikko](https://voikko.puimula.org/), runs on the [enchant](https://github.com/AbiWord/enchant) meta-spellchecker framework. Voikko does come with an `ispell` emulation layer called `tmispell` (Ubuntu package `tmispell-voikko`), but this is deprecated. To run Voikko in a multilingual environment with multiple different spellchecking engines, `enchant` is the currently recommended (and only) option.

**Use Enchant 2.2.4 or later.** Recent versions up to 2.2.3 have a [bug](https://github.com/AbiWord/enchant/issues/212) in the [tokenize_line](https://github.com/AbiWord/enchant/blob/master/src/enchant.c) function which will silently truncate any `ä` or `ö` at the end of a word (before sending the input to Voikko for actual spellchecking).

**Ubuntu 20.04 and later** provide the package `enchant-2`, so just `sudo apt install enchant-2 libenchant-2-voikko`. [In older ubuntu versions](https://bugs.launchpad.net/ubuntu/+source/enchant/+bug/1830336), the latest provided `enchant` is 1.6.0, where personal dictionary saving does not work.

For Finnish specifically, you'll also need `sudo apt install libvoikko1 voikko-fi`. If these backend packages are missing, Emacs will hang upon choosing Finnish spellchecking.

Enchant almost works with Emacs's `ispell.el`. However, `enchant` takes `hunspell`-style *locale names* (`-d fi_FI`) for choosing the dictionary, instead of `ispell`-style language names (`-d finnish`). Hence, **out-of-the-box it will only work with the default dictionary** (which requires no `-d` option). Without further configuration, when `flyspell-mode` is enabled, and `enchant` is set up as the `Ispell program`, trying to switch to a non-default dictionary (`M-m S d` in Spacemacs) causes Emacs to freeze. (`C-g` to `Quit` or `killall -s USR2 emacs` to invoke the Elisp debugger (see [here](https://emacs.stackexchange.com/questions/506/debugging-a-frozen-emacs)) will temporarily help, but the freeze will occur again.)

To use non-default dictionaries, put this in your `dotspacemacs/user-config` (in `.spacemacs.d/init.el`):

```elisp
  (setq ispell-local-dictionary "english")
  (setq ispell-local-dictionary-alist
        '(("english" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)  ; "English" is en_US
          ("american" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)
          ("british" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_UK") nil utf-8)
          ("finnish" "[[:alpha:]]" "[^[:alpha:]]" "['-]" t ("-d" "fi") nil utf-8)))
  (setq ispell-program-name "~/.local/bin/enchant")
```

(This is already included in the `init.el` provided in this repository.)

Add more languages if needed. Important parts are the `-d lang_VARIANT` and the `utf-8`. Emacs is overly conservative here - the default settings still assume iso-8859-1, which will break the handling of `ä` and `ö` in any 21st century setup where everything is utf-8.

**Full instructions** for Enchant + Voikko + Emacs, roughly:

 - To spellcheck Finnish, install Voikko.
   - In Debian-based distros, Voikko should be in the default repository, so just `sudo apt install` it. The packages needed are `libvoikko1`, `voikko-fi`.
 - To spellcheck languages other than Finnish, install Aspell and/or Hunspell and the relevant dictionaries, as desired. See e.g. `apt search hunspell`.
 - **If running on a distro that provides Enchant 2**, such as Ubuntu 20.04 or later:
   - `sudo apt install enchant-2 libenchant-2-voikko`
 - **If running on a distro that doesn't have Enchant 2**:
   - Before building Enchant, make sure you have installed the prerequisites. On Debian-based distros, this means `sudo apt install` at least `build-essentials` and `libaspell-dev`, `libhunspell-dev`, `libenchant-voikko`, `libvoikko-dev`.
     - If Enchant's `configure` complains or something looks not quite right, add more packages as needed, and then re-run the `configure` step.
   - Get the [Enchant](https://github.com/AbiWord/enchant) source code, release 2.2.4 or later.
     - If you prefer to build from git instead, install also GNU autotools.
     - Make sure `libtool` and `pkg-config` are installed **before** starting the build. If not, misleading error messages may occur (e.g. complaining about `GLIB` during `configure`, when the actual problem is that `PKG_CONFIG_MODULES` was not defined at the time when `bootstrap` was run).
     - Start the build by running `./bootstrap` to generate the `configure` script. If you're new to autotools, see [the manual](https://www.gnu.org/software/automake/manual/automake.html), esp. [section 2](https://www.gnu.org/software/automake/manual/automake.html#Autotools-Introduction), for the concepts and an overview.
   - Build and install Enchant. The build process is a rather standard `./configure`, `make`, `make install`. [LfS has some instructions](http://www.linuxfromscratch.org/blfs/view/cvs/general/enchant.html) just in case.
   - The goal is to get a config that supports Voikko for Finnish, and some other spellchecker(s) for other languages.
   - In the `configure` step, use `./configure --prefix=/home/myusername/.local --disable-static   --enable-relocatable` or some such to get an installation that won't conflict with anything installed from the distro's package manager.
     - This assumes you *have* a `~/.local/bin` directory, and that it's in your `PATH`. See your `~/.bashrc`, which may need something like `export PATH=/home/myusername/.local/bin:$PATH`
 - Make the above customization to your `.spacemacs.d/init.el`.
 - `M-x customize-group RET ispell RET`. Find the setting for `Ispell program` and change it to `~/.local/bin/enchant`. Set and save for future sessions (option `1`).

You should now have a multilingual Enchant with support for both English and Finnish, configured to work with Emacs.

Once Enchant is installed, to see what backends it has, `enchant-lsmod` in the terminal. You can also query the backend used for an individual language by the locale name. E.g. `enchant-lsmod -lang en_US` will tell you which backend Enchant will use to check American English. If everything went smoothly, the output of `enchant-lsmod -lang fi_FI` should mention Voikko.

To test, restart Emacs, open a new buffer in text mode (Spacemacs: `M-m b N n`, `M-x text-mode RET`), type some English, and spellcheck the buffer (`M-m S b`). Then open another new buffer in text mode, write some Finnish, switch the dictionary to Finnish (`M-m S d fin RET`), and spellcheck the buffer. Be sure to test also with words that end with an `ä` or `ö`, such as `tämä`, `häiriö` and `tietyö`.

### Personal dictionary

 - **With Enchant 2.2.4 or later**, saving new words to the personal dictionary with the `Save` option in `flyspell-correct-wrapper` (`M-m S c`) works for all languages.
   - The personal dictionary **does not work with the old Enchant 1.6.0**.
 - Note in case of Finnish, **Enchant 2.2.4 or later is needed**. Enchant 2.2.3 will not work correctly; see [issue 212](https://github.com/AbiWord/enchant/issues/212), which was fixed in 2.2.4.

### Why is Finnish so hard to spellcheck?

Complex morphology. It's an [agglutinative](https://en.wikipedia.org/wiki/Agglutinative_language) language. See [the classic example](https://satwcomic.com/aimlessly) (which Voikko clears without breaking a sweat). Also `tietokoneeseenikohan?` ("into my computer, I wonder?") is a valid construction - with a compound noun, and no less than *four* suffixes (-een: into, -ni: my, -ko: question, -han: I wonder).

This is why Aspell is simply not an option for Finnish. Even Hunspell only handles two suffixes. We really need Voikko and Enchant.
