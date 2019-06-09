# spacemacs.d

My [Spacemacs](http://spacemacs.org/) configuration. Contains some useful custom functions (this being Emacs, just like everyone else's).

Any contributions from the internet are attributed in source code comments, as appropriate. All original content in this repository is published under [The Unlicense](LICENSE.md).

Potentially useful notes below.

## Emacs, flyspell, English and Finnish

The only free spellchecker that works properly with Finnish, [Voikko](https://voikko.puimula.org/), runs on the [enchant](https://github.com/AbiWord/enchant) meta-spellchecker framework. The Debian and Ubuntu packages of `enchant` are currently (2019/06) [eight years out of date](https://bugs.launchpad.net/ubuntu/+source/enchant/+bug/1830336).

Voikko also has an `ispell` emulation layer called `tmispell` (Ubuntu package `tmispell-voikko`), but this is deprecated. To run Voikko in a multilingual environment with multiple different spellchecking engines, `enchant` is the currently recommended (and only) option.

So here's what to do.

Enchant almost works with Emacs's `ispell.el`. However, because `enchant` takes `hunspell`-style *locale names* (`-d fi_FI`) for choosing the dictionary, instead of `ispell`-style language names (`-d finnish`), it will only work **with the default dictionary** (which requires no `-d` option). This is obviously suboptimal in a multilingual environment - I want spellchecking for both English and Finnish, since I use both languages frequently.

When `flyspell-mode` is enabled, and `enchant` is set up as the `Ispell program`, trying to switch to a non-default dictionary (`M-m S d` in Spacemacs) causes Emacs to freeze. (`C-g` to `Quit` or `killall -s USR2 emacs` to invoke the Elisp debugger will temporarily help, but the freeze will occur again.)

`enchant-wrapper` is a `bash` script that remedies the argument format incompatibility by translating command-line arguments, replacing a `-d` option for the language name with one for a locale name, while passing all other options through to `enchant`. It uses a hard-coded mapping, with the data copied from `ispell-dictionary-base-alist` in `/usr/share/emacs/site-lisp/dictionaries-common/ispell.el`. Using `~/.spacemacs.d/enchant-wrapper` instead of `enchant` itself as the `Ispell program` allows non-default dictionaries to work with `enchant` in Emacs.

To get it running in Emacs, roughly:

 - To spellcheck Finnish, install Voikko.
   - In Debian-based distros, Voikko should be in the default repository, so just `sudo apt install` it. The packages are `libvoikko1`, `voikko-fi`, and optionally others such as `python-libvoikko`, `python3-libvoikko` (if you Python and happen to want them - but for this, not needed).
 - To spellcheck languages other than Finnish, install Aspell and/or Hunspell and the relevant dictionaries, as desired. See e.g. `apt search hunspell`.
 - Build and install the latest release of [Enchant](https://github.com/AbiWord/enchant). The build process is a rather standard `./configure`, `make`, `make install`, but [LfS has some instructions](http://www.linuxfromscratch.org/blfs/view/cvs/general/enchant.html) just in case.
   - On Debian-based distros, make sure you first `sudo apt install` at least `build-essentials` and `libaspell-dev`, `libhunspell-dev`, `libenchant-voikko`, `libvoikko-dev`. If `configure` complains or something looks not quite right, add more packages as needed, and then re-run the `configure` step.
     - The goal is to get a config that supports Voikko for Finnish, and some other spellchecker(s) for other languages.
   - In the `configure` step, use `./configure --prefix=~/.local/bin --disable-static` or some such to get an installation that won't conflict with anything installed from the distro's package manager.
     - This assumes you *have* a `~/.local/bin` directory, and that it's in your `PATH`. See your `~/.bashrc`, which may need something like `export PATH=/home/youruser/.local/bin:$PATH`
 - `M-x customize-group RET ispell RET`. Find the setting for `Ispell program` and change it to `~/.spacemacs.d/enchant-wrapper`. Set and save for future sessions (option `1`).

You should now have a multilingual Enchant with support for both English and Finnish, configured to work with Emacs.

Once Enchant is installed, to see what backends it has, `enchant-lsmod` in the terminal. You can also query the backend used for an individual language by the locale name. E.g. `enchant-lsmod -lang en_US` will tell you which backend Enchant will use to check American English. If everything went smoothly, the output of `enchant-lsmod -lang fi_FI` should mention Voikko.

To test, restart Emacs, open a new buffer in text mode (Spacemacs: `M-m b N n`, `M-x text-mode RET`), type some English, and spellcheck the buffer (`M-m S b`). Then open another new buffer in text mode, write some Finnish, switch the dictionary to Finnish (`M-m S d fin RET`), and spellcheck the buffer.

### Personal dictionary

 - Saving new words to the personal dictionary with the `Save` option in `flyspell-correct-wrapper` (`M-m S c`) **should** work with Enchant 2.2.3.
   - Internally, this works by sending the `#` command to the spellchecker (which is running in `ispell` pipe mode). See `ispell-pdict-save` in `ispell.el`.
   - **This does not work with the old Enchant 1.6.0**.
 - However, so far I've only gotten personal dictionary saving to work **with the default dictionary**. When the Finnish dictionary is active, the relevant file in the `~/.config/enchant` directory is *sometimes* touched, but remains at zero size. No idea yet as for why.

### Why is Finnish so hard to spellcheck?

Complex morphology. It's an [agglutinative](https://en.wikipedia.org/wiki/Agglutinative_language) language. See [the classic example](https://satwcomic.com/aimlessly) (which Voikko clears without breaking a sweat). Also `tietokoneeseenikohan?` ("into my computer, I wonder?") is a valid construction - with a compound noun, and no less than *four* suffixes (-een: into, -ni: my, -ko: question, -han: I wonder).

This is why Aspell is simply not an option for Finnish. Even Hunspell only handles two suffixes. Enchant with Voikko really is the only free software that spellchecks Finnish properly.
