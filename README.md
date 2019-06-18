# spacemacs.d

My [Spacemacs](http://spacemacs.org/) configuration. Contains some useful custom functions (this being Emacs, just like everyone else's).

Any contributions from the internet are attributed in source code comments, as appropriate. All original content in this repository is published under [The Unlicense](LICENSE.md).

Potentially useful notes below.

## Emacs, flyspell, English and Finnish

The only free spellchecker that works properly with Finnish, [Voikko](https://voikko.puimula.org/), runs on the [enchant](https://github.com/AbiWord/enchant) meta-spellchecker framework. Voikko does come with an `ispell` emulation layer called `tmispell` (Ubuntu package `tmispell-voikko`), but this is deprecated. To run Voikko in a multilingual environment with multiple different spellchecking engines, `enchant` is the currently recommended (and only) option.

The Debian and Ubuntu packages of `enchant` are currently (2019/06) [eight years out of date](https://bugs.launchpad.net/ubuntu/+source/enchant/+bug/1830336). In the old version, personal dictionary saving does not work.

Recent versions of Enchant, on the other hand, handle the personal dictionary correctly, but up to version 2.2.3 have a [bug](https://github.com/AbiWord/enchant/issues/212) in the [tokenize_line](https://github.com/AbiWord/enchant/blob/master/src/enchant.c) function which will silently truncate any `ä` or `ö` at the end of a word (before sending the input to Voikko for actual spellchecking). **Update 2019-06-18: Fixed in Enchant 2.2.4, use that!**

Enchant almost works with Emacs's `ispell.el`. However, `enchant` takes `hunspell`-style *locale names* (`-d fi_FI`) for choosing the dictionary, instead of `ispell`-style language names (`-d finnish`). Hence, **out-of-the-box it will only work with the default dictionary** (which requires no `-d` option). Without further configuration, when `flyspell-mode` is enabled, and `enchant` is set up as the `Ispell program`, trying to switch to a non-default dictionary (`M-m S d` in Spacemacs) causes Emacs to freeze. (`C-g` to `Quit` or `killall -s USR2 emacs` to invoke the Elisp debugger (see [here](https://emacs.stackexchange.com/questions/506/debugging-a-frozen-emacs)) will temporarily help, but the freeze will occur again.)

To use non-default dictionaries, put this in your `dotspacemacs/user-config` (in `.spacemacs.d/init.el`):

```elisp
  (setq ispell-local-dictionary "english")
  (setq ispell-local-dictionary-alist
        '(("english" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)  ;; "English" is en_US
          ("american" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)
          ("british" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_UK") nil utf-8)
          ("finnish" "[[:alpha:]]" "[^[:alpha:]]" "['-]" t ("-d" "fi") nil utf-8)))
  (setq ispell-program-name "~/.local/bin/enchant")
```

Add more languages if needed. Important parts are the `-d lang_VARIANT` and the `utf-8`. Emacs is overly conservative here - the default settings still assume iso-8859-1, which will break the handling of `ä` and `ö` in any 21st century setup where everything is utf-8.

**Full instructions** for Enchant + Voikko + Emacs, roughly:

 - To spellcheck Finnish, install Voikko.
   - In Debian-based distros, Voikko should be in the default repository, so just `sudo apt install` it. The packages needed are `libvoikko1`, `voikko-fi`.
 - To spellcheck languages other than Finnish, install Aspell and/or Hunspell and the relevant dictionaries, as desired. See e.g. `apt search hunspell`.
 - Before building Enchant, make sure you have installed the prerequisites. On Debian-based distros, this means `sudo apt install` at least `build-essentials` and `libaspell-dev`, `libhunspell-dev`, `libenchant-voikko`, `libvoikko-dev`.
   - If Enchant's `configure` complains or something looks not quite right, add more packages as needed, and then re-run the `configure` step.
 - Get the Enchant [Enchant](https://github.com/AbiWord/enchant) source code, release 2.2.4 or later.
   - If you prefer to build from git instead, install also GNU autotools. Start the build by running `./bootstrap` to generate the `configure` script. If you're new to autotools, see [the manual](https://www.gnu.org/software/automake/manual/automake.html), esp. [section 2](https://www.gnu.org/software/automake/manual/automake.html#Autotools-Introduction), for the concepts and an overview.
 - Build and install Enchant. The build process is a rather standard `./configure`, `make`, `make install`. [LfS has some instructions](http://www.linuxfromscratch.org/blfs/view/cvs/general/enchant.html) just in case.
   - The goal is to get a config that supports Voikko for Finnish, and some other spellchecker(s) for other languages.
 - In the `configure` step, use `./configure --prefix=/home/myusername/.local --disable-static --enable-relocatable` or some such to get an installation that won't conflict with anything installed from the distro's package manager.
   - This assumes you *have* a `~/.local/bin` directory, and that it's in your `PATH`. See your `~/.bashrc`, which may need something like `export PATH=/home/myusername/.local/bin:$PATH`
 - Make the above customization to your `.spacemacs.d/init.el`.
 - `M-x customize-group RET ispell RET`. Find the setting for `Ispell program` and change it to `~/.local/bin/enchant`. Set and save for future sessions (option `1`).

You should now have a multilingual Enchant with support for both English and Finnish, configured to work with Emacs.

Once Enchant is installed, to see what backends it has, `enchant-lsmod` in the terminal. You can also query the backend used for an individual language by the locale name. E.g. `enchant-lsmod -lang en_US` will tell you which backend Enchant will use to check American English. If everything went smoothly, the output of `enchant-lsmod -lang fi_FI` should mention Voikko.

To test, restart Emacs, open a new buffer in text mode (Spacemacs: `M-m b N n`, `M-x text-mode RET`), type some English, and spellcheck the buffer (`M-m S b`). Then open another new buffer in text mode, write some Finnish, switch the dictionary to Finnish (`M-m S d fin RET`), and spellcheck the buffer. Be sure to test also with words that end with an `ä` or `ö`, such as `tämä`, `häiriö` and `tietyö`.

### Personal dictionary

 - **With Enchant 2.2.4**, saving new words to the personal dictionary with the `Save` option in `flyspell-correct-wrapper` (`M-m S c`) works for all languages.
   - The personal dictionary **does not work with Enchant 1.6.0** that is currently in Debian/Ubuntu repos.
 - Note in case of Finnish, **Enchant 2.2.4 is needed**. Enchant 2.2.3 will not work correctly; see [issue 212](https://github.com/AbiWord/enchant/issues/212), which was fixed in 2.2.4.

### Why is Finnish so hard to spellcheck?

Complex morphology. It's an [agglutinative](https://en.wikipedia.org/wiki/Agglutinative_language) language. See [the classic example](https://satwcomic.com/aimlessly) (which Voikko clears without breaking a sweat). Also `tietokoneeseenikohan?` ("into my computer, I wonder?") is a valid construction - with a compound noun, and no less than *four* suffixes (-een: into, -ni: my, -ko: question, -han: I wonder).

This is why Aspell is simply not an option for Finnish. Even Hunspell only handles two suffixes. We really need Voikko and Enchant.
