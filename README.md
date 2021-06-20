# d125q’s configuration files and scripts

This repository gathers configuration files and scripts for various pieces of
software that I use.

## Installation

Typically, one would use [Git][git-homepage] to clone the repository and [GNU
Stow][gnu-stow-homepage] to install it.  A simple installation would consist of
the following steps:

```console
$ mkdir -p $HOME/stow
$ cd $HOME/stow
$ git clone -b master --depth 1 'git@github.com:d125q/d125q-config.git'
$ stow d125q-config
```

## Custom scripts

- [`d125q-install-pkg`](#d125q-install-pkg)

### `d125q-install-pkg`

[`d125q-install-pkg`](.local/bin/d125q-install-pkg) is a script to build and
install packages locally.  It relies on [GNU Stow][gnu-stow-homepage].

``` console
$ d125q-install-pkg -h
Usage: d125q-install-pkg [OPTION]... PATTERN...
Install packages whose names match PATTERN from their local
repositories.
Options:
    -h, -help          display this help and exit
    -x, -xtrace        enable tracing
    -j N, -parallel N  use up to N concurrent jobs when building
                       and installing packages
    -noupdate          do not update any source code from the remote
                       repositories
    -nobuild           do not build any packages
    -nostow            do not stow any installed files
    -pkgstowdir DIR    stow directory to install packages to
                           (default: $HOME/.local/stow)
    -cfgstowdir DIR    stow directory to install configuration files to
                           (default $HOME/stow)
    -srcrootdir DIR    directory to download source code to
                           (default: $HOME/.local/src)
    -patchrootdir DIR  directory to include patches from
                           (default: $HOME/.local/patches)
    -altdir DIR        directory containing alternatives
                           (default: $HOME/Alternatives)
    -admindir DIR      directory containing administrative information
                           (default: $HOME/.var/lib/dpkg)
```

## Components

- [Alacritty](.config/alacritty/alacritty.yml)
- [fontconfig](.config/fontconfig)
- [GNU Emacs](#gnu-emacs)
- [`latexmk`](#latexmk)
- [PAM environment](#pam-environment)
- [tmux](.tmux.conf)
- [Vim](#vim)
- [X resources](.Xresources)
- [Zathura](.config/zathura/zathurarc)
- [Zsh](#zsh)

### GNU Emacs

- A recent version of GNU Emacs is required – feel free to build [from
  source][gnu-emacs-repo].  ([`d125q-install-pkg`](#d125q-install-pkg) can also
  help.)
- The configuration is pretty big and opinionated.  It tries to follow the
  official [key binding conventions][gnu-emacs-kbd-convs-doc].
  + [Ido][ido-doc] is used in conjunction with [Amx][amx-repo] and kept on the
    default key sequences.  It is primarily meant for operations where one
    already has a good overview of things and can get away with using the
    minibuffer.
  + For more elaborate scenarios, [Helm][helm-repo] is configured and uses the
    <kbd>Hyper</kbd> modifier key.  (And <kbd>Caps Lock</kbd> is set to act as
    <kbd>Hyper</kbd>.)  For example, `helm-M-x` is bound to <kbd>H-x</kbd>,
    `helm-find-files` to <kbd>H-f</kbd>, and `helm-buffers-list` to
    <kbd>H-b</kbd>.
  + Packages other than [Helm][helm-repo] use the <kbd>Super</kbd> modifier
    key.  For example, `deadgrep` and `rg-menu` are bound to <kbd>s-[</kbd> and
    <kbd>s-]</kbd>, respectively.
  + Custom key bindings for built-in commands use the usual <kbd>C-c
    letter</kbd> key sequences or <kbd>F5</kbd> through <kbd>F9</kbd> without
    any modifier keys.  (Where <kbd>letter</kbd> is either upper or lower case.)
    For example, `ibuffer` is bound to <kbd>C-c b</kbd>.
  + Key sequences used by the window manager should not be touched by GNU Emacs
    and vice versa.
- Code should be autoloaded whenever possible.
- The [`lisp` directory](.config/emacs/lisp) contains a few custom libraries
  that are required.  To extract autoloads:
  ```console
  $ cd .config/emacs/lisp
  $ make
  EMACSLOADPATH= 'emacs' --batch --no-site-file --no-site-lisp -l autoload \
  --eval '(setq autoload-ensure-writable t)' \
  --eval '(setq generated-autoload-file "d125q-loaddefs.el")' \
  -f batch-update-autoloads .
    SCRAPE   .
    INFO     Scraping files for d125q-loaddefs.el...
    INFO     Scraping files for d125q-loaddefs.el...done
  ```
  If the autoloads are not present, the files will be loaded eagerly.  To clean
  the autoloads, run `make clean`.
- GNU Emacs should be run as a daemon using its systemd service.  There is
  additionally a [drop-in](.config/systemd/user/emacs.service.d/override.conf)
  to set the correct environment.

### `latexmk`

[`latexmk`](.config/latexmk/latexmkrc) is set to use LuaLaTeX by default.
Furthermore, it is set to run it with the following options:

- `--interaction=nonstopmode` to run without interaction.
- `--file-line-error` to print errors in the form `file:line:error`.
- `--shell-escape` to enable the `\write18{command}` construct which is required
  by packages such as [minted](https://ctan.org/pkg/minted?lang=en).
- `--synctex=1` to enable SyncTeX.

### PAM environment

The [PAM environment file](.pam_environment) is used to set the environment for
the entire PAM session.

### Vim

- A recent version of Vim is required – feel free to build [from
  source][vim-repo].  ([`d125q-install-pkg`](#d125q-install-pkg) can also help.)
- `.vimrc` should come from [grml-etc-core][grml-etc-core-repo].
- Local changes go to `.vimrc.local` instead.
- Included plugins:
  + <https://github.com/junegunn/fzf.vim>
  + <https://github.com/lervag/vimtex>

### Zsh

- A recent version of Zsh is required – feel free to build [from
  source][zsh-repo].  ([`d125q-install-pkg`](#d125q-install-pkg) can also help.)
- `ZDOTDIR` should be configured to point to `$HOME/.config/zsh`.
- `.zshrc` should come from [grml-etc-core][grml-etc-core-repo].
- Local changes to go `.zshrc.pre` and `.zshrc.local` instead.


[git-homepage]: https://git-scm.com/ "Homepage of Git"
[gnu-stow-homepage]: https://www.gnu.org/software/stow/ "Homepage of GNU Stow"
[gnu-emacs-repo]: https://github.com/emacs-mirror/emacs "Git repository of GNU Emacs"
[gnu-emacs-kbd-convs-doc]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html "Key bindings conventions for GNU Emacs"
[ido-doc]: https://www.gnu.org/software/emacs/manual/html_mono/ido.html "HTML documentation for Ido"
[amx-repo]: https://github.com/DarwinAwardWinner/amx "Git repository of Amx"
[helm-repo]: https://github.com/emacs-helm/helm/ "Git repository of Helm"
[grml-etc-core-repo]: https://github.com/grml/grml-etc-core "Git repository of grml-etc-core"
[gruvbox-repo]: https://github.com/briemens/gruvbox "Git repository of Gruvbox"
[vim-repo]: https://github.com/vim/vim "Git repository of Vim"
[zsh-repo]: https://github.com/zsh-users/zsh "Git repository of Zsh"
