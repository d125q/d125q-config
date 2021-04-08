# d125q’s configuration files and scripts

This repository gathers configuration files and scripts for various pieces of
software that I use.

## Installation

Typically, one would use [Git][git-homepage] to clone the repository and [GNU
Stow][stow-homepage] to install it.  A simple installation would consist of the
following steps:

```console
$ mkdir -p $HOME/stow
$ cd $HOME/stow
$ git clone -b master --depth 1 'git@github.com:d125q/d125q-config.git'
$ stow d125q-config
```

## Components

- [Alacritty](.config/alacritty/alacritty.yml)
- [fontconfig](.config/fontconfig)
- [GNU Emacs](#gnu-emacs)
- [`install-pkg`](#install-pkg)
- [`latexmk`](#latexmk)
- [PAM environment](#pam-environment)
- [tmux](.tmux.conf)
- [Vim](#vim)
- [X resources](.Xresources)
- [Zathura](.config/zathura/zathurarc)
- [Zsh](#zsh)

### GNU Emacs

- A recent version of GNU Emacs is required – feel free to build [from
  source][gnu-emacs-repo].  (`install-pkg` can also help.)
- The configuration is pretty big and opinionated.  It tries to follow the
  official [key binding conventions][kbd-convs-doc].
  + [Ido][ido-doc] is used in conjunction with [Amx][amx-repo] and kept on the
    default key sequences.  It is primarily meant for operations where one
    already has a good overview of things and can get away with using the
    minibuffer.
  + For more elaborate scenarios, [Helm][helm-repo] is configured and bound to
    the usual <kbd>C-c letter</kbd> user-reserved key sequences.  (Where
    <kbd>letter</kbd> is either upper or lower case.)  For example, `helm-M-x`
    is bound to <kbd>C-c x</kbd>, `helm-find-files` to <kbd>C-c f</kbd>, and
    `helm-buffers-list` to <kbd>C-c b</kbd>.
  + Other custom key bindings use either <kbd>F5</kbd> through <kbd>F9</kbd>
    without any modifier keys or the <kbd>Super</kbd> modifier key.
  + Key sequences used by the window manager should not be touched by GNU Emacs
    and vice versa.
- Code should be autoloaded as much as possible.
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
- GNU Emacs should be run as a daemon using its systemd service.  There is
  additionally a [drop-in](.config/systemd/user/emacs.service.d/override.conf)
  to set the correct environment.

### `install-pkg`

[`install-pkg`](.local/bin/install-pkg) is a script to build and install
packages locally.  It relies on [GNU Stow][stow-homepage].

```
Usage: install-pkg [OPTION]... PATTERN...
Install packages whose names match PATTERN from their local repositories.
Options:
    -noupdate      do not update from the remote repositories
    -nobuild       do not build the packages
    -nostow        do not stow the packages or their configuration files
    -pkgdir DIR    directory where packages will be installed
                       (default: $HOME/.local/stow)
    -cfgdir DIR    directory where configuration files will be installed
                       (default: $HOME/stow)
    -srcdir DIR    directory containing the local repositories
                       (default: $HOME/.local/src)
    -altdir DIR    directory containing the alternatives
                       (default: $HOME/Alternatives)
    -admindir DIR  directory containing the administrative information
                       (default: $HOME/.var/lib/dpkg)
```

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
  source][vim-repo].  (`install-pkg` can also help.)
- `.vimrc` should come from [grml-etc-core][grml-etc-core-repo].
- Local changes go to `.vimrc.local` instead.
- Included plugins:
  + <https://github.com/junegunn/fzf.vim>
  + <https://github.com/lervag/vimtex>

### Zsh

- A recent version of Zsh is required – feel free to build [from
  source][zsh-repo].  (`install-pkg` can also help.)
- `ZDOTDIR` should be configured to point to `$HOME/.config/zsh`.
- `.zshrc` should come from [grml-etc-core][grml-etc-core-repo].
- Local changes to go `.zshrc.pre` and `.zshrc.local` instead.


[amx-repo]: https://github.com/DarwinAwardWinner/amx "Git repository of Amx"
[git-homepage]: https://git-scm.com/ "Homepage of Git"
[gnu-emacs-repo]: https://github.com/emacs-mirror/emacs "Git repository of GNU Emacs"
[grml-etc-core-repo]: https://github.com/grml/grml-etc-core "Git repository of grml-etc-core"
[gruvbox-repo]: https://github.com/briemens/gruvbox "Git repository of Gruvbox"
[helm-repo]: https://github.com/emacs-helm/helm/ "Git repository of Helm"
[ido-doc]: https://www.gnu.org/software/emacs/manual/html_mono/ido.html "HTML documentation for Ido"
[kbd-convs-doc]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html "Key bindings conventions for GNU Emacs"
[stow-homepage]: https://www.gnu.org/software/stow/ "Homepage of GNU Stow"
[vim-repo]: https://github.com/vim/vim "Git repository of Vim"
[zsh-repo]: https://github.com/zsh-users/zsh "Git repository of Zsh"
