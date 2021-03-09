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
- [`install-pkg`](#install-pkg)
- [`latexmk`](#latexmk)
- [PAM environment](#pam-environment)
- [tmux](.tmux.conf)
- [Vim](#vim)
- [X resources](.Xresources)
- [Zathura](.config/zathura/zathurarc)
- [Zsh](#zsh)

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

- A new version of Vim is required – feel free to build [from source][vim-repo].
- `.vimrc` should come from [grml-etc-core][grml-etc-core-repo].
- Local changes go to `.vimrc.local` instead.
- Included plugins:
  + <https://github.com/junegunn/fzf.vim>
  + <https://github.com/lervag/vimtex>

### Zsh

- A new version of Zsh is required – feel free to build [from source][zsh-repo].
- `ZDOTDIR` should be configured to point to `$HOME/.config/zsh`.
- `.zshrc` should come from [grml-etc-core][grml-etc-core-repo].
- Local changes to go `.zshrc.pre` and `.zshrc.local` instead.


[git-homepage]: https://git-scm.com/ "Homepage of Git"
[grml-etc-core-repo]: https://github.com/grml/grml-etc-core "Git repository of grml-etc-core"
[gruvbox-repo]: https://github.com/briemens/gruvbox "Git repository of Gruvbox"
[stow-homepage]: https://www.gnu.org/software/stow/ "Homepage of GNU Stow"
[vim-repo]: https://github.com/vim/vim "Git repository of Vim"
[zsh-repo]: https://github.com/zsh-users/zsh "Git repository of Zsh"
