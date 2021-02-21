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
- [PAM environment](#pam-environment)
- [tmux](.tmux.conf)
- [X resources](.Xresources)
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

### PAM environment

The [PAM environment file](.pam_environment) is used to set the environment for
the entire PAM session.

### Zsh

- A new version of Zsh is required – feel free to build [from source][zsh-repo].
- `ZDOTDIR` should be configured to point to `$HOME/.config/zsh`.
- `.zshrc` should come from [grml-etc-core][grml-etc-core-repo].


[git-homepage]: https://git-scm.com/ "Homepage of Git"
[grml-etc-core-repo]: https://github.com/grml/grml-etc-core "Git repository of grml-etc-core"
[gruvbox-repo]: https://github.com/briemens/gruvbox "Git repository of Gruvbox"
[stow-homepage]: https://www.gnu.org/software/stow/ "Homepage of GNU Stow"
[zsh-repo]: https://github.com/zsh-users/zsh "Git repository of Zsh"
