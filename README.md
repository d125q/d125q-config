# d125q’s configuration files

This repository gathers my configuration files for various software
that I use.

## Installation

Typically, one would use [Git][git-homepage] to clone the repository
and [GNU Stow][stow-homepage] to install it.  A simple installation
would consist of the following:

```console
$ mkdir -p $HOME/stow
$ cd $HOME/stow
$ git clone -b master --depth 1 'git@github.com:d125q/d125q-config.git'
$ stow d125q-config
```

[git-homepage]: https://git-scm.com/ "Homepage of Git"
[stow-homepage]: https://www.gnu.org/software/stow/ "Homepage of GNU Stow"

## Components

There are configuration files for the following components:

- The [PAM environment](#pam-environment)
- [Zsh](#zsh)
- [Alacritty](#alacritty)

### PAM environment

The [PAM environment file](.pam_environment) is used to set the
environment for the entire PAM session.  Currently, it sets:

- `QT_QPA_PLATFORMTHEME` to `qt5ct`
- `ZDOTDIR` to `$HOME/.config/zsh`

### Zsh

1. A new version of Zsh is required – feel free to build [from
   source][zsh-repo]
2. `ZDOTDIR` should be configured to point to `$HOME/.config/zsh` as
   in [PAM environment](#pam-environment)
3. `.zshrc` should come from [grml-etc-core][grml-etc-core-repo]

[zsh-repo]: https://github.com/zsh-users/zsh "Git repository of Zsh"
[grml-etc-core-repo]: https://github.com/grml/grml-etc-core "Git repository of grml-etc-core"

### Alacritty

1. Sets the `TERM` variable to `xterm-256` color
2. Runs `zsh`
3. Requires a custom build of [Iosevka][iosevka-repo], called `Iosevka
   Custom Condensed`
4. Configures the colors to a dark variant of [Gruvbox][gruvbox-repo]

[iosevka-repo]: https://github.com/be5invis/Iosevka "Git repository of Iosevka"
[gruvbox-repo]: https://github.com/briemens/gruvbox "Git repository of Gruvbox"
