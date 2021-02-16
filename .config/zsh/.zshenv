# -*- mode: sh; -*-

typeset -gxTU LD_LIBRARY_PATH ld_library_path
typeset -gxTU PKG_CONFIG_PATH pkg_config_path
typeset -gxTU PYTHONPATH pythonpath

typeset -a components=(
    cargo
    go
    pyenv
    rbenv
    grass74
    instantclient_21_1
    airflow
    lesspipe
)

## * Declare how to set various components up

## ** Cargo (https://doc.rust-lang.org/cargo/)
function setup_cargo {
    source ${CARGO_HOME:-$HOME/.cargo}/env
}

## ** Go (https://golang.org/)
function setup_go {
    [[ -v commands[go] ]] || return 1
    path+=$( go env GOPATH )/bin
    typeset -gx FZF_HOME
    FZF_HOME=$( go list -f '{{.Dir}}' 'github.com/junegunn/fzf' ) || {
        unset FZF_HOME
        return 1
    }
    if [[ -v commands[fd] ]] ; then
        typeset -gx FZF_DEFAULT_COMMAND='command fd -tf'                   \
                    FZF_ALT_C_COMMAND='command fd --min-depth 1 -L -H -td'
    fi
}

## ** pyenv (https://github.com/pyenv/pyenv)
function setup_pyenv {
    typeset -gx PYENV_ROOT=$HOME/.pyenv
    [[ -d $PYENV_ROOT ]] || {
        unset PYENV_ROOT
        return 1
    }
    typeset -gx PYTHON_CONFIGURE_OPTS='--enable-shared'
    typeset -TU PYENV_PREFIX pyenv_prefix
    path+=$PYENV_ROOT/bin
    PYENV_PREFIX=$( pyenv prefix )
    ld_library_path+=( ${^pyenv_prefix}/lib )
    pkg_config_path+=( ${^pyenv_prefix}/lib/pkgconfig )
    unset PYENV_PREFIX pyenv_prefix
    eval "$( pyenv init - )"
    source $( pyenv which virtualenvwrapper.sh )
}

## ** rbenv (https://github.com/rbenv/rbenv)
function setup_rbenv {
    typeset -gx RBENV_ROOT=$HOME/.rbenv
    [[ -d $RBENV_ROOT ]] || {
        unset RBENV_ROOT
        return 1
    }
    path+=$RBENV_ROOT/bin
    eval "$( rbenv init - )"
}

## ** GRASS (https://grass.osgeo.org/)
function setup_grass74 {
    typeset -gx GISBASE=/usr/lib/grass74
    [[ -d $GISBASE ]] || {
        unset GISBASE
        return 1
    }
    path+=$GISBASE/bin
    ld_library_path+=$GISBASE/lib
    pythonpath+=$GISBASE/etc/python
}

## ** SQL*Plus (https://www.orafaq.com/wiki/)
function setup_instantclient_21_1 {
    local instantclient_home=/opt/instantclient_21_1
    [[ -d $instantclient_home ]] || return 1
    path+=$instantclient_home
    ld_library_path+=$instantclient_home
    typeset -gx TNS_ADMIN=$HOME
}

## ** Apache Airflow (https://airflow.apache.org/)
function setup_airflow {
    typeset -gx AIRFLOW_HOME=$HOME/airflow
    [[ -d $AIRFLOW_HOME ]] || {
        unset AIRFLOW_HOME
        return 1
    }
    typeset -gx AIRFLOW__CORE__DAGS_FOLDER=$AIRFLOW_HOME/dags           \
                AIRFLOW__LOGGING__BASE_LOG_FOLDER=$AIRFLOW_HOME/logs
    pythonpath+=$AIRFLOW__CORE__DAGS_FOLDER
}

## ** lesspipe (https://www-zeuthen.desy.de/~friebel/unix/lesspipe.html)
function setup_lesspipe {
    [[ -v commands[lesspipe] ]] && eval "$( lesspipe )"
}

## * Run the setup for all enabled components
for component in $components ; do
    setup_$component || printf 'Could not set "%s" up\n' "$component"
done

## * Add local paths to LD_LIBRARY_PATH
ld_library_path+=$HOME/.local/lib

## * Add local paths to PKG_CONFIG_PATH
pkg_config_path+=($HOME/.local/lib/pkgconfig
                  $HOME/.local/lib/x86_64-linux-gnu/pkgconfig)

## * Disable telemetry for .NET Core
typeset -gx DOTNET_CLI_TELEMETRY_OPTOUT=true
