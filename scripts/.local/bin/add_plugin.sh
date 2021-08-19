#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 2 ]]
then
    echo "Usage: $0 <REPOSITORY> <PLUGIN_NAME>." >> /dev/stderr
    exit 1
fi

if [ -z ${VIM_PLUGINS_FOLDER+x} ]
then
   VIM_PLUGINS_FOLDER="${HOME}/.dotfiles/vim/.vim/plugin"
fi

cd $VIM_PLUGINS_FOLDER
git clone --quiet $1 $2
git submodule add $1 $2
cd -
