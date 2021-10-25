#!/usr/bin/env sh

link_folder() {
    local from="$1"
    local to=$([[ $# -lt 2 ]] && echo "$HOME" || echo "$2")

    echo -n "Linking ${from} ... "

    local runner='doas'
    if [ -w "${to}" ]
    then
        runner=''
    fi

    ${runner} stow ${from} -t ${to}

    echo "done"
}

link_folder nix/ /etc/nixos/
link_folder scripts/
link_folder vim/
link_folder X/
link_folder ghci/
link_folder fish/ ${HOME}/.config/fish/
link_folder stumpwm/ ${HOME}/.stumpwm.d/
link_folder git/
