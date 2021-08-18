#!/usr/bin/env sh
link_folder() {
    echo -n "Linking $1 ... "
    stow $1
    echo "done"
}

link_folder scripts/
link_folder vim/
link_folder bash/
link_folder X/
