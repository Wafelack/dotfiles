#!/bin/sh
set -euo pipefail

## Script to install all needed* packages for Calculate Linux
## By Wafelack <wafelack@protonmail.com>
## Licensed under the GNU General Public License v3.0+
##
## *Needed for my needs, you might have other needs based on your domain, feel free to modify this script in order to fit your needs.

if [[ $EUID -ne 0 ]]
then
	echo "This script needs to be run as administrator."
	exit 1
fi

install_package() {
	echo "* Installing ${1}..."
	emerge --quiet $1
	echo "* Sucessfully installed $1."
}

cargo_install() {
	echo "* Installing ${1}"
	cargo install ${1}
	echo "* Successfully installed $1."
}

install_rad() {
	echo "* Installing rad..."
	git clone https://github.com/wafelack/rad.git
	cd rad/
	chmod +x ./configure
	./configure
	make
	make install
	cd ../
	rm -frd rad
	echo "* Successfully installed rad."
}

install_rdfm() {
	echo "* Installing rfm..."
	git clone https://github.com/wafelack/rdfm.git
	cd rdfm/
	./configure
	make
	make install
	cd ../
	rm -frd rdfm
	echo "* Successfully installed rdfm."
}

install_rust() {
	curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh	
}

install_dotfiles() {
	git clone https://github.com/Wafelack/dotfiles.git
	mv dotfiles/ ~/.config/.dotfiles/
	rdfm install
}

install_package "x11-wm/i3-gaps"
install_package "x11-misc/i3blocks"
install_package "x11-misc/rofi"
install_package "x11-terms/alacritty"
install_package "app-editors/vim"
install_package "www-client/firefox"
install_package "app-shells/fish"
install_package "media-fonts/fira-code"
install_package "media-gfx/inkscape"
install_package "media-gfx/flameshot"
install_package "x11-misc/i3lock"
install_package "app-text/texlive"
install_package "dev-tex/latexmk"
install_rust
install_rad
install_rdfm
install_dotfiles
cargo_install "zellij"
