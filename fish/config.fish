# config.fish | Wafelack

# aliases
function py
	python3 $argv
end

function please
	doas $argv
end

function rtfm
	man $argv
end

function rl
source $HOME/.config/fish/config.fish
end

# Ssh keys management
eval (ssh-agent -c)

# Gpg
set GPG_TTY (tty)
export GPG_TTY

# Cross compiler
	set PREFIX "/home/wafelack/.cross/bin/"
	set TARGET i686-elf

# Path
	set PATH /home/wafelack/ $PATH
	set PATH $DENO_INSTALL/bin $PATH
	set PATH /home/wafelack/.werb_bin $PATH
	set PATH /home/wafelack/.lua $PATH
	set PATH /home/wafelack/.gem/ruby/2.7.0/bin $PATH
	set PATH /home/wafelack/.cargo/bin $PATH
	set PATH "$PREFIX/bin" $PATH
	set PATH /home/wafelack/.local/bin $PATH

	function fish_prompt
set -l is_git (git rev-parse --is-inside-work-tree 2> /dev/null)
	switch $is_git
	case 'true'
	echo -n '['(set_color yellow)(git branch --show-current)(set_color normal)'] '
	end
	echo (set_color cyan)(prompt_pwd)(set_color normal) '$ '
	end

# Remap escape to caps lock

	setxkbmap -option caps:escape

# Variables
	export TERM=xterm-256color
