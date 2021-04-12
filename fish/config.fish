# config.fish | Wafelack

# aliases
function py
python3 $argv
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
set PATH $DENO_INSTALL/bin $PATH
set PATH /home/wafelack/.cargo/bin $PATH
set PATH "$PREFIX/bin" $PATH
set PATH /home/wafelack/.local/bin $PATH
set PATH /usr/local/bin/ $PATH
set PATH /usr/local/texlive/2021/bin/x86_64-linux $PATH

# Prompt

function git_branch
set -l is_git (git rev-parse --is-inside-work-tree 2> /dev/null)

switch $is_git
case 'true'
	echo -n "on "(set_color red)" "(git branch --show-current)(set_color normal)
end
end

function git_status
set -l is_git (git rev-parse --is-inside-work-tree 2> /dev/null)

switch $is_git
case 'true'
	set -l output (git status -uno -b -s | head -n 1 | cut -d "[" -f2 | cut -d "]" -f1)
	set -l type (echo $output | awk '{print $1}')
	set -l how_many (echo $output | awk '{print $2}')
	set -l even (echo $output | string match '## *')

	if test "$even" = ""
		if test "ahead" = $type
			echo -n "["(set_color red)"+"$how_many(set_color normal)"]"
		else
			echo -n "["(set_color red)"-"$how_many(set_color normal)"]"
		end
	end
end
end

function fish_prompt
set -l saved_status $status
echo -n (set_color cyan)(prompt_pwd) (set_color normal)
echo (git_branch) (git_status)

if test $saved_status -eq 0
	echo (set_color green)"➜ "(set_color normal)
else
	echo (set_color red)"➜ "(set_color normal)
end
end

# Variables
export TERM=xterm-256color
