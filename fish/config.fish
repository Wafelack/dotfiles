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
ssh-add -t 1h

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
	echo -n (set_color -b red)(set_color blue)""(set_color black) " "(git branch --show-current)
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
			echo -n (set_color -b red)"+"$how_many (set_color normal)(set_color -o red)""(set_color normal)
		else
			echo -n (set_color -b red)"-"$how_many (set_color normal)(set_color -o red)""(set_color normal)
		end
	else
		echo -n (set_color -b normal)(set_color red -o)""
	end
case '*'
	echo -n (set_color -b normal)(set_color blue -o)""
end
end

function fish_prompt
set -l saved_status $status
echo -n (set_color normal)(set_color -b blue) (set_color black)(prompt_pwd)" "
# echo -n (set_color cyan)(prompt_pwd) (set_color normal)
echo (git_branch) (git_status)

if test $saved_status -eq 0
	echo (set_color green)"❯ "(set_color normal)
else
	echo (set_color red)"❯ "(set_color normal)
end
end

# Variables
export TERM=xterm-256color
set PATH /home/wafelack/.quark $PATH
export QUARK=/home/wafelack/.quark
set PATH /home/wafelack/.orion/bin $PATH
export ORION_LIB=/home/wafelack/.orion/lib//orion
