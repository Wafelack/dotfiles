# config.fish | Wafelack

function py
python3 $argv
end

function rl
source $HOME/.config/fish/config.fish
end

setxkbmap -option caps:escape
xmodmap -e "keysym Shift_R = Multi_key"

set GPG_TTY (tty)
export GPG_TTY
eval (ssh-agent -c)

set PATH /home/wafelack/.cargo/bin $PATH
set PATH "$PREFIX/bin" $PATH
set PATH /home/wafelack/.local/bin $PATH
set PATH /usr/local/bin/ $PATH
set PATH /sbin/ $PATH
set PATH /usr/local/texlive/2021/bin/x86_64-linux $PATH

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
echo (git_branch) (git_status)
if test $saved_status -eq 0
	echo (set_color green)"❯ "(set_color normal)
else
	echo (set_color red)"❯ "(set_color normal)
end
end

# Auto dotfiles linked
sed '/^#/d' ~/.config/.dotfiles/dotfiles.rdfm | sed -r '/^\s*$/d' | awk -F'=' '{ print $1 }' | rlgl -qs rdfm link

export TERM=xterm-256color
set PATH /home/wafelack/.quark $PATH
export QUARK=/home/wafelack/.quark
set PATH /usr/bin $PATH
export ORION_LIB=/usr/lib/orion
set PATH /home/wafelack/.ark $PATH
export ARKSCRIPT_PATH="/home/wafelack/.ark"
