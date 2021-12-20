set -U fish_greeting ""

# himalaya

function hl
	himalaya list
end

function hw
	himalaya write
end

function hr
	echo -n "* Creating temporary file..."
	set -l fname (mktemp)
	echo "done"
	echo -n "* Fetching mail content..."
	himalaya read $argv[1] > $fname
	echo "done"
	vim $fname
	set -l answer (read -P 'Do you want to delete the temporary file [y/n] ? ') 
	if test "$answer" = 'y'
		echo -n "* Deleting temporary file..."
		rm $fname
		echo "done"
	end
end

function visukey $argv
	screenkey --mods-mode emacs -f "Liberation Mono" --position bottom
end

function py
python3 $argv
end
function fuckit
git reset --hard HEAD
end
function rl
source $HOME/.config/fish/config.fish
end
function get_license
curl --silent www.gnu.org/licenses/gpl-3.0.txt -o COPYING
end
function c
xclip -selection clipboard
end

function start_ssh_agent
	if not set -q SSH_AGENT_PID
		eval (ssh-agent -c)
end
end

function nix-shell
	/run/current-system/sw/bin/nix-shell --command fish $argv
end

function transfer
	curl --progress-bar --upload-file $argv[1] http://transfer.sh/$argv[1]
end

set -gx GPG_TTY (tty)
set -gx EDITOR vim

set -x PATH $HOME/.cargo/bin $PATH
set -x PATH $HOME/.local/bin $PATH

function git_branch
	set branch (git branch 2> /dev/null | grep '^*' | colrm 1 2)
	if test -n "$branch"
		echo -n (set_color blue)"["(set_color green)$branch(set_color blue)"]"(set_color normal)
	end
end

function fish_prompt
	echo -n (set_color magenta)(whoami)(set_color blue)"@"(set_color purple)(hostname)
	echo -n (set_color blue)"["(set_color yellow)(prompt_pwd)(set_color blue)"]"(set_color normal)
	echo (set_color blue)"\$ "
end

function fish_right_prompt
	set s $status

	echo -n (git_branch)

	if set -q IN_NIX_SHELL
		echo -n (set_color blue)" ("(set_color yellow)"nix-shell"(set_color blue)")"
	end
	if test $s != 0
		echo -n (set_color red) $s(set_color normal)
	end
end

if status is-interactive
	start_ssh_agent
end
