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

function transfer
    curl --progress-bar --upload-file $argv[1] http://transfer.sh/$argv[1]
end

set -gx GPG_TTY (tty)
set -gx EDITOR vim

fish_add_path $HOME/.cargo/bin
fish_add_path $HOME/.local/bin
fish_add_path $HOME/.scripts
fish_add_path /usr/local/bin
fish_add_path /usr/local/sbin
fish_add_path /sbin

function git_branch
    set branch (git branch 2> /dev/null | grep '^*' | colrm 1 2)
    if test -n "$branch"
        echo -n (set_color blue)"("(set_color green)$branch(set_color blue)")"(set_color normal)
    end
end

function fish_prompt
    set s $status
    if test $s != 0
        echo -n (set_color red)$s (set_color normal)
    end
    echo -n (set_color magenta)(whoami)(set_color blue)"@"(set_color purple)(hostname)
    echo -n (set_color blue)"["(set_color yellow)(prompt_pwd)(set_color blue)"]"(set_color normal)
    echo -n (git_branch)
    echo (set_color blue)"\$ "
end

if status is-interactive
    start_ssh_agent
end
