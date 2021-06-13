alias py='python3'
alias fuckit="git reset --heard \$(git log --oneline | head -n 1 |awk -F' ' '{ print \$1 }')"
alias rl='source $HOME/.bashrc'

setxkbmap -option caps:escape
xmodmap -e "keysym Shift_R = Multi_key"

# Dotfiles linker
alias summon_linker="sed '/^#/d' ~/.config/.dotfiles/dotfiles.rdfm | sed -r '/^\s*$/d' | awk -F'=' '{ print \$1 }' | rlgl -qs rdfm link"

eval $(ssh-agent)
export GPG_TTY=$(tty)
export PATH="/home/wafelack/.cargo/bin:$PATH"
export PATH="/home/wafelack/opt/cross/bin:$PATH"
export PATH="/home/wafelack/.local/bin:$PATH"
export PATH="/usr/local/bin:$PATH"
export PATH="/sbin/:$PATH"
export PATH="/usr/local/textlive/2021/bin/x86_64-linux:$PATH"
export PATH="/home/wafelack/.ark:$PATH"
export ORION_LIB="/usr/lib/orion:$PATH"
export ARKSCRIPT_PATH="/home/wafelack/.ark"
export TERM=st-256color
export EDITOR=vim
PS1="(\j) \w \$ "
