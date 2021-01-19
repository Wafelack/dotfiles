# Path to your oh-my-zsh installation.
export ZSH="/home/wafelack/.oh-my-zsh"
ZSH_THEME="waffle"

# Uncomment the following line to change how often to auto-update (in days).
export UPDATE_ZSH_DAYS=7

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

ENABLE_CORRECTION="true"
COMPLETION_WAITING_DOTS="true"

source $ZSH/oh-my-zsh.sh

export DENO_INSTALL="/home/wafelack/.deno"

eval "$(ssh-agent -s)"
ssh-add # Used to avoid pass phrase on git push

# Aliases

alias rl="source ~/.zshrc"
alias py="python3"

# Path
export PATH="/home/wafelack/:$PATH"
export PATH="$DENO_INSTALL/bin:$PATH"
export PATH="/home/wafelack/.werb_bin:$PATH"

