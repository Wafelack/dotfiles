# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

export ARKSCRIPT_PATH=/home/wafelack/ark/



# If not running interactively, don't do anything
case $- in
  *i*) ;;
  *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
  debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
  xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
  if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
    # We have color support; assume it's compliant with Ecma-48
    # (ISO/IEC-6429). (Lack of such support is extremely rare, and such
    # a case would tend to support setf rather than setaf.)
    color_prompt=yes
  else
    color_prompt=
  fi
fi

if [ "$color_prompt" = yes ]; then
  PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
  PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
  xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
  *)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
  test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
  alias ls='ls --color=auto'
  #alias dir='dir --color=auto'
  #alias vdir='vdir --color=auto'
  alias rl='source ~/.bashrc'
  alias grep='grep --color=auto'
  alias fgrep='fgrep --color=auto'
  alias egrep='egrep --color=auto'
  alias py='python3'
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias eb='vim ~/.bashrc'
# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
  . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

if [[ "$git_status" =~ On\ branch\ ([^[:space:]]+) ]]; then
  branch=${BASH_REMATCH[1]}
else
  # detached head
  branch="(`git describe --all --contains --abbrev=4 HEAD 2> /dev/null || echo HEAD`)"
fi


export PS1='\033[0;37m[\t] \033[1;31m\u\033[1;30m in \033[1;32m\w\033[1;30m on\033[1;34m $(__git_ps1 '%s')\033[0m\r\n🧇 '

export PS1='\033[1;31m\u\033[0m at\033[1;33m \h\033[0m in\033[1;32m \w\033[0m on\033[0;36m $(__git_ps1 '%s') \033[0;37m[\033[1;33m$(git log --format="%h" -n 1 2> /dev/null)\033[0;37m] \033[0m \D{%H:%M}\r\n🧇 '

# Color codes

# BLACK="\033[0;30m"
# RED="\033[0;31m"
# GREEN="\033[0;32m"
# BROWN="\033[0;33m"
# BLUE="\033[0;34m"
# PURPLE="\033[0;35m"
# CYAN="\033[0;36m"
# LIGHT_GRAY="\033[0;37m"
# DARK_GRAY="\033[1;30m"
# LIGHT_RED="\033[1;31m"
# LIGHT_GREEN="\033[1;32m"
# YELLOW="\033[1;33m"
# LIGHT_BLUE="\033[1;34m"
# LIGHT_PURPLE="\033[1;35m"
# LIGHT_CYAN="\033[1;36m"
# LIGHT_WHITE="\033[1;37m"
# RESET="\033[0m"

# Install Ruby Gems to ~/gems
export GEM_HOME="$HOME/gems"
export PATH="$HOME/gems/bin:$PATH"
export PATH="/home/wafelack/.yarpm/bin:$PATH"
export DENO_INSTALL="/home/wafelack/.deno"
export PATH="$DENO_INSTALL/bin:$PATH"
export PATH=$(echo "$PATH" | sed 's|:/mnt/c/Program Files/nodejs/||') # removing windows annoying npm installation
export alias commit='git add . && gitmoji -c'
source "$HOME/.cargo/env"
sshd_status=$(service ssh status)
if [[ $sshd_status = *"is not running"* ]]; then
  sudo service ssh --full-restart
fi

eval "$(ssh-agent -s)"
ssh-add # Used to avoid pass phrase on git push
