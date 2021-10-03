alias py="python3"
alias fuckit="git reset --hard HEAD"
alias rl="source $HOME/.bashrc"
alias get_license="curl --silent www.gnu.org/licenses/gpl-3.0.txt -o COPYING"
alias ls="ls --color=always"

export GPG_TTY=$(tty)
export EDITOR=vim

export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="/usr/local/bin:$PATH"
export PATH="/usr/local/sbin:$PATH"
export PATH="/sbin/:$PATH"
export PATH="$HOME/scripts/:$PATH"

__stat() {
  STATUS=$?
  if [[ $STATUS -ne 0 ]]
  then
    echo -n "$STATUS "
  fi
}

start_ssh_agent() {
    if [[ -z ${SSH_AGENT_PID+x} ]]
    then
        eval $(ssh-agent)
    fi
}

start_ssh_agent
PS1="(\$(git branch 2> /dev/null | grep '^*' | colrm 1 2)) \$(__stat)\$(prompt_pwd.pl) % "
